(ns uml-viewer.csharp-language.graph-csharp-spec
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all]
            [uml-viewer.application.ir-generator :as generator]
            [uml-viewer.csharp-language.graph-csharp :as csharp]
            [uml-viewer.domain.hierarchy :as hierarchy]
            [uml-viewer.domain.ir :as ir]
            [uml-viewer.graph :as graph])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- spit-file [dir rel content]
  (let [file (io/file dir rel)]
    (io/make-parents file)
    (spit file content)
    file))

(describe "C# graph"
  (with root (-> (Files/createTempDirectory "uml-csharp-" (make-array FileAttribute 0)) .toFile))
  (after (doseq [file (reverse (file-seq @root))] (io/delete-file file true)))

  (it "translates extractor facts without changing C# casing"
    (let [result (csharp/read-graph
                   "{\"classes\":[{\"id\":\"Shop.Domain.Order\",\"ns\":\"Shop.Domain.Order\",\"name\":\"Order\",\"stereotype\":\"record\"}],\"edges\":[{\"from\":\"Shop.Domain.Order\",\"to\":\"Shop.Domain.Entity\",\"kind\":\"inheritance\"}]}"
                   {:prefix "Shop"})]
      (should= :Domain.Order (-> result :classes first :id))
      (should= :csharp (-> result :classes first :lang))
      (should= :record (-> result :classes first :stereotype))
      (should= [{:from :Domain.Order :to :Domain.Entity :kind :inheritance}] (:edges result))))

  (it "extracts types, partial members, aliases, and relationships while skipping build output"
    (spit-file @root "Types.cs"
               (str "namespace Shop.Domain {\n"
                    "  public abstract class Entity {}\n"
                    "  public interface IRepository<T> { T Load(int id); }\n"
                    "  public class Order : Entity {}\n"
                    "  public record Receipt(Order Order);\n"
                    "  public struct Position { public int X; }\n"
                    "  public enum Status { New, Done }\n"
                    "  public class Box {}\n"
                    "  public class Box<T> {}\n"
                    "}\n"))
    (spit-file @root "Service.cs"
               (str "using Alias = Shop.Domain.Order;\n"
                    "namespace Shop.Services;\n"
                    "public partial class OrderService : Shop.Domain.IRepository<Alias> {\n"
                    "  private Alias cached;\n"
                    "  public Alias Current { get; set; }\n"
                    "  public Alias Load(int id) { return new Alias(); }\n"
                    "  public Alias Load(string code) { return new Alias(); }\n"
                    "  private void Reset() {}\n"
                    "  public class Nested {}\n"
                    "}\n"))
    (spit-file @root "Other.cs"
               "namespace Shop.Services { public partial class OrderService { public void Save() {} } }")
    (spit-file @root "obj/Generated.cs" "namespace Shop { public class Hidden {} }")
    (spit-file @root "bin/Generated.cs" "namespace Shop { public class AlsoHidden {} }")
    (let [before (slurp (io/file @root "Service.cs"))
          result (graph/scan csharp/impl @root {:prefix "Shop"})
          classes (into {} (map (juxt :id identity) (:classes result)))
          edges (set (map (juxt :from :to :kind) (:edges result)))
          repository-id :Domain.IRepository$1
          box-id :Domain.Box$1
          service (classes :Services.OrderService)]
      (should= before (slurp (io/file @root "Service.cs")))
      (should= :abstract (:stereotype (classes :Domain.Entity)))
      (should= :interface (:stereotype (classes repository-id)))
      (should= :record (:stereotype (classes :Domain.Receipt)))
      (should= :struct (:stereotype (classes :Domain.Position)))
      (should= :enum (:stereotype (classes :Domain.Status)))
      (should (classes :Domain.Box))
      (should= "Box<T>" (:name (classes box-id)))
      (should (classes :Services.OrderService.Nested))
      (should-not (classes :Hidden))
      (should-not (classes :AlsoHidden))
      (should= "Shop.Services.OrderService" (:ns service))
      (should (str/ends-with? (:file service) ".cs"))
      (should= #{"cached" "Current"} (set (map :name (:fields service))))
      (should= 2 (count (filter #(= "Load" (:name %)) (:ops service))))
      (should (some #(= "Save" (:name %)) (:ops service)))
      (should (:private (first (filter #(= "Reset" (:name %)) (:ops service)))))
      (should (contains? edges [:Domain.Order :Domain.Entity :inheritance]))
      (should (contains? edges [:Services.OrderService repository-id :implements]))
      (should (contains? edges [:Services.OrderService :Domain.Order :dependency]))
      (let [path (spit-file @root "diagram.edn"
                            (generator/emit
                              (generator/document csharp/impl
                                                  {:src (str @root) :prefix "Shop" :hierarchical true})))
            doc (ir/load-document (str path))]
        (should (:hierarchical doc))
        (should (seq (:classes doc)))
        (let [view (hierarchy/view-at doc [:Domain])
              names (set (map :name (mapcat :classes (:packages view))))
              root-view (hierarchy/view-at doc [])
              children (mapcat :contents (mapcat :classes (:packages root-view)))]
          (should (contains? names "Box<T>"))
          (should (contains? (set (map :name children)) "IRepository<T>"))))))

  (it "resolves dependencies across projects in a solution"
    (spit-file @root "Domain/Domain.csproj"
               "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>")
    (spit-file @root "Domain/Order.cs" "namespace Shop.Domain; public class Order {}")
    (spit-file @root "App/App.csproj"
               (str "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup>"
                    "<ItemGroup><ProjectReference Include=\"../Domain/Domain.csproj\" /></ItemGroup></Project>"))
    (spit-file @root "App/Service.cs"
               "namespace Shop.App; public class Service { public Shop.Domain.Order Make() => new(); }")
    (spit-file @root "Shop.sln"
               (str "Microsoft Visual Studio Solution File, Format Version 12.00\n"
                    "Project(\"{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}\") = \"Domain\", \"Domain/Domain.csproj\", \"{C78668EF-A82D-45F1-BFC8-1312698B228E}\"\nEndProject\n"
                    "Project(\"{FAE04EC0-301F-11D3-BF4B-00C04F79EFBC}\") = \"App\", \"App/App.csproj\", \"{5D907EAE-C8B9-4DA2-A86B-F5C42597E72F}\"\nEndProject\n"
                    "Global\nEndGlobal\n"))
    (let [result (graph/scan csharp/impl (io/file @root "Shop.sln") {:prefix "Shop"})
          classes (into {} (map (juxt :id identity) (:classes result)))
          edges (set (map (juxt :from :to :kind) (:edges result)))]
      (should= #{:Domain.Order :App.Service} (set (map :id (remove :foreign (:classes result)))))
      (should-not (:foreign (classes :Domain.Order)))
      (should (contains? edges [:App.Service :Domain.Order :dependency]))))

  (it "loads a project and reports invalid roots"
    (spit-file @root "Demo.csproj"
               "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup><TargetFramework>net8.0</TargetFramework></PropertyGroup></Project>")
    (spit-file @root "Class.cs" "namespace Shop; public class Sample {}")
    (let [result (graph/scan csharp/impl (io/file @root "Demo.csproj") {:prefix "Shop"})]
      (should= [:Sample] (map :id (remove :foreign (:classes result)))))
    (should-throw clojure.lang.ExceptionInfo
                  (graph/scan csharp/impl (io/file @root "missing") {:prefix "Shop"}))))