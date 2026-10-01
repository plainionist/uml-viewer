(ns uml-viewer.csharp-language.graph-csharp
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [uml-viewer.graph :as graph]))

(defn read-graph
  [text {:keys [prefix]}]
  (let [raw (json/read-str text :key-fn keyword)]
    {:classes (mapv (fn [class]
                      (cond-> (assoc class
                                     :id (graph/id-of (:id class) prefix)
                                     :lang :csharp)
                        (:stereotype class) (update :stereotype keyword)
                        (:file class) (update :file graph/relative-path)))
                    (:classes raw))
     :edges (mapv (fn [edge]
                    (-> edge
                        (update :from graph/id-of prefix)
                        (update :to graph/id-of prefix)
                        (update :kind keyword)))
                  (:edges raw))}))

(defn- extractor-project []
  (let [repo (-> (io/resource "uml_viewer/csharp_language/graph_csharp.clj")
                 io/file .getParentFile .getParentFile .getParentFile .getParentFile)]
    (io/file repo "tools" "csharp" "UmlViewer.CSharp.csproj")))

(defn- checked-command [& args]
  (let [{:keys [exit out err]} (apply shell/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str "C# extractor failed: " err out)
                      {:command (vec args) :exit exit})))
    (when (seq err)
      (binding [*out* *err*] (print err)))
    out))

(def impl
  (reify graph/LanguageGraph
    (scan [_ root opts]
      (let [project (extractor-project)
            dll (io/file (.getParentFile project) "bin" "Release" "net8.0" "UmlViewer.CSharp.dll")]
        (checked-command "dotnet" "build" (str project)
                         "--configuration" "Release" "--nologo" "--verbosity" "quiet")
        (read-graph (checked-command "dotnet" (str dll) (.getCanonicalPath (io/file root))) opts)))))

(graph/register! :csharp impl)