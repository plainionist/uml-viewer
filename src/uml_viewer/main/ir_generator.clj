(ns uml-viewer.main.ir-generator
  (:require [uml-viewer.application.ir-generator :as ir-generator]
            [uml-viewer.clojure-language.graph-clojure]
            [uml-viewer.csharp-language.graph-csharp]
            [uml-viewer.graph :as graph]
            [uml-viewer.python-language.graph-python]
            [uml-viewer.rust-language.graph-rust]
            [uml-viewer.typescript-language.graph-typescript])
  (:gen-class))

(defn -main [& args]
  (let [policy-path (or (first args) "examples/uml-viewer.policy.edn")
        out (second args)
        policy (ir-generator/read-policy policy-path)
        lang (or (:lang policy) :clojure)
        impl (or (graph/lookup lang)
                 (throw (ex-info (str "no LanguageGraph for " lang) {:lang lang})))]
    (println "Wrote" (ir-generator/generate impl policy-path out))))
