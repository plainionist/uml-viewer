(ns uml-viewer.adapters.core
  (:require [uml-viewer.adapters.sketch :as sketch]))

(def help-text
  (str "Usage: clj -M:run [options] [edn-file]\n"
       "\n"
       "  edn-file          Diagram to watch (default: examples/library.edn).\n"
       "                    A fresh start waits for the companion Grok to send\n"
       "                    :display unless the associated agent recycles\n"
       "                    the window with :uml-viewer-restart, or you press R.\n"
       "\n"
       "  --restart         Associated agent only (via :uml-viewer-restart).\n"
       "                    New JVM, keep the existing Grok tmux session.\n"
       "                    Reloads the last view (depth, pan, zoom, proposal).\n"
       "                    Do not use this if no companion is attached.\n"
       "\n"
      "  --read-only       Load immediately without a companion. Disable edits,\n"
      "                    source windows, and metric-refresh commands.\n"
      "\n"
       "  -h, --help        Print this help and exit.\n"))

(defn parse-args
  "EDN path and flags. `--restart` skips spawning a new agent."
  [args]
  (let [args (keep identity args)
        help? (boolean (some #{"--help" "-h"} args))
        restart? (boolean (some #{"--restart"} args))
        read-only? (boolean (some #{"--read-only"} args))
        path (->> args (remove #{"--help" "-h" "--restart" "--read-only"}) first)]
    (when (and restart? read-only?)
      (throw (ex-info "--restart and --read-only cannot be combined" {})))
    (cond-> {:help? help?
             :restart? restart?
             :path (or path "examples/library.edn")}
      read-only? (assoc :read-only? true))))

(defn start!
  "Launch the viewer. `source-impl` satisfies `LanguageSource`."
  [source-impl & args]
  (let [{:keys [path restart? help? read-only?]} (parse-args args)]
    (if help?
      (do (print help-text) :help)
      (do
        (if read-only?
          (sketch/start! path source-impl restart? true)
          (sketch/start! path source-impl restart?))
        (println "Watching" path)
        (println "Double-click a class for its card. Scroll to pan (Shift-scroll for horizontal). Ctrl+/− zoom; Ctrl+0 resets. R reloads. Click the real diagram above Proposals, or a proposal to show it.")))))
