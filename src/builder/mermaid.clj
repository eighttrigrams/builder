(ns builder.mermaid
  (:require [builder.core :as core]))

(defn -main [& args]
  (when (empty? args)
    (println "Usage: builder-mermaid <pipeline-name> [output-file]")
    (System/exit 1))
  (let [[pipeline-name output-file] args]
    (core/load-config! pipeline-name)
    (let [mermaid (core/generate-mermaid)]
      (if output-file
        (spit output-file mermaid)
        (println mermaid)))))
