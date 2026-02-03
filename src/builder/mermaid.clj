(ns builder.mermaid
  (:require [builder.core :as core]))

(defn -main [& args]
  (let [[pipeline-name output-file] args]
    (when-not pipeline-name
      (println "Usage: builder-mermaid <pipeline-name> [output-file]")
      (System/exit 1))
    (core/load-config! pipeline-name)
    (let [mermaid (core/generate-mermaid)]
      (if output-file
        (spit output-file mermaid)
        (println mermaid)))))
