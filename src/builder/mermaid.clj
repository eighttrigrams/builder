(ns builder.mermaid
  (:require [builder.core :as core]
            [clojure.edn :as edn]
            [babashka.fs :as fs]))

(defn -main [& args]
  (let [[output-file] args
        config-file "project-builder.edn"]
    (when-not (fs/exists? config-file)
      (println "Error: project-builder.edn not found in current directory")
      (System/exit 1))
    (let [{:keys [pipeline-name]} (edn/read-string (slurp config-file))]
      (when-not pipeline-name
        (println "Error: project-builder.edn must contain :pipeline-name")
        (System/exit 1))
      (core/load-config! pipeline-name)
      (let [mermaid (core/generate-mermaid)]
        (if output-file
          (spit output-file mermaid)
          (println mermaid))))))
