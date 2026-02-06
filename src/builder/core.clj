(ns builder.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.process :refer [shell process]]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]))

(def ^:dynamic *config* nil)
(def total-cost (atom 0.0))
(def total-duration-ms (atom 0))

(defn validate-pipeline [config]
  (let [produced (atom (set (:seed-artifacts config)))]
    (doseq [{:keys [requires produces cleanup-after]} (:stages config)]
      (when requires
        (doseq [req requires]
          (when-not (contains? @produced req)
            (throw (ex-info "Required document not produced by earlier stage"
                            {:required req :produced @produced})))))
      (when produces
        (swap! produced into produces))
      (when cleanup-after
        (swap! produced #(apply disj % cleanup-after))))
    true))

(defn load-config! [pipeline-name]
  (let [resource-name (str "pipelines/" pipeline-name ".pipeline.edn")]
    (if-let [resource (io/resource resource-name)]
      (let [config (edn/read-string (slurp resource))]
        (validate-pipeline config)
        (alter-var-root #'*config* (constantly config)))
      (do
        (println "Error: Unknown pipeline:" pipeline-name)
        (System/exit 1)))))

(defn docs-dir [] (:docs-dir *config*))
(defn documents [] (:documents *config*))

(defn doc-path [doc-key]
  (str (docs-dir) "/" (:file (get (documents) doc-key))))

(defn interpolate [template ctx]
  (reduce-kv
   (fn [s k v]
     (str/replace s (str "{{" (name k) "}}") v))
   template
   (merge ctx
          {:docs-dir (docs-dir)}
          (into {} (map (fn [[k _]] [k (doc-path k)]) (documents))))))

(defn file-valid? [path]
  (and (fs/exists? path)
       (not (str/blank? (slurp path)))))

(defn check-requires [{:keys [requires]}]
  (when requires
    (doseq [doc-key requires]
      (let [path (doc-path doc-key)]
        (when-not (file-valid? path)
          (println "Error: Required document" path "not found or empty.")
          (System/exit 1))))))

(defn check-produces [{:keys [produces]}]
  (when produces
    (doseq [doc-key produces]
      (let [path (doc-path doc-key)]
        (when-not (file-valid? path)
          (println "Error: Expected output" path "not found or empty.")
          (System/exit 1))))))

(defn strip-frontmatter [content]
  (if (str/starts-with? content "---")
    (let [end-idx (str/index-of content "---" 3)]
      (if end-idx
        (str/trim (subs content (+ end-idx 3)))
        content))
    content))

(defn run-tests []
  (println "Running tests...")
  (let [{:keys [exit]} (shell {:continue true} "make" "test")]
    (when (not= 0 exit)
      (println "Unit tests failed. Aborting.")
      (System/exit 1))))

(defn cleanup-docs [doc-keys]
  (doseq [doc-key doc-keys]
    (let [path (doc-path doc-key)]
      (when (fs/exists? path)
        (fs/delete path)))))

(defn log-file [] (:log-file *config*))

(defn timestamp []
  (let [fmt (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (.format fmt (java.util.Date.))))

(defn log-to-file [msg]
  (when-let [f (log-file)]
    (spit f (str (timestamp) " - " msg "\n") :append true)))

(defn builder-root []
  (-> (io/resource "pipelines")
      .toURI
      java.io.File.
      .getParentFile
      .getParentFile
      .getAbsolutePath))

(defn load-skill [skill-path]
  (when skill-path
    (let [full-path (str (builder-root) "/" skill-path)]
      (if (fs/exists? full-path)
        (strip-frontmatter (slurp full-path))
        (throw (ex-info "Skill file not found" {:path full-path}))))))

(defn build-prompt [{:keys [requires prompt skill]} ctx]
  (when prompt
    (let [file-refs (when (seq requires)
                      (str (str/join " " (map #(str "@" (doc-path %)) requires))
                           "\n\n"))
          skill-content (when skill
                          (str (load-skill skill) "\n\n"))]
      (interpolate (str skill-content file-refs prompt) ctx))))

(defn run-claude-json [stage-id prompt model produces allowed-tools]
  (println "Running Claude (JSON mode) with model:" model)
  (let [plugin-dir (builder-root)
        stage-name (name stage-id)
        output-keys (map name produces)
        json-schema (json/generate-string
                     {:type "object"
                      :properties (into {} (map (fn [k] [k {:type "string"}]) output-keys))
                      :required (vec output-keys)})
        base-args ["claude" "-p" prompt
                   "--output-format" "json"
                   "--json-schema" json-schema
                   "--plugin-dir" plugin-dir
                   "--model" model]
        args (if allowed-tools
               (into base-args ["--allowedTools" allowed-tools])
               base-args)]
    #_(log-to-file (str "### [" stage-name "] Plugin dir: " plugin-dir))
    (log-to-file (str "### [" stage-name "] Model: " model))
    (log-to-file (str "### [" stage-name "] Allowed tools: " (or allowed-tools "none")))
    (log-to-file (str "### [" stage-name "] JSON schema: " json-schema))
    (log-to-file (str "### [" stage-name "] Prompt:"))
    (log-to-file prompt)
    (log-to-file (str "### [" stage-name "] End of prompt\n"))
    (let [result (apply babashka.process/shell {:out :string} args)
          parsed (json/parse-string (:out result) true)
          content-json (:structured_output parsed)
          cost (or (:total_cost_usd parsed) 0)
          duration (or (:duration_ms parsed) 0)]
      (when (pos? cost)
        (swap! total-cost + cost))
      (when (pos? duration)
        (swap! total-duration-ms + duration))
      (let [cost-line (str "Stage cost: $" (format "%.4f" cost) " | Duration: " (format "%.1f" (/ duration 1000.0)) "s")]
        (println cost-line)
        (log-to-file (str "### [" stage-name "] " cost-line)))
      (log-to-file (str "### [" stage-name "] Response:\n" (yaml/generate-string parsed :dumper-options {:flow-style :block})))
      (if content-json
        (doseq [doc-key produces]
          (let [k (keyword (name doc-key))
                content (get content-json k)
                path (doc-path doc-key)]
            (if content
              (do
                (println "Writing" path)
                (log-to-file (str "### JSON output: writing key '" (name k) "' to " path))
                (spit path content))
              (log-to-file (str "### JSON output: key '" (name k) "' not found in response")))))
        (do
          (println "Error: No structured_output in Claude response")
          (log-to-file "### JSON output: ERROR - No structured_output in Claude response"))))))

(defn start-app []
  (println "Starting app...")
  (shell "make" "stop")
  (process "make" "start")
  (Thread/sleep 2000))

(defn stop-app []
  (println "Stopping app...")
  (shell "make" "stop"))

(defn create-human-opinion []
  (let [path (doc-path :human-opinion)]
    (spit path "")
    (shell "code" path)))

(defn send-notification [stage-id message project-name]
  (let [send-msg "scripts/send-message.sh"
        title (str "Stage: " (name stage-id))]
    (when (fs/exists? send-msg)
      (shell {:continue true} send-msg title message project-name))))

(defn wait-for-human [message {:keys [id produces]} project-name]
  (shell "say" (str project-name " needs your attention now."))
  (send-notification id message project-name)
  (when (some #{:human-opinion} produces)
    (create-human-opinion))
  (println message)
  (loop []
    (print "Type 'ok' to proceed: ")
    (flush)
    (if (= "ok" (str/trim (read-line)))
      (let [missing (filter #(not (file-valid? (doc-path %))) produces)]
        (if (seq missing)
          (do
            (println "Missing/empty:" (str/join ", " (map #(doc-path %) missing)))
            (recur))
          :done))
      (recur))))

(defn has-changes-to-commit? []
  (let [result (babashka.process/shell {:out :string} "git" "status" "--porcelain")]
    (not (str/blank? (:out result)))))

(defn git-commit [message commit-message-prefix]
  (shell "git" "add" ".")
  (shell "git" "reset" "HEAD" "--" (str (docs-dir) "/"))
  (if (has-changes-to-commit?)
    (shell "git" "commit" "-m" (str commit-message-prefix " - " message))
    (println "No changes to commit, skipping.")))

(defn git-amend-commit [commit-message-prefix]
  (let [body (when (fs/exists? (doc-path :commit-message-body))
               (slurp (doc-path :commit-message-body)))]
    (shell "git" "commit" "--amend" "-m"
           (str commit-message-prefix " - Implementation")
           "-m" (or body ""))))

(defn git-clear-next-feature []
  (spit (doc-path :next-feature) "")
  (shell "git" "add" ".")
  (shell "git" "commit" "--amend" "--no-edit"))

(defn run-shell-stage [{:keys [shell shell-output]} ctx]
  (when (and shell shell-output)
    (let [cmd (interpolate shell ctx)
          result (babashka.process/shell {:out :string :continue true} "bash" "-c" cmd)]
      (spit (doc-path shell-output) (:out result)))))

(defn run-stage [stage ctx]
  (let [{:keys [id prompt human-input? start-app? stop-app? run-tests?
                commit cleanup cleanup-after git-revert? git-restore? amend-commit?
                clear-next-feature? message]} stage
        {:keys [commit-message-prefix project-name]} ctx]
    (println "\n=== Stage:" (name id) "===")

    (check-requires stage)

    (when cleanup
      (cleanup-docs cleanup))

    (when git-revert?
      (shell "git" "revert" "--no-edit" "HEAD"))

    (when git-restore?
      (shell "git" "restore" "."))

    (run-shell-stage stage ctx)

    (when start-app?
      (start-app))

    (when human-input?
      (wait-for-human (interpolate (or message "Waiting for human input...") ctx) stage project-name))

    (when stop-app?
      (stop-app))

    (when prompt
      (let [model (or (:model stage) (:model ctx))]
        (run-claude-json id (build-prompt stage ctx) model (:produces stage) (:allowed-tools stage))))

    (check-produces stage)

    (when run-tests?
      (run-tests))

    (when commit
      (git-commit (:message commit) commit-message-prefix))

    (when amend-commit?
      (git-amend-commit commit-message-prefix))

    (when cleanup-after
      (cleanup-docs cleanup-after))

    (when clear-next-feature?
      (git-clear-next-feature))))

(defn run-pipeline [ctx]
  (reset! total-cost 0.0)
  (reset! total-duration-ms 0)
  (doseq [stage (:stages *config*)]
    (run-stage stage ctx))
  (let [total-secs (/ @total-duration-ms 1000.0)
        mins (int (/ total-secs 60))
        secs (int (mod total-secs 60))]
    (println (str "\n=== Total Cost: $" (format "%.4f" @total-cost) " | Total Duration: " mins "m " secs "s ==="))))

(defn stage-id->node [id]
  (-> (name id)
      (str/replace #"-" "_")
      str/upper-case))

(defn doc-id->node [id]
  (-> (name id)
      (str/replace #"-" "_")))

(defn commit-artifact-id [stage-id]
  (keyword (str (name stage-id) "-commit")))

(defn generate-mermaid []
  (let [stages (:stages *config*)
        sb (StringBuilder.)
        commit-artifacts (atom #{})
        pseudo-artifacts (atom #{})
        last-commit (atom nil)
        past-revert? (atom false)
        post-revert-commits (atom [])
        finalize-stage (atom nil)]
    (.append sb "flowchart TB\n")
    (doseq [{:keys [id requires produces commit git-revert? requires-commit? amend-commit?
                    produces-pseudo requires-pseudo requires-commits]} stages]
      (let [stage-node (stage-id->node id)]
        (when amend-commit?
          (reset! finalize-stage stage-node))
        (when (seq requires)
          (doseq [req requires]
            (.append sb (format "    %s --> %s\n" (doc-id->node req) stage-node))))
        (when (seq requires-pseudo)
          (doseq [req requires-pseudo]
            (.append sb (format "    %s --> %s\n" (doc-id->node req) stage-node))))
        (when (and requires-commit? @last-commit)
          (.append sb (format "    %s --> %s\n" (doc-id->node @last-commit) stage-node)))
        (when (seq requires-commits)
          (doseq [c requires-commits]
            (.append sb (format "    %s --> %s\n" (doc-id->node (commit-artifact-id c)) stage-node))))
        (when (seq produces)
          (doseq [prod produces]
            (.append sb (format "    %s --> %s\n" stage-node (doc-id->node prod)))))
        (when (seq produces-pseudo)
          (doseq [prod produces-pseudo]
            (swap! pseudo-artifacts conj prod)
            (.append sb (format "    %s --> %s\n" stage-node (doc-id->node prod)))))
        (when (or commit git-revert?)
          (let [commit-id (commit-artifact-id id)]
            (swap! commit-artifacts conj commit-id)
            (reset! last-commit commit-id)
            (when (and @past-revert? commit)
              (swap! post-revert-commits conj commit-id))
            (.append sb (format "    %s --> %s\n" stage-node (doc-id->node commit-id)))))
        (when git-revert?
          (reset! past-revert? true))))
    (when @finalize-stage
      (doseq [c @post-revert-commits]
        (.append sb (format "    %s --> %s\n" (doc-id->node c) @finalize-stage))))
    (.append sb "\n")
    (let [all-stages (map :id stages)
          all-docs (->> stages
                        (mapcat (juxt :requires :produces))
                        flatten
                        (remove nil?)
                        distinct)]
      (doseq [s all-stages]
        (.append sb (format "    style %s fill:#455a64,color:#fff\n" (stage-id->node s))))
      (.append sb "\n")
      (doseq [d all-docs]
        (.append sb (format "    style %s fill:#fff,stroke:#455a64,color:#000\n" (doc-id->node d))))
      (doseq [c @commit-artifacts]
        (.append sb (format "    style %s fill:#fff,stroke:#455a64,color:#000\n" (doc-id->node c))))
      (doseq [p @pseudo-artifacts]
        (.append sb (format "    style %s fill:#fff,stroke:#455a64,color:#000\n" (doc-id->node p)))))
    (.toString sb)))

(defn check-makefile-targets []
  (when-not (fs/exists? "Makefile")
    (println "Error: Makefile not found in current directory")
    (System/exit 1))
  (let [content (slurp "Makefile")
        has-target? #(re-find (re-pattern (str "(?m)^" % ":")) content)]
    (doseq [target ["start" "stop" "test"]]
      (when-not (has-target? target)
        (println "Error: Makefile missing required target:" target)
        (System/exit 1)))))

(def valid-models #{"haiku" "sonnet" "opus"})

(defn load-project-config []
  (let [config-file "project-builder.edn"]
    (when-not (fs/exists? config-file)
      (println "Error: project-builder.edn not found in current directory")
      (System/exit 1))
    (let [config (edn/read-string (slurp config-file))
          model (:model config)]
      (when-not model
        (println "Error: project-builder.edn must contain :model (haiku, sonnet, or opus)")
        (System/exit 1))
      (when-not (valid-models model)
        (println "Error: :model must be one of: haiku, sonnet, opus")
        (System/exit 1))
      config)))

(defn -main [& args]
  (let [[feature-name] args
        {:keys [pipeline-name port project-name model]} (load-project-config)]
    (when-not feature-name
      (println "Usage: builder <feature-name>")
      (System/exit 1))
    (when-not (and pipeline-name project-name)
      (println "Error: project-builder.edn must contain :pipeline-name and :project-name")
      (System/exit 1))
    (load-config! pipeline-name)
    (when (:standard-fullstack? *config*)
      (when-not port
        (println "Error: This pipeline requires :port in project-builder.edn")
        (System/exit 1))
      (check-makefile-targets))
    (run-pipeline {:commit-message-prefix (str "feature/" feature-name)
                   :feature-name feature-name
                   :port port
                   :project-name project-name
                   :model model})))
