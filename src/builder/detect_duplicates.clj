(ns builder.detect-duplicates
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk]))

(def special-forms
  #{'let 'when 'if 'cond 'do 'fn 'loop 'try 'catch 'throw
    'def 'defn 'defn- 'defmacro 'ns 'require 'import
    '-> '->> 'some-> 'some->> 'as-> 'cond-> 'cond->>
    'and 'or 'not 'if-let 'when-let 'if-some 'when-some
    'for 'doseq 'dotimes 'while 'case 'condp})

(defn read-forms [file]
  (let [content (slurp file)]
    (try
      (read-string (str "[" content "]"))
      (catch Exception _ []))))

(defn defn-form? [form]
  (and (list? form) (= 'defn (first form))))

(defn extract-defn-info [form]
  (when (defn-form? form)
    (let [[_ name params & body] form]
      {:name name
       :params params
       :body body
       :form form})))

(defn single-call-body? [{:keys [body]}]
  (and (= 1 (count body))
       (list? (first body))))

(defn extract-call-pattern [{:keys [params body] :as defn-info}]
  (when (single-call-body? defn-info)
    (let [call (first body)
          [called-fn & args] call]
      (when-not (special-forms called-fn)
        (let [param-set (set params)
              literal-args (remove #(or (param-set %) (symbol? %)) args)
              forwarded-args (filter param-set args)]
          {:called-fn called-fn
           :forwarded-args (vec forwarded-args)
           :literal-args (vec literal-args)
           :param-count (count params)})))))

(defn find-wrapper-duplicates [defns]
  (->> defns
       (map (fn [d] (assoc d :pattern (extract-call-pattern d))))
       (filter :pattern)
       (filter #(seq (:literal-args (:pattern %))))
       (group-by (fn [{:keys [pattern]}]
                   [(:called-fn pattern)
                    (:forwarded-args pattern)
                    (:param-count pattern)]))
       (filter (fn [[_ group]] (> (count group) 1)))))

(defn normalize-form [form params]
  (let [param-set (set params)
        sym-counter (atom 0)
        sym-map (atom {})]
    (walk/postwalk
      (fn [x]
        (cond
          (string? x) :STRING
          (number? x) :NUMBER
          (keyword? x) x
          (and (symbol? x) (param-set x)) :PARAM
          (symbol? x)
          (if (special-forms x)
            x
            (if-let [mapped (@sym-map x)]
              mapped
              (let [new-sym (symbol (str "SYM" (swap! sym-counter inc)))]
                (swap! sym-map assoc x new-sym)
                new-sym)))
          :else x))
      form)))

(defn extract-structure [{:keys [params body]}]
  (normalize-form body params))

(defn find-structural-duplicates [defns]
  (->> defns
       (filter #(>= (count (:params %)) 2))
       (map (fn [d] (assoc d :structure (extract-structure d))))
       (group-by (fn [{:keys [params structure]}]
                   [(count params) structure]))
       (filter (fn [[_ group]] (> (count group) 1)))
       (filter (fn [[_ group]]
                 (let [names (map :name group)]
                   (not (apply = names)))))))

(defn report-wrapper-duplicates [file groups]
  (doseq [[pattern group] groups]
    (let [[called-fn forwarded-args _] pattern]
      (println (str "\n" file " [wrapper]:"))
      (println (str "  Functions wrapping '" called-fn "' with params " forwarded-args ":"))
      (doseq [{:keys [name pattern]} group]
        (println (str "    - " name " (literal: " (:literal-args pattern) ")"))))))

(defn report-structural-duplicates [file groups]
  (doseq [[_ group] groups]
    (println (str "\n" file " [structural]:"))
    (println "  Functions with identical structure:")
    (doseq [{:keys [name]} group]
      (println (str "    - " name)))))

(defn analyze-file [file]
  (let [forms (read-forms file)
        defns (->> forms
                   (filter defn-form?)
                   (map extract-defn-info)
                   (filter identity))
        wrapper-groups (find-wrapper-duplicates defns)
        structural-groups (find-structural-duplicates defns)]
    (when (seq wrapper-groups)
      (report-wrapper-duplicates file wrapper-groups))
    (when (seq structural-groups)
      (report-structural-duplicates file structural-groups))))

(defn find-clj-files [dir]
  (->> (file-seq (io/file dir))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".clj"))))

(defn analyze-directory [dir]
  (doseq [file (find-clj-files dir)]
    (analyze-file (str file))))

(defn -main [& args]
  (let [arg (first args)
        src-dir (io/file "src")]
    (cond
      (and arg (.isFile (io/file arg))) (analyze-file arg)
      (and arg (.isDirectory (io/file arg))) (analyze-directory arg)
      (.isDirectory src-dir) (analyze-directory "src")
      :else (println "Usage: detect-duplicates [file.clj|directory]"))))
