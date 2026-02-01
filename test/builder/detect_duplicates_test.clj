(ns builder.detect-duplicates-test
  (:require [clojure.test :refer [deftest is testing]]
            [builder.detect-duplicates :as dd]))

(deftest defn-form?-test
  (testing "recognizes defn forms"
    (is (dd/defn-form? '(defn foo [x] x)))
    (is (dd/defn-form? '(defn bar [a b] (+ a b)))))
  (testing "rejects non-defn forms"
    (is (not (dd/defn-form? '(def foo 1))))
    (is (not (dd/defn-form? '(let [x 1] x))))
    (is (not (dd/defn-form? '[1 2 3])))))

(deftest extract-defn-info-test
  (testing "extracts name, params and body"
    (let [info (dd/extract-defn-info '(defn foo [x y] (+ x y)))]
      (is (= 'foo (:name info)))
      (is (= '[x y] (:params info)))
      (is (= '((+ x y)) (:body info)))))
  (testing "returns nil for non-defn"
    (is (nil? (dd/extract-defn-info '(def x 1))))))

(deftest single-call-body?-test
  (testing "true for single function call"
    (is (dd/single-call-body? {:body '((foo x y))})))
  (testing "false for multiple expressions"
    (is (not (dd/single-call-body? {:body '((foo x) (bar y))}))))
  (testing "false for non-list body"
    (is (not (dd/single-call-body? {:body '(x)})))))

(deftest extract-call-pattern-test
  (testing "extracts pattern for wrapper function"
    (let [defn-info {:params '[x y]
                     :body '((some-fn x y "literal"))}
          pattern (dd/extract-call-pattern defn-info)]
      (is (= 'some-fn (:called-fn pattern)))
      (is (= '[x y] (:forwarded-args pattern)))
      (is (= ["literal"] (:literal-args pattern)))
      (is (= 2 (:param-count pattern)))))
  (testing "returns nil for special forms"
    (let [defn-info {:params '[x] :body '((let [y 1] y))}]
      (is (nil? (dd/extract-call-pattern defn-info))))))

(deftest find-wrapper-duplicates-test
  (testing "finds functions wrapping same fn with different literals"
    (let [defns [{:name 'foo :params '[x] :body '((wrap x "a"))}
                 {:name 'bar :params '[x] :body '((wrap x "b"))}]
          groups (dd/find-wrapper-duplicates defns)]
      (is (= 1 (count groups)))
      (is (= 2 (count (second (first groups)))))))
  (testing "no duplicates when literals match"
    (let [defns [{:name 'foo :params '[x] :body '((wrap x "a"))}
                 {:name 'bar :params '[y] :body '((other y "a"))}]
          groups (dd/find-wrapper-duplicates defns)]
      (is (empty? groups)))))

(deftest normalize-form-test
  (testing "replaces strings and numbers with placeholders"
    (let [result (dd/normalize-form '(foo "hello" 42) #{})]
      (is (= '(SYM1 :STRING :NUMBER) result))))
  (testing "replaces params with :PARAM"
    (let [result (dd/normalize-form '(foo x y) #{'x 'y})]
      (is (= '(SYM1 :PARAM :PARAM) result))))
  (testing "preserves special forms"
    (let [result (dd/normalize-form '(let [x 1] x) #{})]
      (is (= 'let (first result))))))

(deftest find-structural-duplicates-test
  (testing "finds structurally identical functions"
    (let [defns [{:name 'foo :params '[a b] :body '((+ a b))}
                 {:name 'bar :params '[x y] :body '((+ x y))}]
          groups (dd/find-structural-duplicates defns)]
      (is (= 1 (count groups)))))
  (testing "ignores functions with fewer than 2 params"
    (let [defns [{:name 'foo :params '[x] :body '((inc x))}
                 {:name 'bar :params '[y] :body '((inc y))}]
          groups (dd/find-structural-duplicates defns)]
      (is (empty? groups)))))
