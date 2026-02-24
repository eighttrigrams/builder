(ns builder.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [builder.core :as core]))

(deftest requires-must-connect-to-earlier-produces
  (testing "valid: second stage requires what first stage produces"
    (is (= true
           (core/validate-pipeline
            {:stages [{:id :stage-1 :produces [:spec]}
                      {:id :stage-2 :requires [:spec]}]}))))

  (testing "invalid: second stage requires something not produced"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Required document not produced"
         (core/validate-pipeline
          {:stages [{:id :stage-1 :produces [:other]}
                    {:id :stage-2 :requires [:spec]}]})))))

(deftest cleanup-after-removes-from-available
  (testing "valid: require before cleanup-after"
    (is (= true
           (core/validate-pipeline
            {:stages [{:id :stage-1 :produces [:spec]}
                      {:id :stage-2 :requires [:spec] :cleanup-after [:spec]}
                      {:id :stage-3 :produces [:other]}]}))))

  (testing "invalid: require after cleanup-after"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Required document not produced"
         (core/validate-pipeline
          {:stages [{:id :stage-1 :produces [:spec]}
                    {:id :stage-2 :cleanup-after [:spec]}
                    {:id :stage-3 :requires [:spec]}]}))))

  (testing "valid: produce, cleanup, reproduce, then require"
    (is (= true
           (core/validate-pipeline
            {:stages [{:id :stage-1 :produces [:spec]}
                      {:id :stage-2 :cleanup-after [:spec]}
                      {:id :stage-3 :produces [:spec]}
                      {:id :stage-4 :requires [:spec]}]})))))

(deftest stage-cannot-produce-what-it-requires
  (testing "invalid: stage produces same artifact it requires"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Stage cannot produce the same artifact it requires"
         (core/validate-pipeline
          {:seed-artifacts [:spec]
           :stages [{:id :stage-1 :requires [:spec] :produces [:spec]}]}))))

  (testing "valid: stage requires one artifact and produces another"
    (is (= true
           (core/validate-pipeline
            {:seed-artifacts [:spec]
             :stages [{:id :stage-1 :requires [:spec] :produces [:enriched-spec]}]})))))
