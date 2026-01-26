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
