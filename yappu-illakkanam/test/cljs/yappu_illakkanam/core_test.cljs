(ns cljs-reframe.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [cljs-reframe.core :as core]))

(deftest fake-test
  (testing "fake description"
    (is (= 1 2))))
