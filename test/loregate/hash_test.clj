(ns loregate.hash-test
  (:require [clojure.test :refer [deftest is]]
            [loregate.hash :as h]))

(deftest canonical-ignores-construction-order
  (let [a {:b 1 :a #{3 1 2} [:rows :x] "r" :c [3 2 1]}
        b (-> (sorted-map-by #(compare (str %2) (str %1)))   ; different build order
              (assoc :c [3 2 1] [:rows :x] "r" :a (into #{} [2 3 1]) :b 1))]
    (is (= (h/canonical a) (h/canonical b)))
    (is (= (h/sha256 a) (h/sha256 b)))
    (is (re-matches #"sha256:[0-9a-f]{64}" (h/sha256 a)))))

(deftest canonical-keeps-what-matters
  (is (not= (h/sha256 [1 2]) (h/sha256 [2 1])) "vector order is meaning")
  (is (not= (h/sha256 {:a 1}) (h/sha256 {:a 1.0})))
  (is (not= (h/sha256 {:a "x"}) (h/sha256 {:a :x}))))
