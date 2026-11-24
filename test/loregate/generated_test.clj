(ns loregate.generated-test
  "Generated queries over the fixture ontology compile, validate, are deterministic and run."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [loregate.compile.core :as c]
            [loregate.gen :as gen]
            [loregate.schema.plan :as plan]
            [loregate.test-system :as ts]))

(deftest generated-queries-compile-and-run
  (let [sys (ts/system)
        result (tc/quick-check
                100
                (prop/for-all [q gen/query]
                  (let [p (c/compile-query (:ontology sys) q {:sources (:sources sys)})
                        rows (ts/run-query sys q)]
                    (and (m/validate plan/Plan p)
                         (= (:plan/id p) (:plan/id (c/compile-query (:ontology sys) q {:sources (:sources sys)})))
                         (vector? rows)
                         (or (nil? (:limit q)) (<= (count rows) (:limit q))))))
                :seed 7)]
    (is (:pass? result) (pr-str (get-in result [:shrunk :smallest])))))
