(ns loregate.schema.json
  "JSON Schema export of the contract schemas, for clients outside Clojure (SPEC 4.2).
  clojure -M:json-schema out/  writes one <name>.json per schema."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [malli.json-schema :as mjs]
            [loregate.schema.expr :as expr]
            [loregate.schema.plan :as plan]
            [loregate.schema.query :as query]
            [loregate.schema.result :as result]))

(def schemas
  {"query" query/Query "expression" expr/Expr "result" result/Result "run-error" result/RunError
   "compile-error" plan/CompileError "column" plan/Column "inputs" plan/Inputs})

(defn export
  "{name json-schema-map} for every contract schema."
  []
  (into (sorted-map)
        (for [[n s] schemas]
          [n (-> (walk/postwalk #(if (instance? java.util.regex.Pattern %) (str %) %) (mjs/transform s))
                 (assoc "$comment" (str "loregate " n " v" query/version)))])))

(defn -main [& [dir]]
  (doseq [[n s] (export)
          :let [f (io/file (or dir "json-schema") (str n ".json"))]]
    (io/make-parents f)
    (spit f (json/write-str s :indent true))
    (println "wrote" (str f))))
