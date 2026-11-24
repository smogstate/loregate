(ns loregate.schema.plan
  "The compiled plan (SPEC 7.1) and the closed set of compile errors (SPEC 7)."
  (:require [loregate.schema.query :as query]
            [loregate.schema.ontology :as ont]))

(def Hash [:re #"^sha256:[0-9a-f]{64}$"])

(def RuleId [:tuple [:enum :rows :field] qualified-keyword?])

(def Column
  [:map {:closed true}
   [:name [:or qualified-keyword? simple-keyword?]]
   [:type (into [:enum :set] ont/scalar-types)]
   [:of {:optional true} keyword?]
   [:unit {:optional true} qualified-keyword?]
   [:owned? boolean?]])

(def Source
  [:map
   [:kind keyword?]
   [:type qualified-keyword?]
   [:binding qualified-keyword?]
   [:fields [:map-of qualified-keyword? ont/Field]]
   [:call {:optional true} [:map [:args [:map-of string? any?]]]]
   [:pushed {:optional true} [:vector [:map {:closed true}
                                       [:attribute qualified-keyword?]
                                       [:op (into [:enum] ont/pushable-ops)]]]]
   [:path {:optional true} string?]
   [:row-cap {:optional true} pos-int?]])

(def Plan
  [:map {:closed true}
   [:plan/id Hash]
   [:plan/query query/Query]
   [:plan/sources [:map-of simple-keyword? Source]]
   [:plan/sql string?]
   [:plan/params [:map-of simple-keyword? query/ParamDef]]
   [:plan/user [:set simple-keyword?]]
   [:plan/requires [:map {:closed true}
                    [:fields [:set qualified-keyword?]]
                    [:rules [:set RuleId]]]]
   [:plan/output [:vector {:min 1} Column]]
   [:plan/pins [:map-of [:or qualified-keyword? RuleId] Hash]]
   [:plan/compiler pos-int?]
   [:plan/compiled-at inst?]])

(def compile-errors
  #{:compile/invalid-query
    :compile/unknown-name
    :compile/bad-path
    :compile/type-mismatch
    :compile/aggregate-in-where
    :compile/measure-not-on-from
    :compile/duplicate-column
    :compile/sort-not-selected
    :compile/cycle
    :compile/unknown-param
    :compile/limit-too-large
    :compile/no-binding
    :compile/no-row-rule
    :compile/invalid-binding})

(def CompileError
  [:map
   [:error (into [:enum] (sort compile-errors))]
   [:at {:optional true} vector?]])
