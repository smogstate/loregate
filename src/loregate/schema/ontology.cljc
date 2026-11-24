(ns loregate.schema.ontology
  "Ontology declarations (SPEC 5) as malli schemas. An ontology is a flat vector of
  declarations; the kind of each is told by its identifying key."
  (:require [loregate.schema.expr :as expr]))

(def scalar-types [:string :int :decimal :boolean :date :datetime :enum])

(def Attribute
  [:multi {:dispatch :type}
   [:enum [:map {:closed true}
           [:type [:= :enum]]
           [:values [:set {:min 1} qualified-keyword?]]
           [:nullable? {:optional true} boolean?]]]
   [:decimal [:map {:closed true}
              [:type [:= :decimal]]
              [:scale {:optional true} [:int {:min 0 :max 38}]]
              [:unit {:optional true} qualified-keyword?]
              [:nullable? {:optional true} boolean?]]]
   [:set [:map {:closed true}
          [:type [:= :set]]
          [:of (into [:enum] (remove #{:enum} scalar-types))]
          [:nullable? {:optional true} boolean?]]]
   [:malli.core/default [:map {:closed true}
                 [:type (into [:enum] (remove #{:enum :decimal} scalar-types))]
                 [:unit {:optional true} qualified-keyword?]
                 [:nullable? {:optional true} boolean?]]]])

(def Type
  [:map {:closed true}
   [:type/id qualified-keyword?]
   [:type/title string?]
   [:type/owner string?]
   [:type/definition {:optional true} string?]
   [:type/key [:vector {:min 1} qualified-keyword?]]
   [:type/attributes [:map-of {:min 1} qualified-keyword? Attribute]]])

(def Link
  [:map {:closed true}
   [:link/id qualified-keyword?]
   [:link/from qualified-keyword?]
   [:link/to qualified-keyword?]
   [:link/on [:vector {:min 1} [:tuple qualified-keyword? qualified-keyword?]]]
   [:link/cardinality [:enum :many-to-one :one-to-one :one-to-many]]])

(def Def
  [:map {:closed true}
   [:def/id qualified-keyword?]
   [:def/on qualified-keyword?]
   [:def/kind [:enum :attribute :measure]]
   [:def/owner string?]
   [:def/type (into [:enum] (remove #{:enum} scalar-types))]
   [:def/unit {:optional true} qualified-keyword?]
   [:def/expr expr/DefExpr]
   [:def/where {:optional true} expr/DefExpr]])

(def pushable-ops [:= :in :< :<= :> :>= :between])

(def Field
  "A column name, or [column json-path] for a value inside a JSON column (SPEC 5.3)."
  [:or string? [:tuple string? [:re #"^\$"]]])

;; Open map: keys outside :binding/* (:service/*, :parquet/*) belong to the source adapter,
;; which checks them (SPEC 4.2, 5.3).
(def Binding
  [:map
   [:binding/id qualified-keyword?]
   [:binding/type qualified-keyword?]
   [:binding/kind keyword?]
   [:binding/fields [:map-of {:min 1} qualified-keyword? Field]]
   [:binding/values {:optional true}
    [:map-of qualified-keyword? [:map-of {:min 1} qualified-keyword? string?]]]
   [:binding/accepts {:optional true}
    [:map-of qualified-keyword?
     [:map {:closed true}
      [:arg string?]
      [:ops [:set {:min 1} (into [:enum] pushable-ops)]]]]]
   [:binding/required {:optional true} [:set qualified-keyword?]]
   [:binding/row-cap {:optional true} pos-int?]
   [:binding/priority {:optional true} int?]
   [:binding/rows-enforced-by [:enum :layer :service]]])

(def Rule
  [:multi {:dispatch :rule/kind}
   [:rows [:map {:closed true}
           [:rule/on qualified-keyword?]
           [:rule/kind [:= :rows]]
           [:rule/allow expr/RuleExpr]]]
   [:field [:map {:closed true}
            [:rule/on qualified-keyword?]
            [:rule/kind [:= :field]]
            [:rule/visible expr/RuleExpr]]]])

(def Command
  [:map {:closed true}
   [:command/id qualified-keyword?]
   [:command/on qualified-keyword?]
   [:command/params any?]                 ; a malli schema form, passed to clients as is
   [:command/service string?]
   [:command/endpoint string?]])

(defn kind
  "Which declaration a map is, by its identifying key."
  [decl]
  (some (fn [[k kind]] (when (contains? decl k) kind))
        [[:type/id :type] [:link/id :link] [:def/id :def]
         [:binding/id :binding] [:rule/kind :rule] [:command/id :command]]))

(def Declaration
  [:multi {:dispatch kind}
   [:type Type] [:link Link] [:def Def] [:binding Binding] [:rule Rule] [:command Command]])

(def Ontology [:vector Declaration])

(defn rule-id
  "Rules have no id of their own; they are named by [kind on], as in plan pins."
  [rule]
  [(:rule/kind rule) (:rule/on rule)])
