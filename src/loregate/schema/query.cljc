(ns loregate.schema.query
  "The query DSL (SPEC 6) as a malli schema. Structure only; names, types, grouping and
  the configured :limit maximum are checked at compile."
  (:require [loregate.schema.expr :as expr]))

(def version 1)

(def ParamType [:enum :string :int :decimal :boolean :date :datetime])

(def ParamDef
  [:map {:closed true}
   [:type ParamType]
   [:default {:optional true} expr/Scalar]
   [:min {:optional true} number?]
   [:max {:optional true} number?]
   [:options {:optional true} [:vector {:min 1} expr/Scalar]]])

(def Query
  [:map {:closed true}
   [:from expr/Ref]
   [:select [:vector {:min 1} expr/Selectable]]
   [:let {:optional true} [:map-of expr/Local expr/Expr]]
   [:where {:optional true} expr/Expr]
   [:having {:optional true} expr/Expr]
   [:sort {:optional true} [:vector {:min 1} [:tuple expr/Selectable [:enum :asc :desc]]]]
   [:limit {:optional true} pos-int?]
   [:params {:optional true} [:map-of simple-keyword? ParamDef]]])
