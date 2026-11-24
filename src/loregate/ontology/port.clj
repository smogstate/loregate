(ns loregate.ontology.port
  "Port to the ontology (SPEC 5.6). Loregate only reads through it.")

(defprotocol Ontology
  (types   [o]      "[{:type/id … :type/title …}], sorted by id.")
  (type-of [o id]   "The type declaration with :type/links (outgoing), :type/defs, :type/bindings,
                     :type/rules (its row rule and the field rules of its attributes) and
                     :type/commands; nil when unknown.")
  (search  [o text] "Candidate types, attributes and measures for a text:
                     [{:kind :type|:attribute|:measure :id … :on type-id}].")
  (hash-of [o id]   "\"sha256:…\" of one declaration: a type, link, def or binding id, or a
                     rule id [kind on]; nil when unknown."))
