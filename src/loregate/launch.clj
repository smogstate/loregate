(ns loregate.launch
  "The jar's entry point. Only this namespace is compiled ahead of time; everything else
  loads from source at start, because ahead-of-time compiling malli breaks its registry."
  (:gen-class))

(defn -main [& args]
  (apply (requiring-resolve 'loregate.main/-main) args))
