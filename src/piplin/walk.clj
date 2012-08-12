(ns piplin.walk
  (:refer-clojure :exclude [compile cast])
  (:use [piplin types])) 

(defn walk
  "Takes an expr, a function to reduce over with a depth
  first postwalk, and an initial value and reduces the expr."
  [expr f init]
  )

(defn merged-args
  [ast]
  (apply merge (map (value ast) [:args :consts])))

(defn compile
  "Takes an expr, a function that takes an expr and a map
  of exprs to names and returns the form for the expr and
  its generated name, and it returns a list of all the
  forms in order.
  
  You can provide pre-initialized name-lookup or form list."
  [expr expr->name+form name-table body]
  (letfn [(render-expr [expr name-table body]
            (if (contains? name-table expr)
              [name-table body]
              (let [[name form]
                    (expr->name+form expr name-table)]
                (if name
                  [(assoc name-table
                          expr
                          name)
                   (conj body form)]
                  [name-table body]))))]
    (if (pipinst? expr)
      (render-expr expr name-table body)
      (let [args (vals (merged-args expr))
            [name-table body]
            (if (seq args)
              (reduce
                (fn [[name-table body] expr]
                  (compile expr
                           expr->name+form
                           name-table
                           body))
                [name-table body]
                args)
              [name-table body])]
        (render-expr expr name-table body)))))
