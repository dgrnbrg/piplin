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
  of exprs to names and returns the code for the expr and
  the updated name table, and it returns a list of all
  the code in order.
  
  You can provides pre-initialized name-lookup or code list."
  [expr f name-table body]
  (letfn [(render-expr [expr name-table body]
            (let [[name-table partial]
                  (f expr name-table)]
              [name-table (conj body partial)]))]
    (if (pipinst? expr)
      (render-expr expr name-table body)
      (let [args (vals (merged-args expr))
            [name-table body]
            (if (seq args)
              (reduce
                (fn [[name-table body] expr]
                  (compile expr
                           f
                           name-table
                           body))
                [name-table body]
                args)
              [name-table body])]
        (render-expr expr name-table body)))) )
