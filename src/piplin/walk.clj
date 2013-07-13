(ns piplin.walk
  (:refer-clojure :exclude [compile cast])
  (:use [piplin types protocols]))

(def ^:dynamic *compiler-cache*)

(defn render-expr
  [expr->name+body expr name-table body]
  (if (contains? name-table expr)
    [name-table body]
    (let [[name body'] (expr->name+body expr name-table)]
      (if name
        [(assoc name-table expr name)
         (into body body')]
        [name-table body]))))

(declare compile*)

(defn reduce-kv-compile*
  [expr->name+body [name-table body] _ expr]
  (compile* expr expr->name+body name-table body))

(defn compile*
  [expr expr->name+body name-table body]
  (let [render-expr (partial render-expr expr->name+body)
        reduce-kv-compile* (partial
                             reduce-kv-compile*
                             expr->name+body)
        ]
    (if-let [result (@*compiler-cache* expr)]
      [name-table body]
      (doto
        (if (pipinst? expr)
          (render-expr expr name-table body)
          (let [args (get (value expr) :args {})
                [name-table body]
                (reduce-kv reduce-kv-compile*
                           [name-table body]
                           args)]
            (render-expr expr name-table body)))
        ((partial swap! *compiler-cache* assoc expr))))))

(defn compile
  "Takes an expr, a function that takes an expr and a map
  of exprs to names and returns the form for the expr and
  its generated name, and it returns a list of all the
  forms in order.

  You can provide pre-initialized name-lookup or form list."
  [expr expr->name+body name-table body]
  (binding [*compiler-cache* (atom {})]
    (compile* expr expr->name+body name-table body)))
