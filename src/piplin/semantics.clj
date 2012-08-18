(ns piplin.semantics
  #_(:refer-clojure :as clj :exclude [not= bit-or bit-xor + - * bit-and inc dec bit-not < > <= >= = cast not cond condp])
  (:use [piplin.protocols]
        [piplin.types :only [kindof]]
        [piplin.modules :only [walk-modules]]))

;The following are needed semantic checks:
;* Whether the index's bit width matches up to the array's
;* Are there no assoc in the body of modules when synthesizing
;* Do the assigned-to indices in the connect-impl don't have the array or the index constant (code won't work w/ constant atm)
;* Check that each port is connected to once
;* Check that reported pipinsts are in fact pipinsts
;* Include a special "terminated" type which can be connected to in order to add roots to trace unconnected nodes, which enables stricter operation
;** All elements of a bundle which are read are used
;* Check for duplicate declartionso of the same ports (also must turn maps into vectors of pairs in module
;
;What kinds of errors are there?
;* Errors in type declarations
;* Nodes can have errors (perhaps here we could return a special AST that we search for)
;* Ports could be errors (duplicate input connections

(defn duplicated-ports?
  [module]
  (let [ports (:ports module)
        ports (map #(select-keys (value %) [:port :port-type]) ports)
        messages (atom [])]
    (reduce (fn [seen port]
              (when (seen port)
                (swap! messages conj
                       (str "Duplicate port named " (:port port)
                            " in module " (:token module))))
              (conj seen port))
            #{}
            ports)
    @messages))

(defn collect-ast-errors
  [expr]
  (when (= :error (:op (value expr)))
    [(value expr)]))
