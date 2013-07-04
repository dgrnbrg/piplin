(ns piplin.modules
  (:use [piplin types protocols])
  (:use [clojure.string :only [join]]
        [clojure.set :only [map-invert]])
  (:use [slingshot.slingshot :only [throw+]])
  (:refer-clojure :exclude [replace cast])
  (:use [clojure.string :only [join replace split]])
  (:use [swiss-arrows.core :only [-<> -<>>]])
  (:use [clojure.pprint :only [pprint]])
  (:require [plumbing.graph :as graph]
            [plumbing.core :as plumb]
            [piplin.walk :as walk]))

(defn make-port*
  "Takes a keyword name, an owning module token, and
  the port's type and returns the port."
  [name piplin-type port-type]
  (alter-value (mkast piplin-type :port []
                      #(throw+
                         (error "no longer using sim-fn")))
               merge
               {:port name
                :port-type port-type}))

(declare make-input-map)

(def ^:dynamic *module-path* [])

(defn walk-expr
  [expr visit combine]
  (let [x (visit expr)]
    (if-let [args (:args (value expr))]
      (let [subexprs (vals args)]
        (reduce combine x
                (map #(walk-expr % visit combine)
                     subexprs)))
      x)))

(def ^:dynamic *sim-state*)

(def fn-lookup (atom {}))

(defn make-sim-expr
  "Returns a pair of the name, and the line of the `let` binding
   that defines the name."
  [expr name-table]
  (let [name (gensym)]
    (cond
      (pipinst? expr)
      (do
        (swap! fn-lookup assoc name expr)
        [name [name `(@fn-lookup '~name)]])
      (= (-> expr value :port-type) :register)
      [name [name `(get *sim-state* ~(:port (value expr)))]]
      :else
      (let [[sim-fn arg-names] (-> expr meta :sim-factory)
            args (mapv (comp name-table (-> expr value :args)) arg-names)]
        (swap! fn-lookup assoc name sim-fn)
        [name [name `((@fn-lookup '~name) ~@args)]]))))

(declare make-sim-fn-legacy)

(defn make-sim-fn
  "Takes a sequence of exprs, and returns a function that returns
   a vector containing that sequence of exprs, evaluated."
  [exprs]
  ;;TODO: remove this legacy check and upgrade unit tests
  (if-not (nil? (typeof exprs))
    (make-sim-fn-legacy exprs)
    (let [[name-table body]
          (reduce (fn [[name-table body] expr]
                    (walk/compile expr make-sim-expr name-table body))
                  [{} []]
                  exprs)]
      (eval `(fn []
               (let [~@body]
                 ~(mapv name-table exprs)))))))

(defn make-sim-fn-legacy
  "This is for compatibility with old make-sim-fn tests."
  [expr]
  (comp first (make-sim-fn [expr])))

(defn push-down-map
  "Takes a map and a keyword maps all values maps with the
  key as the keyword and the value as the old value."
  [m kw]
  (plumb/for-map [[k v] m]
                 k {kw v}))

;;This stores the path of the current module--used for outputing hierarchical data
(def ^:dynamic *current-module* [])
;;This is an atom containing a map of state element paths to a map
;;of their sim fns (:fn) and initial values (:init)
(def ^:dynamic *state-elements*)

;TODO: automatically cast the return values of fnks
;that are being assigned to regs
(defn modulize
  ([computation state]
   (assert (map? computation)
           "You forgot to include the register map")
   (modulize
     (-<> (RuntimeException.)
          .getStackTrace
          ^java.lang.StackTraceElement (aget 2)
          .getLineNumber

          (str "module_" <>)
          keyword
          )
     computation state))
  ([module-name computation state]
   (doseq [[k v] state
           :when (not= (kindof v) :array)]
     (assert (computation k)
             (str "Missing register definition for " k)))
   ^::module
   (fn [& inputs]
     (assert (every? keyword? (take-nth 2 inputs)))
     (binding [*current-module* (conj *current-module*
                                      module-name)]
       (let [state-renames (plumb/for-map [k (keys state)]
                             k (keyword (name (gensym))))
             reverse-renames (map-invert state-renames)
             renamed-computation
             (plumb/map-keys #(or (state-renames %) %)
                             computation)
             init-map (push-down-map state ::init)
             register-ports (plumb/for-map [[k v] state]
                              k (make-port*
                                  (conj *current-module* k)
                                  (typeof v)
                                  :register))
             port-map (push-down-map register-ports ::port)
             result (-<>> (graph/run
                            renamed-computation
                            (if (seq inputs)
                              (apply assoc register-ports inputs)
                              register-ports))
                          (plumb/map-keys
                            #(or (reverse-renames %) %)))
             result-fns (plumb/for-map [[k v] result
                                    :when (and (typeof v)
                                               (not= (-> v value :op) :array-store))]
                                   k v)
             store-fns (plumb/for-map [[k v] result
                                       :when (= (-> v value :op) :array-store)
                                       :let [{:keys [array index write-enable v]}
                                             (-> v value :args)]]
                                      k {::index index
                                         ::write-enable? write-enable
                                         ::value v
                                         ::dest array})
             fn-map (push-down-map result-fns ::fn)
             state-elements (->> (merge-with
                                   merge
                                   init-map
                                   fn-map
                                   store-fns
                                   port-map)
                              (plumb/map-keys
                                #(conj *current-module* %)))]
         (when (bound? #'*state-elements*)
           (swap! *state-elements* merge state-elements))
         ;We actually want to refer to registers, not their inputs,
         ;in this map
         (merge result-fns register-ports))))))

(defn store?
  "Returns true if the given map represents a store ast node"
  [value]
  (contains? value ::write-enable?))

(defn register?
  "Returns true if the given map represents a register ast node"
  [value]
  (contains? value ::port)) 

(defn wire?
  "Returns true if the given map represents a wire ast node"
  [value]
  (not (or (store? value) (register? value))))

(defn compile-root
  [module & inputs]
  (assert (::module (meta module)) "Must pass a module as first argument")
  (binding [*state-elements* (atom {})]
    (apply module inputs)
    (with-meta @*state-elements*
               {::compiled true})))

(defn find-exprs
  [compiled-module pred]
  (apply concat
         (map #(-> % second
                 ::fn
                 (walk-expr (fn [expr]
                              (if (pred expr)
                                [expr]))
                            concat))
              compiled-module)))

(defn find-inputs
  [compiled-module]
  (find-exprs compiled-module
              #(= :input (-> % value :port-type))))

(def ^{:arglists (list '[name type])}
  input #(make-port* %1 %2 :input))

(defn make-port->mem-name
  "Takes a module and return a map from port to
  the memory's keyword name."
  [compiled-module]
  (let [mems (->> compiled-module
               (filter #(-> % second ::init kindof
                          (= :array))))]
    (plumb/for-map [[name {port ::port}] mems]
                   port name)))

(defn compute-store-fns
  "Given a map of memory registers and a list of store ops,
  make a map from memory names to their simulation-store fns."
  [registers stores]
  ;;TODO: check that registers is a subset of arrays in stores
  (let [stores (map (fn [{dest ::dest :as reg}]
                      (assoc reg
                             ::dest
                             (-> dest value :port)))
                    stores)
        reg->stores (group-by ::dest stores)]
    (plumb/for-map [[reg {f ::fn}] registers
                    :let [stores (->> (reg->stores reg)
                                      (map (comp
                                             (partial map make-sim-fn)
                                             (juxt ::index
                                                   ::write-enable?
                                                   ::value))))
                          f (make-sim-fn f)]]
                   reg (fn []
                         (reduce (fn [mem [i we v]]
                                     (if (we)
                                       (assoc mem (i) (v))
                                       mem))
                                 (*sim-state* reg)
                                 stores)))))

(defn module-keys-by-type
  "Takes a compiled module and returns a map containing
   reg-keys, store-keys, and wire-keys. These are useful
   to compilers, as they're the 3 kinds of runnable code."
  [compiled-module]
  (let [reg-keys (->> compiled-module
                      (filter (comp register? second))
                      (map first))
        store-keys (->> compiled-module
                        (filter (comp store? second))
                        (map first))
        wire-keys (->> compiled-module
                       (filter (comp wire? second))
                       (map first))]
    {:reg-keys reg-keys
     :store-keys store-keys
     :wire-keys wire-keys}))

(defn sim
  [compiled-module cycles]
  (assert (::compiled (meta compiled-module))
          "Module must be compiled")
  ;;TODO: is this next line a perf black hole?
  #_(assert (empty? (find-inputs compiled-module))
          "Cannot have any input ports during simulation")
  (let [{:keys [reg-keys store-keys wire-keys]} (module-keys-by-type compiled-module)
        wire-fn (->> wire-keys
                     (map (comp ::fn #(get compiled-module %)))
                     (make-sim-fn))
        reg-fn (->> reg-keys
                    (map (comp ::fn #(get compiled-module %)))
                    (remove nil?)
                    (make-sim-fn))
        ;store-fns (compute-store-fns (select-keys compiled-module reg-keys)
        ;                             (vals (select-keys compiled-module store-keys)))
        port->mem-name (make-port->mem-name compiled-module)
        reg-inits (plumb/map-vals
                    ::init
                    (select-keys compiled-module reg-keys))
        inits (binding [*sim-state* reg-inits]
                (merge reg-inits
                       (zipmap wire-keys
                               (wire-fn))))]
    (loop [state inits
           cycles cycles
           history [inits]]
      (if (zero? cycles)
        history
        (let [reg-state (binding [*sim-state* state]
                          (merge state
                                 ;(plumb/map-vals #(%) store-fns)
                                 ;(plumb/map-vals #(%) reg-fns)  
                                 (zipmap (remove (comp nil? ::fn #(get compiled-module %)) reg-keys)
                                         (reg-fn)))) 
              wire-state (binding [*sim-state* reg-state]
                           (merge reg-state
                                  (zipmap wire-keys
                                          (wire-fn))))]
          (recur
            wire-state
            (dec cycles)
            (conj history wire-state)))))))
