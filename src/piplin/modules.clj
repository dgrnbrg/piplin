(ns piplin.modules
  (:use [piplin sim types])
  (:use [slingshot.slingshot :only [throw+]])
  (:require [clojure.zip :as z]))

(comment
  The syntax for a module is

; (module [:inputs
           [a type
            b type]
           :outputs
           [o1 init-val]
           :feedback
           [x init-val]
           :modules
           [sub1 (instantiate)
            sub2 (instantiate) :only [:port1 :port2]]
;   body...)

  The body should include connections for everything that
  needs one.

  There are 4 sections, provided in this order, each of
  which should have a nonempty list of symbol/value pairs,
  like "let". The value varies from being a type to an
  initialization value to a module instance.

  Module instances can use :exclude and :only like for imports
  to only refer to certain ports and leave the rest unconncted.

  The body can contain connections between registers. It's the
  responsibility of a semantic validation/error checking pass
  over the tree to verify that inputs and outputs are fully
  connected.

  The :inputs section is for inputs to the process. The
  :outputs section is for registered outputs from the
  process. The :feedback section is for registers that
  aren't outputs (kind of like a private :output). :modules
  is for other submodules, and then all the specified ports
  (no spec = all ports) must be connected somewhere in
  the body.
  )

(comment
  All exprs must list their subexprs that should be checked
  as an :args map. This map's keys are used by the synth/sim
  phases to do something specific, but the values are implied
  to be exprs that should be walked and checked.
  )

(defn explode [& msgs]
  (throw (RuntimeException. (apply str msgs))))

(defn- parse-sections
  "take a vector of pairs of bindings and their
  headers (config) as well as a seq of acceptable headers.
  Returns a map of the section header to nil if
  undefined or to a map of keys to values of the
  binding. Converts the binding keys to keywords."
  ([config headers]
   (parse-sections (partition 2 config) headers {}))
  ([params headers result]
   (if (seq params)
     (let [[section decls] (first params)]
       (if-not (vector? decls)
         (explode "Decls must be a vector")
         (let [keys (map keyword (take-nth 2 decls))
               vals (take-nth 2 (rest decls))
               decls (zipmap keys vals)]
           ;check here that decls are in the correct form
           (if-not (some #{section} headers)
             (explode section " not found in " headers)
             (if (contains? result section)
               (explode "multiple definition of " section)
               (recur (rest params)
                      headers
                      (assoc result section decls)))))))
     result)))

(defn- make-port
  "Takes a keyword name, an owning module token, and
  a list which can be (eval)ed to get the port's type
  and returns a list which can be (eval)ed to get
  the port."
  [name token type-syntax]
  `(vary-meta
     (piplin.types.ASTNode. ~type-syntax 
                            {:port ~name 
                             :token '~token}
                            {:pipinst? (fn [x#] false)})
     assoc :sim-factory
     [#(get sim-fn-args ['~token ~name]) []]))

(defn- tuplefn
  "Takes a function of n arguments and an index <n
  and returns a function of 1 argument that returns
  the n-element tuple that was passed in with the
  index element replaced by the return value of
  applying the tuple to the given function."
  [index f]
  (fn [tuple]
    (assoc tuple index (apply f tuple))))

(defn make-connect-impl
  "Takes a token of the module at hand and the symbol
  that will be bound to the (atom []) of connections
  that will be made and a symbol that will be bound to
  the parent module's connect fn and returns the syntax
  of the connect function that should be bound for
  that module."
  [token connections old-connect]
  `(fn [~'reg ~'expr]
    (if (= (:token (value ~'reg)) '~token)
      (swap! ~connections
             conj
             (connect-impl
               ~'reg
               ~'expr))
      (~old-connect ~'reg ~'expr))))

(defmacro module
  "module is the fundamental structural building block
  in Piplin. It returns an AST fragment that can be
  processed into a synthesizable Verilog design or into
  a simulation. It has up to 4 declaration sections:
  :inputs, :outputs, :feedback, and :modules. :inputs
  is used to specify wires leading into this module.
  :outputs specify registered outputs from this module.
  :feedback is similar to :outputs, but they're only
  readable from within the module. :modules is for
  instantiating submodules in the final design."
  [config & body]
  (if-not (even? (count config))
    (explode "Odd number of elements in module args."))
  (let [{:keys [inputs outputs feedback modules]}
        (parse-sections
          config [:inputs :outputs :feedback :modules])]
    (let [token (gensym "module")
          connections (gensym "connections")
          old-connect (gensym "old-connect")
          registers (map (tuplefn 1
                           #(identity `(typeof ~%2)))
                         (concat outputs feedback))
          exprs (map (tuplefn 1
                       #(make-port %1 token %2))
                     (concat inputs registers))
          bindings (mapcat (tuplefn 0
                             (fn [x y] (symbol (name x))))
                           (concat exprs modules))]
      `(let [~@bindings
             ~connections (atom [])
             ~old-connect connect]
         (binding [connect ~(make-connect-impl
                              token connections
                              old-connect)]
           ~@body
           {:type :module
            :token '~token
            :inputs ~inputs
            :outputs ~outputs
            :feedback ~feedback
            :modules ~modules
            :body @~connections})))))

(defn connect
  {:dynamic true}
  [reg expr]
  (if (:token reg)
    (throw+ (error "Must call connect within a module"))
    (throw+ (error "Must connect to a register"))))

(defn connect-impl
  "This connects a register to an expr"
  [reg expr]
  (piplin.types.ASTNode. :connection 
            {:args {:reg reg 
                    :expr expr}}
            {}))

(defn entry [k v]
  (clojure.lang.MapEntry. k v))

(defn entry? [e]
  (instance? clojure.lang.MapEntry e))

(defn map-zipper
  "The map-zipper is a nested structure
  of vectors and maps. There are 4 kinds
  of nodes: vectors, maps, mapentries, and
  other objects. The former 3 have children.
  MapEntry children "
  [root]
  (z/zipper
    #(or (map? %)
         (vector? %)
         (entry? %)
         (not (pipinst? %)))
    #(cond
       (map? %) (seq %)
       (vector? %) (seq %)
       (entry? %) (seq %)
       :else
       (seq (value %)))
    (fn [node children]
      (cond
        (map? node) (zipmap (map key children)
                            (map val children))
        (vector? node) (vec children)
        (entry? node) (if-not (= (count children) 2)
                        (throw (RuntimeException.
                                 "MapEntry has 2 children"))
                        (entry (first children)
                               (second children)))))
    root))

(defn mz-key
  "Gets a key from a mapentry zipper"
  [loc]
  (-> loc z/down))

(defn mz-val
  "Gets a value from a mapentry zipper"
  [loc]
  (-> loc z/down z/right))

(defn go-down
  "Gets a key k from beneath the given
  map-zipper node"
  [mz k]
  (loop [loc (z/down mz)]
    (cond
      (nil? loc) nil
      ;get val of entry
      (= (key (z/node loc)) k) (mz-val loc)
      :else (recur (z/right loc)))))

(defn go-path-down
  "Navigates a specific path from the current loc"
  [mz path]
  (if-let [path (seq path)]
    (recur (go-down mz (first path)) (rest path))
    mz))

(defn zipseq
  "Given a node, returns a lazy seq of zippers
  representing each node to the right (no descent)"
  [mz]
  (take-while (comp not nil?)
              (iterate
                z/right (z/down mz))))

(defn walk-modules
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce across the values
  given by (visit module) for every module."
  [mz visit combine]
  (let [x (visit mz)]
    (if-let [submodules (seq (z/node
                               (go-down mz :modules)))]
      (reduce combine x
              (map (comp #(walk-modules % visit combine)
                         mz-val)
                   (zipseq (go-down mz :modules))))
      x)))

(defn walk-connects
  "Takes a map-zipper of the ast and applies
  the function combine as a reduce operation
  across the values given by (visit connect)
  for every connection in every module"
  [mz visit combine]
  (walk-modules
    mz
    (fn [mz]
      (map visit
           (filter #(= (-> % z/node typeof)
                       :connection)
                   (zipseq (go-down mz :body)))))
    combine))

(defn walk-expr
  [exprz visit combine]
  (let [x (visit exprz)]
    (if-let [argsz (go-down exprz :args)]
      (let [subexprs (zipseq argsz)]
        (reduce combine x
                (map (comp #(walk-expr % visit combine)
                           mz-val)
                     subexprs)))
      x)))

(comment
  How to get the combinational function for ports.

  We find all ports and build the arglist.
  The arglist is going to get used to fill in a map that'll
  be set into a binding when the function is executed.
  As the function is built, all termini will be const or
  ports. The ports' access fn will read the value from
  the binding.
  
  This is done by make-connection and make-sim-fn.)

(defn make-sim-fn
  "Takes a map-zipper of the ast of an expr
  and walks along the expr. It returns a pair whose
  first element is a function that computes the expr
  and takes no args (needed ports come via binding).
  The function collects its args into a map which it
  binds before invoking the function so that the
  ports can get their values at the bottom."
  [mz]
  (let [[my-sim-fn my-args]
        (if (pipinst? (z/node mz)) 
          [#(identity (z/node mz)) []] 
          (-> mz
            z/node
            meta
            :sim-factory))]
    (if-let [args (go-down mz :args)]
      (let [args (zipseq args)
            arg-fns (map #(make-sim-fn (mz-val %)) args)
            arg-map (zipmap (map (comp z/node mz-key)
                                 args)
                            arg-fns)
            fn-vec (map #(get arg-map %) my-args)]
        (fn []
          (apply my-sim-fn (map #(%) fn-vec))))
        (if (seq my-args)
          (throw (AssertionError. (str "lol" my-args)))
          (fn [] (my-sim-fn))))))

(def ^:dynamic sim-fn-args {})

(defn make-connection
  "Takes a map-zipper of the ast of a connection
  and returns a pair of [f states]. f is a function
  that takes the needed ports as arguments (listed in
  states), binds them to the sim-fn-arg binding, and
  invokes the no-arg sim-fn to get the result."
  [mz]
  (let [reg (z/node (go-path-down mz [:args :reg]))
        expr (go-path-down mz [:args :expr])
        sim-fn (make-sim-fn expr)
        reg-state [(:token reg) (:port reg)]
        ports (walk-expr mz
                         #(let [n (z/node %)]
                            (if-let [port (:port n)]
                              [[(:token n) port]]
                              nil))
                         concat)]
    (every-cycle
       (fn [& vals]
         (binding [sim-fn-args (zipmap ports vals)]
           (sim-fn)))
       ports
       reg-state)))

(defn- get-qual-state
  "This function takes a module and returns a
  map whose keys are [token port] pairs (token
  is a gensym unique to the module instance and
  port is a keyword) and whose values are the
  initial values of the registers of the module
  (from the :feedback and :outputs sections).
  This state can be used by the simulation
  engine."
  [module]
  (let [token (z/node (go-down module :token))
        regs (merge (z/node (go-down module :outputs))
                    (z/node (go-down module :feedback)))]
    (apply hash-map (mapcat (fn [[k v]]
                              [[token k] v])
                            regs))))

(defn make-sim
  "Takes an elaborated hierarchy of modules and returns a
  pair of [state fns] that can be simulated with
  exec-sim. See exec-sim for details."
  [root]
  (let [mz (map-zipper root)
        initial-state (walk-modules mz get-qual-state
                                    merge)
        connections (walk-connects mz make-connection
                                   concat)
        connections (->> connections
                      (apply concat)
                      (apply hash-map))]
    [initial-state connections]))

(comment
  First make nested modules connect and information hiding
  work properly. Next, add a semantic check to verify
  that everything was connected. This should involve
  several semantic checks and useful errors.

  Next, I must write more functions, like inc, dec, slice,
  bits, concat, and the bits type. I'll also need to write
; if/mux and case.

  At this point we can either try for toVerilog or
  implement structs or vectors, including pattern
  matching aka destructuring. )
