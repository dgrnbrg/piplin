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

(def ^:dynamic sim-fn-args {})

(defn explode [& msgs]
  (throw (RuntimeException. (apply str msgs))))

(defmacro module
  "module is the fundamental structural building block in Piplin.
  It returns an AST fragment that can be processed into a synthesizable
  Verilog design or into a simulation. It has up to 4 declaration
  sections: :inputs, :outputs, :feedback, and :modules. :inputs is
  used to specify wires leading into this module. :outputs specify
  registered outputs from this module. :feedback is similar to :outputs,
  but they're only readable from within the module. :modules is for
  instantiating submodules in the final design."
  [config & body]
  (if-not (even? (count config))
    (explode "Odd number of elements in module args."))
  (let [params (partition 2 config)]
    (loop [unparsed (vec params) inputs nil outputs nil feedback nil modules nil]
      (if (seq unparsed)
        (let [[section decls] (first unparsed)]
          (if-not (vector? decls)
            (explode "Decls must be a vector")
            (let [keys (map keyword (take-nth 2 decls))
                  vals (take-nth 2 (rest decls))
                  decls (zipmap keys vals)]
              ;check here that decls are in the correct form
              (condp = section
                :inputs (if inputs
                          (explode "multiple definition of inputs" inputs)
                          (recur (rest unparsed) decls outputs feedback modules))
                :outputs (if outputs
                           (explode "multiple definition of outputs")
                           (recur (rest unparsed) inputs decls feedback modules))
                :feedback (if feedback
                            (explode "multiple definition of feedback")
                            (recur (rest unparsed) inputs outputs decls modules))
                :modules (if modules
                           (explode "multiple definition of modules")
                           (recur (rest unparsed) inputs outputs feedback decls))
                (explode "unknown section header:" section)))))
        (let [body body
              token (gensym "module")
              registers (map (fn [[x y]] [x `(:type ~y)])
                             (concat outputs feedback))
              exprs (map (fn [[x y]] 
                           [x `{:type ~y
                                :kind (:kind ~y)
                                :port ~x
                                :sim-factory [#(get sim-fn-args ['~token ~x]) []]
                                :token '~token}])
                         (concat inputs registers))
              bindings (mapcat (fn [[x y]] 
                                 [(symbol (name x))
                                  y])
                               (concat exprs modules))]

          `(let [~@bindings
                 connections# (atom [])
                 old-connect# connect]
             (binding [connect (fn [~'reg ~'expr]
                                 (if (= (:token ~'reg) '~token)
                                   (swap! connections#
                                          conj
                                          (connect-impl
                                            ~'reg
                                            ~'expr))
                                   (old-connect# ~'reg ~'expr)))]

               ~@body
               {:type :module
                :kind :module
                :token '~token
                :inputs ~inputs
                :outputs ~outputs
                :feedback ~feedback
                :modules ~modules
                :body @connections#})))))))

(defn connect
  {:dynamic true}
  [reg expr]
  (if (:token reg)
    (throw+ (error "Must call connect within a module"))
    (throw+ (error "Must connect to a register"))))

(defn connect-impl
  "This connects a register to an expr"
  [reg expr]
  {:type :connection
   :kind :connection
   :args {:reg reg
          :expr expr}})

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
         (entry? %))
    #(cond
       (map? %) (seq %)
       (vector? %) (seq %)
       (entry? %) (seq %))
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
                z/right mz)))

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
                   (zipseq (z/down (go-down mz :modules)))))
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
           (filter #(= (-> % z/node :type)
                       :connection)
                   (zipseq (z/down (go-down mz :body))))))
    combine))

(defn make-sim-fn
  "Takes a map-zipper of the ast of an expr
  and walks along the expr. It returns a pair whose
  first element is a function that computes the expr
  and takes no args (needed ports come via binding).
  The function collects its args into a map which it
  binds before invoking the function so that the
  ports can get their values at the bottom."
  [mz]
  (let [[my-sim-fn my-args] (-> mz
                              (go-down :sim-factory)
                              z/node)]
    (if-let [args (go-down mz :args)]
      (let [args (-> args z/down zipseq)
            arg-fns (map #(make-sim-fn (mz-val %)) args)
            arg-map (zipmap (map (comp z/node mz-key)
                                 args)
                            arg-fns)
            fn-vec (map #(get arg-map % ) my-args)]
        (fn []
          (apply my-sim-fn (map #(%) fn-vec))))
        (if (seq my-args)
          (throw (AssertionError. (str "lol" my-args)))
          (fn [] (my-sim-fn))))))

(defn make-sim
  "Takes an elaborated hierarchy of modules and returns a
  pair of [state fns] that can be simulated with
  exec-sim. See exec-sim for details."
  [root]
  (let [mz (map-zipper root)
        get-qual-state (fn [module]
                         (let [token (z/node (go-down module :token))
                               regs (merge (z/node (go-down module :outputs))
                                           (z/node (go-down module :feedback)))]
                           (apply hash-map (mapcat (fn [[k v]]
                                                     [[token k] v])
                                                   regs))))
        initial-state (walk-modules mz get-qual-state merge)
        initial-connections (walk-connects mz z/node concat)]
    initial-connections))

    ;first walk modules
    ;use feedbacks and outputs to figure out initial state array
    ;(drive) connections (formerly link) specify aliases between
    ;  state elts. In the end, reprocess arglists to resolve names
    ;exprs can be turned into fns w/ arglists


(comment
  How to get the combinational function for ports.

  We find all ports and build the arglist.
  The arglist is going to get used to fill in a map that'll
  be set into a binding when the function is executed.
  As the function is built, all termini will be const or
  ports. The ports' access fn will read the value from
  the binding.)


(comment
  First make nested modules connect and information hiding
  work properly. Next, add a semantic check to verify
  that everything was connected.

  At this point we can try for toVerilog, or we can start
  on the simulator (cycle accurate, but can schedule in
  any number of cycles, and that deferral is itself
  dynamic). The simulator must work with a given protocol or
  function or interace so that custom tasks can be written too.
  )
