(ns piplin.modules
  (:use piplin.types)
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

(defn go-down
  "Gets a key k from beneath the given
  map-zipper node"
  [mz k]
  (loop [loc (z/down mz)]
    (if (or (nil? loc)
            (= (key (z/node loc)) k))
      (-> loc z/down z/right) ;get val of entry
      (recur (z/right loc)))))

(defn go-path-down
  "Navigates a specific path from the current loc"
  [mz path]
  (if-let [path (seq path)]
    (recur (go-down mz (first path)) (rest path))
    mz))

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
