(ns piplin.core)

(comment
  The basis of building the type system is to have a type lattice.
  Using a fn that takes an obj and a target type, it must be able to cast the obj to the target type or throw an exception (or be a predicate)
  This lattice could be either a logic program or a contract set of fns.
  A logic program could enumerate the most specific types of all the nodes.
  Must consider how to flow types through functions.
  )

(comment
  The AST is represented as a tree of computation, the only exception being
  structural blocks. Structural blocks create a context for forward decls,
  and (connect) links the forward decl to a fragment.)

(comment
  Type inference has 2 phases--flowing through trees, and resolving structures

  The tree-flowing occurs with AST--consider the 8 bit up-counter

  (defn up-counter []
    (:value (structural [value (make-type (uintm 8) 0)]
      (connect value (inc value)))))

  This will give an AST fragment like

  {:type :structural
   :decls {:value {:type [:uintm 8] :value 0}}
   :connections {:value {:type :adder
                           :lhs {:type [:forward-decl :value]}
                           :rhs {:type :int :value 1}}}
   :value {:type :extractor :structure ... :port :value}}

  Here we can see the structural block is entirely made out of forward decls
  and connections. It also exposes its ports separately.)

(comment
  It's hard to see how one can actually typecheck this thing.
  Ultimately, our goal is to find the most specific type for all connections
  between functions, or determine that there is a conflict, or determine
  that it's not synthesizable and don't worry about it.

  One issue is that if we rewrite the types into the tree during the walk
  and the same subtree appears in multiple places, those subtrees will become
  different instances of those nodes. This seems to imply that the types must
  be a separate table keyed by the fragments.

  An alternative solution says that each fragment is inferred separately, and
  generates separate code/sim, and that the synth tool must merge them. This
  is probably not a good solution because it doesn't match what's most logical

  )

(comment
  The type system is structural.)

;I'm not sure that this is the correct level to express these things
;I'd really like to just have a bunch of rules to convert from the tree
;to verilog or a simulation model

(comment
(defsynthline int-adder [x y]
  ;this is the typechecking fn
  ;it'll be called w/ the types of the arguments
  ;the last 
  (fn [x y out]
    (is-int x)
    (== x y)
    (== y out))
  :verilog {:file "int-adder.v" ;idea for linking to verilog/vhdl impl
            :portmap {:x "x"
                      :y "y"
                      :out "out"}} ;need to decide how to specify syn/ack lines on input and output
  :sim +)
  )

(comment What kinds of conversion rules are needed to convert to simulation
         or synthesis?

         [1] Need to express type restrictions (use logic programming)
         [2] Need to increase specificity of some pipelines and ports
             - this is essentially just storing the results of [1]
         [3] At this point we can have typechecked DAGs for the pipelines
             and arbitrary graphs for the structures. We will initially not
             try to optimize the cycle boundaries of structures, so we just
             need to connect the pipelines into flows.
         [4] To convert a pipeline to a function, each record will have its
             final type in the type-specification map. We can use that type
             along with the record's type to lookup what the actually impl
             function is.
         [5] We can wrap that function into a simulation element.
         [6] We can go through each structure and wire up the simulation
             elements by telling them where to send their results.)

;Impl plan:
;Define simulation primitives
;Define structure->sim conversion
;Define typechecking phase
;Make syntax

;TODO: still needs max cycle to run until

(defn sim-elt [logic argnames fwd]
  "Returns a new simulation element with a function logic "
  "and inputs argnames and value-forwarder fwd"
  {:dests []
   :logic logic
   :fwd-fn fwd
   :inputs (zipmap argnames (repeat []))})

(defn add-dest [se name dest]
  "Adds [name dest] to the list of :dests in se"
  (let [dests (:dests se)]
    (assoc se :dests (conj dests [name dest]))))

(defn add-input [sim-elt key val]
  "Appends an input named key with value val to the sim-elt"
  (let [input-key [:inputs key]
        input-vec (get-in sim-elt input-key)]
    (assoc-in sim-elt input-key (conj input-vec val))))

(defn- map-vals [f m]
  "takes a function of one argument and a map and returns a new map "
  "with the same keys and whose values are the (f old-value)"
  (reduce (fn [tmp-map old-entry]
            (assoc tmp-map (key old-entry) (f (val old-entry))))
          {}
          m))

(defn send-from [sim-elt val]
  "Takes a sim-elt and a value, and sends the val to all of the sim-elt's "
  "destinations. If a sim-elt is a pipeline stage, can be used to "
  "initialize the stage"
  (doseq [addr (:dests sim-elt)]
    (apply (:fwd-fn sim-elt) val addr)))

(defn process-sim-elt [sim-elt]
  "takes a sim-elt and, if every input list has at least one element, "
  "removes the head of each element, computes the result, and invokes "
  "(fwd result name dest) for every destination. Returns the procesed "
  "sim-elt."
  (if (every? #(seq (val %)) (:inputs sim-elt)) ;all inputs are ready
    (let [inputs (:inputs sim-elt)
          inputs-first (map-vals first inputs)
          inputs-rest (map-vals rest inputs)
          result ((:logic sim-elt) inputs-first)]
      (send-from sim-elt result)
      (recur (assoc sim-elt :inputs inputs-rest)))
    sim-elt))

(defn add-input-process [sim-elt key val]
  "Adds the value to the input named key for the sim-elt"
  (process-sim-elt (add-input sim-elt key val)))

(defn- agent-fwd [result name dest]
  "fwd function for agents"
  (send dest add-input-process name result))

(def adder (sim-elt (fn [{x :x y :y}]
                      (+ x y))
                    [:x :y]
                    nil))

(def printer (sim-elt (fn [{a :a}]
                        (println "printing result:" a "end result"))
                      [:a]
                      nil))

(comment

(def p (agent (assoc printer :fwd-fn agent-fwd)))
(send p add-input-process :a "hello world")

(let [agent-adder (assoc adder :fwd-fn agent-fwd)]
  (def a1 (agent agent-adder))
  (def a2 (agent agent-adder)))

(send a1 add-dest :x a2)
(send a2 add-dest :a p)

(send a1 add-input-process :y 2)
(send a2 add-input-process :y 7)

(send a1 add-input-process :x 1)

  )
