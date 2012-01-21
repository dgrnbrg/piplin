(ns piplin.core)

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

         [1] Need to express type restrictions
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
;Make syntax

(comment
  It's time for a new lesson! In hardware protocols!

  I think protocols are a great abstraction to link data types and functions
  since we want to be able to do things like addition for multiple times,
  even though those types are incompatible. What kinds of constraints exist?

  [1] We might have sintm8, uinte6, and sfxpt32.18, and they all might have
      addition defined on them, but all of the additions will be different.
      Thus the protocol must be able to generate the correct hardware.

  [2] We might want to allow adding uintm8 to uinte8, so we need a type
      checker that allows for this. Alternatively, support on the protocol
      may be the best way to represent these conversions, since explicit is
      better than implicit.

  [3] We may be able to generate 3 kinds of barrel shifters, one of which
      uses little logic and a MAC, one of which uses little logic but takes
      4 cycles, and one of which that uses a lot of logic but takes one
      cycle. We should be able to have the optimizer choose between them,
      which means encoding potenially several template expansions of the
      same functional unit. (Note that this can be deferred until we start
      looking at the template expansion).

  With these design goals, I think that an initial implementation can be
  done by making types specify multimethods that generate the appropriate
  hardware, and if no multimethod exists, then that isn't synthesizable.
  )

;(defmacro structural ...)

(comment
(structural [count ((uintm 8) 0)]
            (connect count (+ count 1)))
)
