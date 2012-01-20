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

(defrecord Fragment [op args])

(defrecord Instance [type val])

(defn instance [type val]
  (Instance. type val))

;TODO: enforce that all instances are positive
(defrecord UIntM [type n]
  clojure.lang.IFn
  (invoke [this init-val]
    (instance this init-val)))

(defprotocol IPromotable
  "Used for instance objects"
  (dopromote [type obj]
             "Produces an instance of the given "
             "obj with the type changed"))

(defprotocol IType
  "Used for type objects"
  (kindof [this] "Returns kind of this type."))

(defprotocol ITyped
  "Used for instance objects"
  (type [this] "Returns type instance. "
        "Any parameters have concrete values")
  (kind [this] "Returns kind. Parameters "
        "are ignored"))

(defn uintm [n]
  (UIntM. :uintm n))

(defprotocol IPiplinType
  (piplin-type [this] "returns a keyword representing the type"))

(extend-protocol IPiplinType
  Fragment
  (piplin-type [this] :fragment)
  Instance
  (piplin-type [this] (kind this))
  Number
  (piplin-type [this] :number)
  Integer
  (piplin-type [this] :int)
  Long
  (piplin-type [this] :int))

(defn nary-dispatch
  ([] ::nullary)
  ([x] (piplin-type x))
  ([x y] [(piplin-type x) (piplin-type y)])
  ([x y & more] ::n-ary))

(defmacro defbinop [op zero]
  `(do
     (defmulti ~op nary-dispatch)
     (defmethod ~op ::nullary [] ~zero)
     (defmethod ~op ::n-ary
       [~'x ~'y & ~'more]
       (if ~'more
         (recur (~op ~'x ~'y) (first ~'more) (next ~'more))
         (~op ~'x ~'y)))))

(defbinop + 0)

(declare uintm-adder)

(defmethod + [:int :int] [x y] (clojure.core/+ x y))
(defmethod + [:number :number] [x y] (clojure.core/+ x y))
(defmethod + [:uintm :uintm] [x y] (uintm-adder x y))

(comment
(defprimitive slice [bits low high]
  (typecheck (bits? bits)
             (> high low)
             (>= low 0))
  ...)

(defprimitive uintm-adder [lhs rhs]
  (typecheck
    ;This typecheck works for if they're both uintm
    (fresh [lhs-bits rhs-bits]
           (== {:type uintm :n lhs-bits} lhs)
           (== {:type uintm :n rhs-bits} rhs)
           (== lhs-bits rhs-bits)))
  ...)
)
(comment
  How to determine which implementation of each function is permitted.

  [1] When declaring components, you must know all the concrete types
      supported in that position, be they Numbers, Longs, Pipelines,
      or other objects. This list is supplied as a list of predicates.
      The predicates must return a proper type-encoded AST fragment.

  [2] Now you need to figure out what types everything should be. Each
      component must have had type-flows provided for each type. The
      type flows express the constraints between the input and output
      types. The constraint is given as a function on the :types of
      all the inputs and the output of the component. When the typechecker
      solves the constraint problem, every node now has an acceptable
      type (nodes with unknown type initially are given type (lvar),
      nodes with partial types initially can be given types with some
      parameters having (lvar)).

      The coerce function can then be used to realize all of the
      assigned types that the type checker discovers.

  [3] The type checker is executed with (run 2 ...), so that we can detect
      ambiguities. The checker runs by walking through the tree,
      extracting the types of arguments and calling logic functions as
      it does the traditional logic rewrite of the tree-based calls.
      Structural blocks must be handled specially, by carrying the ports
      through a (binding) context to allow the types of special port
      references to be resolved to the same logic variable (or they could
      get unified through the binding).

  Appendix
  [1] Derived types (typedef) can be done by prefix-checking on lists
      in a :derivation field of the type
  )

(comment
;Interesting type example
;TODO: show that the above algorithm works for it
;note that we should get that a is a uintm with under 32 bits
(structural [a ^input (unknown-type) b ((uintm 32) 0)]
            (connect b (+ (zext a) 7)))
;yields this structure
{:type :structure
 :inputs {:a {:type (lvar)}}
 :outputs {:b {:type :uintm :n 32}}
 :connections {:b {:op :add ;can't know what we're adding!!
                   :lhs {:op :zext
                         :arg {:port :a}}
                   :rhs 7}}}
;Here, we get to make this sequence of deductions:
;b is uintm32 so the :add is a uintm32
;so lhs and rhs are uintm32
;so :port :a is uintm<32
;and the jvm-int is a uintm32

;transposing + and zext is interesting
(structural [a ^input (unknown-type) b ((uintm 32) 0)]
            (connect b (zext (+ a 7))))
;yields this structure
{:type :structure
 :inputs {:a {:type (lvar)}}
 :outputs {:b {:type :uintm :n 32}}
 :connections {:b {:op :zext
                   :arg {:op :add
                         :lhs {:port :a}
                         ;we get to deal w/ jvm ints as if they had no type
                         ;but this couldn't have been made if autocast wasn't
                         ;possible
                         :rhs 7}}}}
;Here, we get to make this sequence of deductions:
;b is uintm32 so the :zext is a uintm32
;so :add is uintm<32
;so lhs and rhs are uintm<32
;so :port :a is uintm<32
;but 7 is :uintm<32, which can't be further specified. FAIL!

(structural [a ^input (uintm 16) b ((uintm 32) 0)]
            (connect b (+ (zext a) b)))
{:type :structure
 :inputs {:a {:type :uintm :n 16}}
 :outputs {:b {:type :uintm :n 32}}
 :connections {:b {:op :add
                   :lhs {:op :zext
                             :arg {:port :a}}
                   :rhs {:port :b}}}}
;Here, we get these deductions
;b is uintm32, so :add is uintm32
;thus :rhs remains consistent, and :zext is uintm32
;thus :port :a remains consistent

;this should fail due to type mismatch
(structural [a ^input (uintm 16) b ((uintm 32) 0)]
            (connect b (zext (+ a b))))
)

(comment
(defmethod + [:uintm :int]
  [x y]
  {:pre [(pos? y)]}
  (+ x (instance (:type x) y)))

(defmethod + [:int :uintm]
  [x y]
  (+ (instance (:type y) x) y))
  )

(defrecord CompilerError [msg]
  ITyped
  (type [this] :error)
  (kind [this] :error))

(defn error [& args]
  (CompilerError. (apply print-str args)))

(defn error-unify* [f]
  "Takes a function a function and produces "
  "a new function that returns a set of errors "
  "if any of the arguments were errors, and "
  "otherwise returns the result of invoking "
  "the function normally."
  (letfn [(error? [a]
            (and (satisfies? ITyped a)
                 (= (kind a) :error)))
          (error-coll? [c]
            (and (coll? c) (every? error? c)))]
    (fn [& args]
      (if-let [errors (seq (filter
                             #(or (error? %)
                                  (error-coll? %))
                             args))]
        (flatten errors)
        (apply f args)))))

(defn promote [typeinst obj]
  (cond 
    (not (satisfies? IPromotable typeinst))
         (error typeinst "is not a valid type instance")
    (not (satisfies? ITyped obj))
        (error "Cannot promote" obj "to" typeinst)
    :else
    (dopromote typeinst obj)))

(def type-unify
  (error-unify*
    (fn [target-kind a b]
      "Takes a target kind and two other "
      "objects. Determines which of them is the "
      "target kind, then promotes the other "
      "to that type instance. Returns errors "
      "objects if any errors occurred, or if "
      "any of the objects were errors"
      (cond
        (not (satisfies? ITyped a))
        (error a "doesn't have a type")
        (not (satisfies? ITyped b))
        (error b "doesn't have a type")
        (= (kind a) target-kind)
        [a (promote (type a) b)]
        (= (kind b) target-kind)
        (let [[b a] (type-unify target-kind b a)]
          [a b])
        :else
        (error "Neither" a "nor" b "is of kind" target-kind)))))

(extend-protocol IType
  UIntM
  (kindof [this] :uintm))

(defrecord JavaLong [])
(def java-long (JavaLong.))

(extend-protocol ITyped
  Instance
  (type [this] (:type this))
  (kind [this] (kindof (:type this)))
  Long
  (type [this] java-long)
  (kind [this] :j-long))

(extend-protocol IPromotable
  JavaLong
  (dopromote [this obj]
    (if (= java-long (type obj))
      obj
      (error "Cannot promote " obj "to Long")))
  UIntM
  (dopromote [this obj]
    (cond
      (= (type obj) this) obj ;Already correct
      (= (kind obj) (kindof this)) (error
                                   "Incompatible type instances: " this
                                   "and" (type obj))
      (= (type obj) java-long) (this obj)
      :else (error "Don't know how to promote from" (type obj)))))

(defn uintm-adder [x y]
  (let [[x y] (type-unify :uintm x y)]
    ((type x) (+ (:val x) (:val y)))))

(defmethod + [:int :uintm]
  [& more] (apply uintm-adder more))
(defmethod + [:uintm :int]
  [& more] (apply uintm-adder more))
(defmethod + [:uintm :uintm]
  [& more] (apply uintm-adder more))

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

(+ ((uintm 8) 3) ((uintm 8) 4))

(+ ((uintm 8) 3) -4)
(+ 3 ((uintm 8) 4))

;(defmacro structural ...)

(comment
(structural [count ((uintm 8) 0)]
            (connect count (+ count 1)))
)
