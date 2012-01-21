(ns piplin.math
  (:use (piplin types)))

(declare java-long)

(defrecord JavaLong []
  IPromotable
  (dopromote [this obj]
    (if (= java-long (type obj))
      obj
      (error "Cannot promote " obj "to Long"))))

(def java-long (JavaLong.))

(extend-protocol ITyped
  Long
  (type [this] java-long)
  (kind [this] :j-long))

;TODO: enforce that all instances are positive
(defrecord Instance [type val]
  ITyped
  (type [this] type)
  (kind [this] (kindof type)))

(defn instance
  "Creates an instance of the type with value val"
  [type val]
  (Instance. type val))

(defrecord UIntM [n]
  clojure.lang.IFn
  (invoke [this init-val]
    (instance this init-val))
  IType
  (kindof [this] :uintm)
  IPromotable
  (dopromote [this obj]
      (cond
        (= (type obj) this) obj ;Already correct
        (= (kind obj) (kindof this)) (error
                                       "Incompatible type instances: " this
                                       "and" (type obj))
        (= (type obj) java-long) (this obj)
        :else (error "Don't know how to promote from" (type obj)))))

(defn uintm [n]
  "Make a new uintm type object"
  (UIntM. n))

(defn nary-dispatch
  "Dispatching logic used by binary math operations"
  ([] ::nullary)
  ([x] (kind x))
  ([x y] [(kind x) (kind y)])
  ([x y & more] ::n-ary))

(defmacro defbinop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively."
  [op zero]
  (let [core-op (gensym (str (name op) "core"))]
    `(do
       (defmulti ~op nary-dispatch)
       (defmethod ~op ::nullary [] ~zero)
       (defmethod ~op ::n-ary
         [~'x ~'y & ~'more]
         (if ~'more
           (recur (~op ~'x ~'y) (first ~'more) (next ~'more))
           (~op ~'x ~'y)))
       (def ~core-op (ns-resolve 'clojure.core '~op))
       (defmethod ~op [:number :number]
         [~'x ~'y]
         (~core-op ~'x ~'y))
       (defmethod ~op [:j-long :j-long]
         [~'x ~'y]
         (~core-op ~'x ~'y)))))

(defbinop + 0)
(defbinop - 0)
(defbinop * 0)
(defbinop bit-and 0)
(defbinop bit-or 0)
(defbinop bit-xor 0)

(defmacro defbinopimpl
  "Defines implementation of a binop on a piplin kind.
  Requires the kind to attempt to unify to, the list
  of types which can be promoted to the kind, and an
  fntail that takes 2 arguments and returns the result.
  The implementation will return an error if the
  unification failed."
  [op k bases & fntail]
  (let [impl-name (gensym (str (name op) (name k)))
        impl-body `(defn ~impl-name
                     [x# y#]
                     (let-safe [[x# y#] (type-unify ~k x# y#)]
                       ((fn ~@fntail) x# y#)))
        k-bases (map #(vector k %) bases)
        dispatches (concat k-bases
                           (map reverse k-bases)
                           [[k k]])
        bodies (map (fn [[a b]]
                      `(defmethod ~op [~a ~b]
                         [~'x ~'y]
                         (~impl-name ~'x ~'y)))
                    dispatches)]
    `(do
       ~impl-body
       ~@bodies)))

(defbinopimpl + :uintm [:j-long]
  [x y]
  ((type x) (+ (:val x) (:val y))))

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

;(+ 42 3)
;(+ ((uintm 8) 3) ((uintm 8) 4))

;(+ ((uintm 8) 3) -4)
;(+ 3 ((uintm 8) 4))

(comment
  TODO:
  - error reporting (file/lineno)
  - enforce boundaries of uintm [0 ..< 2**n]
  - implement other operations on uintm
  - implement sintm, sints, uints, etc. (e type is hard)
  - design AST
  - implement sim function, ast, and
    verilog module binding macro(s)
  - implement simulation with syn/ack)

(comment
  Synthesizable fragments are either expressions or structures.

  Expressions either are immediates or are values.

  All expressions have types and error info.

  The details of a value expression are: the name of the verilog
  module it links to & that module's portspec, the function
  which implements its functionality, its walkable
  arguments, and its other arguments.

  The details of an immediate expression are: the piplin
  instance of it, and the function that converts it
  to verilog. Instead of a piplin instance, it could instead
  have a port usage.

  The details of a structure are the inputs (themselves exprs),
  the output/feedback regs (with exprs whose termini must be
  immediates)
  )

(comment
  Simulation:
  We have a datastructure containing the value of every register
  We run all the functions to get the new values of registers on
  which there is data available.
  Repeat.
  )

(comment
  Structures serve as barriers
  i.e. it produces a result no faster than the longest pipline in it.

  Many primitives are only valid when their inputs are valid. How do
  we communicate validity? With 

  Suppose not: does it become hard to reason about how the data flows?
  Needs a priority encoder/round robin primitive--when 2 things could
  happen, pick one.

  I am thinking now to have structural blocks serve as synchronization
  points, and then provide the priority/round-robin bypass/merging
  combinators.
  )

(comment
  We can propagate metadata via a macro:
  enforces that metadata threading points are in the same lexical scope

  (let ({:keys [:pipe :key :key2]}) (metapipe {:key val :key2 val}
                  (pipeline (exprs key) key2))
    body...)
  )

(comment
  I'm trying to figure out how to write code w/ valid and stall bits
  effectively programmed. This example is for an ALU w/ an intmath
  pipeline that computes +, -, *, and /, and a cordic processor that
  computes sin and cos.)

(defn prioritymerge [x y]
  (if-not (= (type x) (type y))
    (error "Can only merge data of same type")
    ;if x is valid, returns valid x
    ;if y is valid and x isn't, returns valid y
    ;if x and y are invalid, returns invalid
    (if (valid? x) x y)))

(defn mkalu [data op]
  (structural [:in data op :out results (aluresult nil)]
    (let [intmath-op (isany? op #{:+ :- :* :/})
          cordic-op (bit-not intmath-op)
          ;isany? and bit-not can be used in guarded-with clauses
          ;since they can be synthesized as combinational logic
          ;noncombinational logic cannot be used as the guard
          intpipe (guarded-with intmath-op
                    (mkintmath op data))
          cordicpipe (guarded-with cordic-op
                       (mkcordic op data))]
      ;merge functions do arbitration
      (connect results
               (priority-merge intpipe cordicpipe))
      (comment
        How should backpressure work? Here's my hypothesis:

        If the output node is asserting stall backwards, then we
        must not produce more results. Do this by having the stall
        propagate through the pipeline backward. We'll need to
        write the logic that explains when we're stalled to our
        inputs, however.

        )
      (backpressure (or (and intmath-op (ready? intpipe))
                        (and cordic-op (ready? cordic-pipe)))))))
