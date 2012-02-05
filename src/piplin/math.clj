(ns piplin.math
  (:use [slingshot.slingshot :only [throw+]])
  (:use (piplin types)))

(derive-type Long :j-long)
(defmethod promote 
  :j-long
  [type obj]
  (if (isa-type? obj :j-long)
    obj
    (throw+ (error "Cannot promote" obj "to Long"))))

(defmulti constrain
  "Takes a type and a value and constrains the value to
  the type's range."
  (fn [type val] (:kind type))
  :hierarchy types)

(defmethod constrain
  :default
  [a b] b)

(defmulti check
  "Takes an instance and verifies that it meets the
  constraints of its type"
  kindof
  :hierarchy types)

(defmethod check
  :default
  [a] a)

(defrecord Instance [type val])
(defn instance
  "Creates an instance of the type with value val"
  [type val & more]
  (let [val (if (some #{:constrain} more)
              (constrain type val)
              val)
        inst (merge (Instance. type val)
               {:type type})]
    (vary-meta
      (check inst)
      assoc :sim-factory [#(apply instance
                                      type
                                      val
                                      more) []])))

(defrecord UIntM [n])
(defn uintm [n]
  "Make a new uintm type object"
  (merge (UIntM. n)
         {:kind :uintm}))

(defmethod constrain
  :uintm
  [this init-val]
  "Takes a uintm (this) and the value
  to initialize the new instance with,
  and constrains the number to be in the
  range of uintm"
  (mod init-val (bit-shift-left 1 (:n this))))

(defmethod check
  :uintm
  [inst]
  (let [n (get-in inst [:type :n])
        v (:val inst)
        maxval (dec (bit-shift-left 1 n))]
    (if (< v 0)
      (throw+ (error "uintm must be positive:" v))
      (if (> v maxval)
        (throw+ (error "uintm" n "must be less than" maxval
                       ", got:" v))
        inst))))

(defmethod promote
  :uintm
  [this obj]
  (cond
    (= (:type obj) this) obj ;Already correct
    (= (kindof obj)
       (:kind this)) (throw+ (error
                               "Incompatible type instances: " this
                               "and" (type obj)))
    (isa-type? (kindof obj) :j-long) (instance this obj)
    :else (throw+ (error "Don't know how to promote to :uintm from"
                         (type obj)))))


(defn nary-dispatch
  "Dispatching logic used by binary math operations"
  ([] ::nullary)
  ([x] (kindof x))
  ([x y] [(kindof x) (kindof y)])
  ([x y & more] ::n-ary))

(defn- mangle-multi-op [op]
  (symbol (str (name op) "multi")))

(defmacro defbinop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively."
  [op zero]
  (let [core-op (gensym (str (name op) "core"))
        multi-op (mangle-multi-op op)]
    `(do
       (defmulti ~multi-op nary-dispatch :hierarchy types)
       (defn-errors ~op [& ~'args]
         (apply ~multi-op ~'args))
       (defmethod ~multi-op ::nullary [] ~zero)
       (defmethod ~multi-op ::n-ary
         [~'x ~'y & ~'more]
         (if ~'more
           (recur (~multi-op ~'x ~'y) (first ~'more) (next ~'more))
           (~multi-op ~'x ~'y)))
       (def ~core-op (ns-resolve 'clojure.core '~op))
       (defmethod ~multi-op [:number :number]
         [~'x ~'y]
         (~core-op ~'x ~'y))
       (defmethod ~multi-op [:j-long :j-long]
         [~'x ~'y]
         (~core-op ~'x ~'y)))))

(defbinop + 0)
(defbinop - 0)
(defbinop * 0)
(defbinop bit-and 0)
(defbinop bit-or 0)
(defbinop bit-xor 0)

(defn- make-binop-impl-fn
  "Takes a kind, fntail, and an symbol for the op
  and returns a function which should be invoked
  with 2 arguments of the appropriate kind, and
  it will evaluate them if the arguments are
  immediate and it will return an AST expr if one
  or more of the arguments aren't immediates."
  [op unmangled-kw impl-name k fntail]
  `(defn ~impl-name
     [x# y#]
     (let [[x# y#] (type-unify ~k x# y#)]
       (if (and (instance? Instance x#)
                (instance? Instance y#))
         (instance (:type x#)
                   ((fn ~@fntail) x# y#)
                   :constrain)
         (vary-meta
           {:type (:type x#)
            :op ~unmangled-kw
            :args {:lhs x#
                   :rhs y#}}
           assoc :sim-factory [~op [:lhs :rhs]])))))

(defmacro defbinopimpl
  "Defines implementation of a binop on a piplin kind.
  Requires the kind to attempt to unify to, the list
  of types which can be promoted to the kind, and an
  fntail that takes 2 arguments and returns the result.
  The implementation will return an error if the
  unification failed."
  [op k bases & fntail]
  (let [unmangled-kw (keyword (name op))
        op (mangle-multi-op op)
        impl-name (gensym (str (name op) (name k)))
        impl-body (make-binop-impl-fn op unmangled-kw
                                      impl-name k fntail)
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
  (+ (:val x) (:val y)))

(defbinopimpl - :uintm [:j-long]
  [x y]
  (- (:val x) (:val y)))

(defbinopimpl * :uintm [:j-long]
  [x y]
  (* (:val x) (:val y)))

(defbinopimpl bit-and :uintm [:j-long]
  [x y]
  (bit-and (:val x) (:val y)))

(defbinopimpl bit-or :uintm [:j-long]
  [x y]
  (bit-or (:val x) (:val y)))

(defbinopimpl bit-xor :uintm [:j-long]
  [x y]
  (bit-xor (:val x) (:val y)))

(comment
  How to get the combinational function for ops.

  We have an op & args, and we need to look up
  the function that does this thing and the way
  to bind the args. Then we recur on the args.
  )

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

;(+ 42 3)
;(+ ((uintm 8) 3) ((uintm 8) 4))

;(+ ((uintm 8) 3) -4)
;(+ 3 ((uintm 8) 4))

(comment
  TODO
  - error reporting (file/lineno)
  - implement sintm, sints, uints, etc. (e type is hard)
  - design AST
  - implement sim function, ast, and
    verilog module binding macro(s)
  - implement simulation with syn/ack)

(comment
;  The following is converted to AST now:
(pprint (module [:outputs
                        [c (instance (uintm 8) 3)]]
                       [c (+ 1 c)]))
)
