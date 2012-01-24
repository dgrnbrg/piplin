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

;TODO: port all to multimethods for easier development
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
    (let [maxval (dec (bit-shift-left 1 n))]
      (loop [init-val init-val]
        (if (neg? init-val)
          (recur (- maxval init-val))
          (instance this (bit-and init-val maxval))))))
    ;(instance this init-val))
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

(defbinopimpl - :uintm [:j-long]
  [x y]
  ((type x) (- (:val x) (:val y))))

(defbinopimpl * :uintm [:j-long]
  [x y]
  ((type x) (bit-and (* (:val x) (:val y) ))))

(defbinopimpl bit-and :uintm [:j-long]
  [x y]
  ((type x) (bit-and (:val x) (:val y))))

(defbinopimpl bit-or :uintm [:j-long]
  [x y]
  ((type x) (bit-or (:val x) (:val y))))

(defbinopimpl bit-xor :uintm [:j-long]
  [x y]
  ((type x) (bit-xor (:val x) (:val y))))

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

;(+ 42 3)
;(+ ((uintm 8) 3) ((uintm 8) 4))

;(+ ((uintm 8) 3) -4)
;(+ 3 ((uintm 8) 4))

(comment
  TODO
  - error reporting (file/lineno)
; - enforce boundaries of uintm [0 ..< (2 ^ n)]
  - implement other operations on uintm
  - implement sintm, sints, uints, etc. (e type is hard)
  - design AST
  - implement sim function, ast, and
    verilog module binding macro(s)
  - implement simulation with syn/ack)
