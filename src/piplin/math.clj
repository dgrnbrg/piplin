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


(defn instance [type val]
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
  (UIntM. n))

(defn nary-dispatch
  ([] ::nullary)
  ([x] (kind x))
  ([x y] [(kind x) (kind y)])
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

(defn uintm-adder [x y]
  (let [result (type-unify :uintm x y)]
    (if-not (error? result)
      (let [[x y] result]
        ((type x) (+ (:val x) (:val y))))
      result)))

(defmethod + [:j-long :j-long] [x y] (clojure.core/+ x y))
(defmethod + [:number :number] [x y] (clojure.core/+ x y))
(defmethod + [:uintm :uintm] [x y] (uintm-adder x y))
(defmethod + [:j-long :uintm]
  [& more] (apply uintm-adder more))
(defmethod + [:uintm :j-long]
  [& more] (apply uintm-adder more))
(defmethod + [:uintm :uintm]
  [& more] (apply uintm-adder more))

;successfully blocked
;(+ ((uintm 3) 3) ((uintm 4) 3))

;(+ 42 3)
;(+ ((uintm 8) 3) ((uintm 8) 4))

;(+ ((uintm 8) 3) -4)
;(+ 3 ((uintm 8) 4))
