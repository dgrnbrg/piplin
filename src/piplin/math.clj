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
(defn uintm
  "Make a new uintm type object."
  [n]
  (merge (UIntM. n)
         {:kind :uintm}))

(derive-type :uintm :bits)

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

(defn- make-core-binop-fn
  "Makes syntax for a binop using the core
  implementation for a given hierarchy key"
  [multi-op core-op key]
  `(defmethod ~multi-op [~key ~key]
     [~'x ~'y]
     (~core-op ~'x ~'y)))

(defmacro defbinop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively. Existing-types
  is a vector of hierarchy elements whose implementations
  in clojure.core should be integrated."
  [op zero existing-types]
  (let [core-op (gensym (str (name op) "core"))
        multi-op (mangle-multi-op op)
        core-methods (map #(make-core-binop-fn
                             multi-op core-op %)
                          existing-types)]
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
       ~@core-methods)))

(defbinop + 0 [:j-long])
(defbinop - 0 [:j-long])
(defbinop * 0 [:j-long])
(defbinop bit-and 0 [:j-long])
(defbinop bit-or 0 [:j-long])
(defbinop bit-xor 0 [:j-long])

(defn inc
  "Increments x"
  [x]
  (+ x 1))

(defn dec
  "Decrements x"
  [x]
  (- x 1))

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

(defn long-to-bitvec
  "Takes a long and returns a bitvec with the first n bits"
  [l n]
  (->> (iterate #(bit-shift-right % 1) l)
    (map #(bit-and 1 %))
    (take n)
    reverse
    vec))

(defn bitvec-to-long
  "Takes a bitvec and returns a long"
  [bv]
  (if (> (count bv) 64)
    (throw (RuntimeException. "bitvec too long"))
    (reduce #(bit-or (bit-shift-left %1 1) (long %2))
            0 bv)))

(defmethod promote
  :bits
  [type obj]
  (when (and (isa-type? (kindof obj) :bits)
             (not= (:n type) (get-in obj [:type :n])))
    (throw+ (error "Bit size mismatch")))
  (condp = (kindof obj)
    :bits obj
    :uintm (instance type
                     (long-to-bitvec (:val obj)
                                     (:n type)))
    Long (instance type
                      (long-to-bitvec obj
                                      (:n type)))
    (throw+ (error "Cannot promote" obj "to bits"))))

;TODO: needs check & constrain, and a better backing
;impl (bitset instead of 64 bit number)
(defrecord Bits [n])
(defn bits
  "Make a new bits type object."
  [n]
  (merge (Bits. n)
         {:kind :bits}))

(defmulti get-bits
  (fn [expr] (kindof expr))
  :hierarchy types)

(defmethod get-bits
  :default
  [expr]
  (throw+ (error "Cannot convert " expr " to bits")))

(defmethod get-bits
  :uintm
  [expr]
  (let [n (get-in expr [:type :n])
        type (bits n)]
    (if (instance? Instance expr)
      (instance type (long-to-bitvec (:val expr) n))
      (vary-meta
        {:type type
         :op :get-bits
         :args {:expr expr}}
        assoc :sim-factory [get-bits [:expr]]))))

(defn slice-impl
  "Does slicing of bits"
  [expr low high]
  (let [type (bits (- high low))]
    (instance type (-> (:val expr)
                     reverse
                     vec
                     (subvec  low high)
                     reverse
                     vec))))

;This might not need to be defn-errors
(defn bit-slice
  "Takes an expr of type bits and returns a subrange
  of the bits."
  [expr low high]
  (if-not (= (kindof expr) :bits)
    (throw+ (error "Can only slice bits, not " expr))
    (let [type (bits (- high low))]
      (if (instance? Instance expr)
        (slice-impl expr low high)
        (vary-meta
          {:type type
           :op :slice
           :low low
           :high high
           :args {:expr expr}}
          assoc :sim-factory [#(slice-impl % low high) [:expr]])))))

(defn bit-cat-impl
  ([]
   (instance (bits 0) []))
  ([bs]
   bs)
  ([b1 b2]
   (instance (bits (+ (get-in b1 [:type :n])
                      (get-in b2 [:type :n])))
             (vec (concat (:val b1) (:val b2)))))
  ([b1 b2 & more]
   (if more
     (recur (bit-cat-impl b1 b2) (first more) (next more))
     (bit-cat-impl b1 b2))))

(defn-errors bit-cat
  "Takes any number of bits and returns them concatenated"
  [& args]
  (apply bit-cat-impl args))

(defbinopimpl bit-and :bits [:uintm :j-long]
  [x y]
  (vec (map #(bit-and %1 %2)
            (:val x) (:val y))))

(defbinopimpl bit-or :bits [:uintm :j-long]
  [x y]
  (vec (map #(bit-or %1 %2)
            (:val x) (:val y))))

(defbinopimpl bit-xor :bits [:uintm :j-long]
  [x y]
  (vec (map #(bit-xor %1 %2)
            (:val x) (:val y))))

(defrecord Bool [])
(def boolean (merge (Bool.) {:kind :boolean}))

(defn mux2-impl
  [sel v1 v2]
  (when-not (= (:type v1) (:type v2))
    (throw+ (error v1 "and" v1 "are different types")))
  (println (str "muxing on " sel))
  (let [sel (promote boolean sel)]
    (if (instance? Instance sel)
      (if (:val sel) v1 v2)
      (vary-meta
        {:type (:type v1)
         :op :mux2
         :args {:sel sel
                :v1 v1
                :v2 v2}}
        assoc :sim-factory [mux2-impl [:sel :v1 :v2]]))))

(defn-errors mux2
  "Takes a boolean input and selects between
  2 values: true for the first, false for the
  second."
  [sel v1 v2]
  (mux2-impl sel v1 v2))

(defn cast
  "Converts the expr to the type."
  [type expr]
  (if (instance? Instance expr)
    (promote type expr)
    (vary-meta
      {:type type
       :op :cast
       :args {:expr expr}}
      assoc :sim-factory [(partial cast type) [:expr]])))

(defmethod promote
  :boolean
  [type obj]
  (instance boolean
    (= 1
       (cond
         (or (= true obj) (= false obj)) (if obj 1 0)
         (= (:type obj) (bits 1)) (first (:val obj))
         (and (= (kindof obj) :uintm)
              (= (get-in obj [:type :n]) 1)) (:val obj)
         :else
         (throw+ (error "Cannot promote" obj "to boolean"))))))


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
