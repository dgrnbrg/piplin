(ns piplin.math
  (:use [slingshot.slingshot])
  (:use [clojure.set :only [map-invert intersection difference]])
  (:require [clojure.set])
  (:require [clojure.core :as clj])
  (:use (piplin types))
  (:use [piplin.modules :only [connect]]))

(extend-protocol ITyped
  nil
  (typeof [this] (anontype :null))
  (value [this] nil)
  (pipinst? [this] true)
 #_ (comment clojure.lang.IPersistentMap 
  (typeof [this] (anontype :map)) 
  (value [this] this) 
  (pipinst? [this] true)) 
  )

(derive-type clojure.lang.Keyword :piplin-type)
(derive-type java.lang.Boolean :piplin-type)
(derive-type java.lang.Long :piplin-type)
(derive-type java.lang.Integer :piplin-type)
(derive-type java.lang.Short :piplin-type)
(derive-type java.lang.Byte :piplin-type)
(derive-type java.lang.Float :piplin-type)
(derive-type java.lang.Double :piplin-type)

(extend-protocol ITyped
  clojure.lang.Keyword
  (typeof [this] (anontype :keyword))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Boolean
  (typeof [this] (anontype :boolean))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Long
  (typeof [this] (anontype :j-long))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Integer
  (typeof [this] (anontype :j-int))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Short
  (typeof [this] (anontype :j-short))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Byte
  (typeof [this] (anontype :j-byte))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Float
  (typeof [this] (anontype :j-num))
  (value [this] this)
  (pipinst? [this] true)
  java.lang.Double
  (typeof [this] (anontype :j-num))
  (value [this] this)
  (pipinst? [this] true))

(defn cast
  "Converts the expr to the type."
  [type expr]
  (if (typeof expr)
    (cond
      (clj/= (typeof expr) type)
      expr
      (pipinst? expr)
      (promote type expr)
      (-> expr meta :distribute)
      ((-> expr meta :distribute) type)
      :else
      (mkast type :cast [expr] (partial cast type)))
    (promote type expr)))

;TODO: why doesn't this work if I make it :j-integra?
(defmethod promote 
  :j-integral
  [type obj]
  (condp isa-type? (kindof obj)
    :j-integral (clojure.core/long obj)
    :uintm (value obj)
    (throw+ (error "Cannot promote" obj "to Long"))))

(derive-type :j-integral :j-num)

(derive-type Double :j-num)
(derive-type Float :j-num)

(derive-type Byte :j-byte)
(derive-type Short :j-short)
(derive-type Integer :j-int)
(derive-type Long :j-long)

(derive-type :j-byte :j-integral)
(derive-type :j-int :j-integral)
(derive-type :j-short :j-integral)
(derive-type :j-long :j-integral)

(defpiplintype UIntM [n])
(defn uintm
  "Make a new uintm type object."
  [n]
  (merge (UIntM. n)
         {:kind :uintm}))

(defmethod constrain
  :uintm
  [this init-val]
  "Takes a uintm (this) and the value
  to initialize the new instance with,
  and constrains the number to be in the
  range of uintm"
  (bit-and init-val (dec (bit-shift-left 1 (:n this)))))

(defmethod check
  :uintm
  [inst]
  (let [n (-> inst typeof :n)
        v (value inst)
        maxval (dec (bit-shift-left 1 n))]
    (when (neg? v)
      (throw+ (error "uintm must be positive:" v)))
    (when (> v maxval)
      (throw+ (error "uintm" n "must be less than" maxval
                     ", got:" v))))
  inst)

(defmethod promote
  :uintm
  [this obj]
  (cond
    (clj/= (typeof obj) this) obj ;Already correct
    (clj/= (kindof obj)
       (:kind this)) (throw+
                       (error
                         "Incompatible type instances: " this
                         "and" obj))
    (isa-type? :j-integral (kindof obj)) (instance
                                           this
                                           (promote (anontype :j-integral) ;TODO: this should be j-long
                                                    obj))
    :else (throw+ (error "Don't know how to promote to :uintm from"
                         (typeof obj)))))

(defn piplin-clojure-dispatch
  "Returns the kind of the piplin type or
  :use-core-impl if it has no piplin type"
  [x]
  (try+
    (kindof x)
    (catch piplin.types.CompilerError ce
      :use-core-impl)))

(defn binary-dispatch
  "Simplified binary dispatch logic."
  [x y]
  [(piplin-clojure-dispatch x)
   (piplin-clojure-dispatch y)])

(defn nary-dispatch
  "Dispatching logic used by binary math operations"
  ([] ::nullary)
  ([x] (piplin-clojure-dispatch x))
  ([x y] (binary-dispatch x y))
  ([x y & more] ::n-ary))

(defn- make-core-unop-fn
  "Makes syntax for a unop using the core
  implementation for a given hierarchy key"
  [op key]
  `(defmethod ~op ~key
     [~'x]
     ('~(ns-resolve 'clojure.core op) ~'x)))

(defn- make-core-binop-fn
  "Makes syntax for a binop using the core
  implementation for a given hierarchy key"
  [op key]
  `(defmethod ~op [~key ~key]
     [~'x ~'y]
     ('~(ns-resolve 'clojure.core op) ~'x ~'y)))

(defmacro def-n-ary-binop
  "Defines a generic function for a binary operation
  on numeric types. Uses the function in clojure/core
  to provide implementations for Numbers. Handles
  nullary and n-ary invocations by returning zero and
  the left-associative folds, respectively. Existing-types
  is a vector of hierarchy elements whose implementations
  in clojure.core should be integrated."
  [op zero existing-types]
  (let [core-methods (map #(make-core-binop-fn
                             op %)
                          existing-types)
        core-unary (map #(make-core-unop-fn
                           op %)
                        existing-types)]
    `(do
       (defmulti ~op nary-dispatch :hierarchy types)
       (defmethod ~op ::nullary [] ~zero)
       (defmethod ~op ::n-ary
         [~'x ~'y & ~'more]
         (if (seq ~'more)
           (recur (~op ~'x ~'y) (first ~'more) (next ~'more))
           (~op ~'x ~'y)))
       ~@core-methods
       ~@core-unary)))

(defmacro def-binary-binop
  "Like def-n-ary-binop, but without nullary or left-associative
  folds."
  [op existing-types]
  (let [core-methods (map #(make-core-binop-fn
                             op %)
                          existing-types)]
    `(do
       (defmulti ~op binary-dispatch :hierarchy types)
       ~@core-methods)))

(def-n-ary-binop + 0 [:j-num])
(def-n-ary-binop - 0 [:j-num])
(def-n-ary-binop * 0 [:j-num])
(def-n-ary-binop bit-and 0 [:j-num])
(def-n-ary-binop bit-or 0 [:j-num])
(def-n-ary-binop bit-xor 0 [:j-num])

(defn inc
  "Increments x"
  [x]
  (+ x 1))

(defn dec
  "Decrements x"
  [x]
  (- x 1))

(def-binary-binop > [:j-num])
(def-binary-binop < [:j-num])
(def-binary-binop >= [:j-num])
(def-binary-binop <= [:j-num])

(defn not
  [x]
  (if (typeof x)
    (if (pipinst? x)
      (clojure.core/not x) 
      (mkast (anontype :boolean) :not [x] not))
    (clojure.core/not x)))

(defmulti = nary-dispatch :hierarchy types)
(defmethod = :use-core-impl [x]
  (clj/= x))
(defmethod = [:use-core-impl :use-core-impl] [x y]
  (clj/= x y))
(defmethod = :default [x y]
  (clojure.core/cond
    (and (nil? x) (nil? y))
    true
    (or (nil? x) (nil? y))
    false
    (not (or (instance? piplin.types.ASTNode x)
             (instance? piplin.types.ASTNode y)))  
    (clojure.core/= x y)
    (and (pipinst? x) (pipinst? y))
    (clojure.core/= (value x) (value y))
    :else
    (mkast (anontype :boolean) := [x y] =)))
(defmethod = ::n-ary
  [x y & more]
  (if (= x y)
    (if (seq more)
      (recur x (first more) (rest more))
      true)
    false))

(defn not=
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more] (not (apply = x y more))))

(defn- make-binop-impl-fn
  "Takes a kind, fntail, and an symbol for the op
  and returns a function which should be invoked
  with 2 arguments of the appropriate kind, and
  it will evaluate them if the arguments are
  immediate and it will return an AST expr if one
  or more of the arguments aren't immediates."
  [op unmangled-kw k fntail]
  `(defmethod ~op [~k ~k]
     [x# y#]
     (if (and (pipinst? x#)
              (pipinst? y#))
       (instance (typeof x#)
                 ((fn ~@fntail) x# y#)
                 :constrain)
       (let [~'lhs x# ~'rhs y#]
         (->
           (mkast (typeof x#)
                ~unmangled-kw
                [~'lhs ~'rhs] ~op)
           (assoc-dist-fn
             #(~op (cast % ~'lhs) (cast % ~'rhs))))))))

(defn make-binop-explict-coercions
  "Takes a kind and a vector of kinds and returns
  a list of syntax for defmethods that will invoke
  the method for [kind kind] if kind is one of the
  arguments and any element of the vector is the
  other argument's kind."
  [op k bases]
  (let [k-bases (map #(vector k %) bases)
        dispatches (concat k-bases
                           (map reverse k-bases))]
    (map (fn [[a b]]
           `(defmethod ~op [~a ~b]
              [~'x ~'y]
              (let [[~'x  ~'y] (type-unify ~k ~'x ~'y)]
                (~op ~'x ~'y))))
         dispatches)))

(defmacro defcoercions
  "Used to construct automatic coercion for binop types
  when they don't follow the more common return value
  pattern."
  [op k bases]
  `(do ~@(make-binop-explict-coercions op k bases)))

(defmacro defbinopimpl
  "Defines implementation of a binop on a piplin kind.
  Requires the kind to attempt to unify to, the list
  of types which can be promoted to the kind, and an
  fntail that takes 2 arguments and returns the result.
  The implementation will return an error if the
  unification failed."
  [op k bases & fntail]
  (let [unmangled-kw (keyword (name op))
        impl-body (make-binop-impl-fn op unmangled-kw
                                      k fntail)
        bodies (make-binop-explict-coercions op k bases)]
    `(do
       ~impl-body
       ~@bodies)))

(defbinopimpl + :uintm [:j-integral]
  [x y]
  (+ (value x) (value y)))

(defbinopimpl - :uintm [:j-integral]
  [x y]
  (- (value x) (value y)))

(defbinopimpl * :uintm [:j-integral]
  [x y]
  (* (value x) (value y)))

(defmethod > [:uintm :uintm]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (> (value x) (value y))
    (mkast (anontype :boolean) :> [x y] >)))
(defcoercions > :uintm [:j-integral])

(defmethod >= [:uintm :uintm]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (>= (value x) (value y))
    (mkast (anontype :boolean) :>= [x y] >=)))
(defcoercions >= :uintm [:j-integral])

(defmethod < [:uintm :uintm]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (< (value x) (value y))
    (mkast (anontype :boolean) :< [x y] <)))
(defcoercions < :uintm [:j-integral])

(defmethod <= [:uintm :uintm]
  [x y]
  (if (and (pipinst? x) (pipinst? y))
    (<= (value x) (value y))
    (mkast (anontype :boolean) :<= [x y] <=)))
(defcoercions <= :uintm [:j-integral])

(defcoercions = :uintm [:j-integral])

(defcoercions = :enum [:keyword])

(defbinopimpl bit-and :uintm [:j-integral]
  [x y]
  (bit-and (value x) (value y)))

(defbinopimpl bit-or :uintm [:j-integral]
  [x y]
  (bit-or (value x) (value y)))

(defbinopimpl bit-xor :uintm [:j-integral]
  [x y]
  (bit-xor (value x) (value y)))

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
  (when-let [n (-> obj typeof :n)]
    (when-not (clj/= (:n type) n)
      (throw+ (error "Bit size mismatch"))))
  (condp isa-type? (kindof obj)
    :bits obj
    :uintm (instance type
                     (long-to-bitvec (value obj)
                                     (:n type)))
    :j-integral (instance type
                          (long-to-bitvec obj
                                          (:n type)))
    (throw+ (error "Cannot promote" obj "to bits"))))

(defpiplintype Bits [n])
(defn bits
  "Make a new bits type object."
  [n]
  (merge (Bits. n)
         {:kind :bits}))

(defmethod check
  :bits
  [inst]
  (let [n (-> inst typeof :n)
        v (value inst)]
    (when-not (and (vector? v)
                   (every? #{0 1} v))
      (throw+ (error
                "bits must be a vector of 0s and 1s:" v)))
    (when-not (clj/= (count v) n)
      (throw+ (error "bit vector must be of length" n (count v)))))
  inst)

(defmulti from-bits
  (fn [type bits] (:kind type))
  :hierarchy types)

(defmethod from-bits
  :default
  [type bits]
  (throw+ (error "No way to convert bits to " type)))

(defmethod from-bits
  :uintm
  [type bits]
  (bitvec-to-long bits))

(defmethod from-bits
  :bits
  [type bits]
  bits)

(defmulti bit-width-of
  ;TODO: error below never gets thrown
  ;needs a test + fix
;  #(if (pipinst? %)
 ;    (throw+ (error "Should be a type: " %))
     :kind
;     )
  :hierarchy types)

(defmethod bit-width-of
  :bits
  [bits]
  (:n bits))

(defmulti get-bits
  kindof
  :hierarchy types)

(defn serialize
  "Gets the bits representation of its argument. Supports AST frags."
  [expr]
  (when-not (typeof expr)
    (throw+ (error "Only piplin ITyped expressions can be serialized")))
  (let [n (bit-width-of (typeof expr))
        type (bits n)]
    (if (pipinst? expr)
      (instance type (get-bits expr)) 
      (mkast type :serialize [expr] serialize))))

(defn deserialize
  "Takes a type and bits, and converts the bits
  to the given type."
  [type bits]
  (let [bits-type (typeof bits)
        n (:n bits-type)]
    (when-not (clj/= (kindof bits) :bits)
      (throw+ (error bits "must be of kind :bits")))
    (when-not (clj/= n (bit-width-of type))
      (throw+ (error type "has bit width" (bit-width-of type) "but should be" n)))
    (if (pipinst? bits)
      (cast type (from-bits type (value bits)))
      (mkast type :deserialize [bits] (partial deserialize type)))))

(defmethod bit-width-of
  :default
  [expr]
  (throw+ (error "Don't know how to get bit width of" expr)))

(defmethod get-bits
  :default
  [expr]
  (throw+ (error "Cannot convert " expr " to bits")))

(defmethod bit-width-of
  :uintm
  [type]
  (:n type))

(defmethod get-bits
  :uintm
  [expr]
  (let [n (bit-width-of (typeof expr))]
      (long-to-bitvec (value expr) n)))

(defn slice-impl
  "Does slicing of bits"
  [expr low high]
  (let [type (bits (- high low))]
    (instance type (-> (value expr)
                     reverse
                     vec
                     (subvec low high)
                     reverse
                     vec))))

;This might not need to be defn-errors
(defn bit-slice
  "Takes an expr of type bits and returns a subrange
  of the bits."
  [expr low high]
  (if-not (clj/= (kindof expr) :bits)
    (throw+ (error "Can only slice bits, not " expr))
    (let [type (bits (- high low))]
      (if (pipinst? expr)
        (slice-impl expr low high)
        (mkast type :slice [expr low high] slice-impl)))))

(defn bit-cat
  ([]
   (instance (bits 0) []))
  ([bs]
   bs)
  ([b1 b2]
   (instance (bits (+ (-> b1 typeof :n) (-> b2 typeof :n)))
             (vec (concat (value b1) (value b2)))))
  ([b1 b2 & more]
   (if more
     (recur (bit-cat b1 b2) (first more) (next more))
     (bit-cat b1 b2))))

(defbinopimpl bit-and :bits [:uintm :j-integral]
  [x y]
  (vec (map #(bit-and %1 %2)
            (value x) (value y))))

(defbinopimpl bit-or :bits [:uintm :j-integral]
  [x y]
  (vec (map #(bit-or %1 %2)
            (value x) (value y))))

(defbinopimpl bit-xor :bits [:uintm :j-integral]
  [x y]
  (vec (map #(bit-xor %1 %2)
            (value x) (value y))))

(defn mux2-impl
  [sel v1 v2]
  (when-not (clj/= (typeof v1) (typeof v2))
    (throw+ (error v1 "and" v2 "are different types" (typeof v1) (typeof v2))))
  (let [sel (cast (anontype :boolean) sel)]
    (if (pipinst? sel)
      (if sel v1 v2)
      (->
        (mkast (typeof v1) :mux2 [sel v1 v2] mux2-impl)
        (assoc-dist-fn
          #(mux2-impl sel (cast % v1) (cast % v2)))))))  

(defn mux2-helper
  [sel v1-thunk v2-thunk]
  (let [v1-connections (atom {})
        v2-connections (atom {})
        v1-connect #(swap! v1-connections assoc %1 %2)
        v2-connect #(swap! v2-connections assoc %1 %2)
        v1 (binding [piplin.modules/connect v1-connect]
             (v1-thunk))
        v2 (binding [piplin.modules/connect v2-connect]
             (v2-thunk))]
    (cond
      ;TODO: this code breaks if set isn't called below
      ;I don't understand why/don't think that should happen
      (clj/not= (set (keys @v1-connections)) (set (keys @v2-connections)))
      (throw+ (error "Must have the same connections on both parts"))
      (seq @v1-connections)
      (->> @v1-connections
        keys
        (map #(connect % (mux2-impl sel
                                    (get @v1-connections %) 
                                    (get @v2-connections %))))
        dorun)
      :else,
      (mux2-impl sel v1 v2))))

(defmacro mux2
  [sel v1 v2]
  `(mux2-helper ~sel (fn [] ~v1) (fn [] ~v2)))

(defmethod promote
  :boolean
  [type obj]
  (clj/= 1
     (cond
       (clj/= (typeof obj) type) (if obj 1 0)
       (or (clj/= true obj) (clj/= false obj)) (if obj 1 0)
       (clj/= (typeof obj) (bits 1)) (first (value obj))
       (and (clj/= (kindof obj) :uintm)
            (clj/= (-> obj typeof :n) 1)) (value obj)
       :else
       (throw+ (error "Cannot promote" obj "to boolean")))))

(defmethod bit-width-of
  :boolean
  [type]
  1)

(defmethod get-bits
  :boolean 
  [expr]
  (if (value expr) [1] [0]))

(defmethod from-bits
  :boolean
  [type bits]
  (= (first bits) 1))

(defn trace
  "Takes a function and an expr and returns an
  expr whose value is the expr, but when simulated
  will run the given function for its side effects
  every time this value is used."
  [f expr]
  (mkast (typeof expr) :noop [expr] #(do (f %) %)))

(defn pr-trace
  "Prints the args followed by the traced value."
  [& args]
  (trace #(println
            (apply print-str (concat (butlast args) [%])))
         (last args)))

(defn log2
  "Log base 2"
  [v]
  (let [log2v (-> (value v)
                Math/log
                (/ (Math/log 2))
                (+ 0.5)
                int)]
    (promote (typeof v) log2v)))

(defpiplintype PiplinEnum [keymap])
(defn enum
  "Takes a collection of keywords or a map of
  keywords to bits and returns it as an enum
  type."
  [coll & more]
  (let [allow-dups (some #{:allow-dups} more)]
    (if (set? coll)
      (if (every? keyword? coll)
        (let [n (count coll)
              logn (log2 n)]
          (merge (PiplinEnum.
                   (zipmap coll
                           (map #((bits logn) (long-to-bitvec % logn))
                                (iterate inc 0))))
                 {:kind :enum}))
        (throw+ (error
                  "Set must be made of only kewords"
                  (remove keyword? coll))))
      (if (map? coll)
        (cond
          (some #(not= (kindof %) :bits) (vals coll))
          (throw+ (error "Maps values must all be bits")) 
          (some #(clj/not= (-> (seq coll)
                             first
                             val
                             typeof
                             bit-width-of)
                           (bit-width-of (typeof %)))
                (vals coll))
          (throw+ (error
                    "Map's values must be same bit width")) 
          (some #(not (keyword? %)) (keys coll))
          (throw+ (error "Map's keys must be keywords")) 
          (clj/and
            (clj/not allow-dups)
            (clj/not= (count (vals coll))
                      (count (distinct
                               (vals coll)))))
          (throw+ (error "Enum keys must be distinct"))
          :else
          (merge (PiplinEnum. coll)
                 {:kind :enum}))))))

(defmethod promote
  :enum
  [type obj]
  (cond
    (clj/= (typeof obj) type) obj
    (and (keyword? obj)
         (obj (:keymap type))) (instance type obj)
    :else
    (throw+ (error "Cannot promote" obj "to" type))))

(defmethod from-bits
  :enum
  [type bits]
  (let [lookup (reduce (fn [m [k v]]
                         (assoc m (value v) k))
                       {}
                       (:keymap type))
        k (get lookup bits)]
    (cond
      (nil? k)
      (throw+ (error bits "is not a valid element of the enum" (typeof bits)))
      :else
      k)))

(defmethod bit-width-of
  :enum
  [type]
  (-> type 
    :keymap
    vals
    first
    typeof
    bit-width-of))

(defmethod get-bits
  :enum
  [expr]
  (let [type (-> (typeof expr)
               :keymap
               seq
               first
               val
               typeof)]
    (value ((value expr) (:keymap (typeof expr))))))

(defmethod check
  :enum
  [inst]
  (let [keymap (-> inst typeof :keymap)
        v (value inst)]
    (when-not (some #{v} (keys keymap))
      (throw+ (error v "is not in" (keys keymap)))))
  inst)

(defn sym-diff
  "symmetric different of 2 sets"
  [s1 s2]
  (clojure.set/union
    (difference s1 s2)
    (difference s2 s1)))

(defn get
  "Gets the given key from the bundle"
  [bund key]
  (condp = (piplin-clojure-dispatch bund) 
    :use-core-impl 
    (clj/get bund key) 
    :bundle 
    (if (pipinst? bund)
      (clojure.core/get (value bund) key)
      (mkast (get-in (typeof bund) [:schema key])
             :bundle-key
             [bund key]
             get))
    (throw+ (error "Don't know how to get from" bund))))

(defn assoc
  "Returns a new bundle whose key k is equal to v.
  All other keys are unchanged."
  ([bund k v]
   (condp = (piplin-clojure-dispatch bund)
     :use-core-impl
     (clj/assoc bund k v)
     :bundle
     (if (pipinst? bund)
       ((typeof bund) (clj/assoc (value bund) k v))
       (-> (mkast (typeof bund)
                  :bundle-assoc
                  [bund key val]
                  assoc)
         (assoc-dist-fn
           #(assoc bund k
                   (cast (-> % :schema k)
                         v)))))
     (throw+ (error "Don't know how to assoc" bund))))      
  ([bund k v & kvs]
   (if (seq kvs)
     (recur (assoc bund k v)
            (first kvs)
            (second kvs)
            (nnext kvs))
     (assoc bund k v))))

(defn assoc-in
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defpiplintype Bundle [schema])
(defn bundle
  "Takes a map of keys to types and returns
  a bundle type with that schema."
  [schema]
  (cond
    (not (map? schema))
    (throw+ (error "Schema must be a map"))
    (some (comp not keyword?) (keys schema))
    (throw+ (error "keys must be keywords"))
    (some #(not (or (:kind %) (class? %))) (vals schema))
    (throw+ (error "values must be piplin or java types:" schema))
    :else
    (merge (Bundle. schema)
           {:valAt get
            :kind :bundle})))

(defmethod promote
  :bundle
  [type obj]
  (cond
    (clj/= (typeof obj) type) obj
    (map? obj)
    (let  [schema (:schema type)]
      (when-let [bad (seq (sym-diff (set (keys schema))
                                    (set (keys obj))))]
        (throw+ (error "These keys either didn't have a value"
                       "or aren't part of the schema:" bad)))
      (let [casted-obj (apply conj {}
                              (map (fn [[k v]]
                                     [k (cast v (get obj k))]) 
                                   schema))] 
        (if (every? pipinst? (vals obj))
          (instance type casted-obj :constrain) 
          (mkast-explicit-keys type :make-bundle
                               (keys obj) casted-obj 
                               (fn [& args]
                                 (promote type
                                          (zipmap (keys obj)
                                                  args)))))))
    :else
    (throw+ (error "Cannot promote" obj "to" type))))

(defmethod bit-width-of
  :bundle
  [type]
  (->> type 
    :schema
    vals
    (map bit-width-of)
    (reduce +)))

(defmethod get-bits
  :bundle
  [expr]
  (let [schema-ks (keys (:schema (typeof expr)))
        bundle-inst (value expr)
        ordered-vals (map (partial get bundle-inst)
                          schema-ks)]
    (->> ordered-vals
      (map serialize)
      (apply bit-cat)
      value)))

(defmethod from-bits
  :bundle
  [type bs]
  (let [schema (:schema type)
        subtypes (vals schema)
        sizes (map bit-width-of subtypes)
        offsets (reductions + 0 sizes)
        slice (partial subvec bs)
        slices (map slice offsets (map + sizes offsets))
        insts (map from-bits subtypes slices)]
    (zipmap (keys schema) insts)))

(defmethod constrain
  :bundle
  [type val] 
  (let [schema (:schema type)]
    (->> val
      (mapcat (fn [[k v]]
                [k (instance (k schema) (value v) :constrain)]))
      (apply hash-map))))

(defmethod check 
  :bundle
  [inst]
  (let [schema (:schema (typeof inst))
        correct (map (fn [[k v]]
                       (and (isa-type? (k schema)
                                       (typeof v))
                            (pipinst? v)))
                         (value inst))]
    (when-let [bad (seq (sym-diff (set (keys schema))
                                  (set (keys (value inst)))))]
      (throw+ (error "These keys either didn't have a value"
                     "or aren't part of the schema")))
    (when-not (every? identity correct)
      (throw+ (error (value inst)
                     "doesn't match schema"
                     schema)))) 
  inst)

(defn cond-helper [predicates thunks]
  (if (some #(instance? piplin.types.ASTNode %) predicates)
    (let [last-pred (last predicates)
          predicates (butlast predicates)
          mux-tree (->> thunks
                     (interleave predicates)
                     (partition 2)
                     reverse
                     (reduce (fn [prev [p t]]
                               (fn [] (mux2-helper p t prev))) 
                             (last thunks)))]
      (if (clj/= last-pred :else)
        (mux-tree) 
        (throw+ (error "Must include :else in simulated cond"))))
    (let [thunk (->> thunks
                  (interleave predicates)
                  (partition 2)
                  (keep (fn [[p t]] (if p t nil)))
                  first)]
      (if (nil? thunk)
        nil
        (thunk)))))

(defmacro cond [& more]
  (when-not (even? (count more))
    (throw (RuntimeException. "cond takes an even number of clauses")))
  (let [bodies (->> more rest (take-nth 2))
        thunks (map (fn [body]
                      `(fn [] ~body))
                    bodies)
        predicates (take-nth 2 more)]
    `(cond-helper [~@predicates] [~@thunks])))

(defmacro condp [pred expr & clauses]
  (let [pairs (partition 2 clauses)
        else (if (even? (count clauses)) nil (last clauses))
        body (mapcat (fn [[test body]]
                       `((~pred ~test ~expr) ~body))
                     pairs)
        body (if (nil? else) body (concat body [:else else]))]
    `(cond ~@body)))

(defpiplintype Union [schema enum])
(defn union
  "Takes a map of keywords to types and an optional enum
  and returns a tagged union type whose keys are elements
  of the given enum or the default enum of the map's keys."
  [schema & backing-enum]
  (when (> (count backing-enum) 1)
    (throw+ (error "union has only 1 optional argument")))
  (when-not (and (map? schema) (every? keyword? (keys schema)))
    (throw+ (error
              "schema must be a map whose keys are keywords")))
  (let [enum (if (seq backing-enum)
               (first backing-enum)
               (enum (set (keys schema))))]
    (when-not (clj/= (:kind enum) :enum)
      (throw+ (error "not an enum: " enum)))
    (when-not (clj/= (intersection (set (keys (:keymap enum)))
                               (set (keys schema))))
      (throw+ (error "Schema and enum must have same keys")))
    (merge (Union. schema enum)
           {:kind :union})))

(defmethod promote
  :union
  [type obj]
  (cond
    (clj/= (typeof obj) type) obj
    (map? obj)
    (let [tag (key (first obj))
          v (val (first obj))]
      (when-not (clj/= (count obj) 1)
        (throw+ (error "Union map must have 1 element")))
      (if-let [val-type (get (:schema type) tag)] 
        (let [v (cast val-type v)]
          (if (pipinst? v)
            (instance type obj :constrain)
            ;TODO: should composite types need to make ast in promote
            ;or should cast be smarter?
            (mkast-explicit-keys type :make-union
                                 [:tag :val] {:tag tag :val v}
                                 #(promote type {tag %}))))
        (throw+ (error "Tag must be one of"
                       (keys (:schema type))))))))

(defmethod bit-width-of
  :union
  [type]
  (->> type
    :schema
    vals
    (map bit-width-of)
    (reduce max)
    (+ (bit-width-of (:enum type)))))

(defmethod get-bits
  :union
  [expr]
  (let [v (value expr)
        e (:enum (typeof expr))
        tag (get-bits (cast e (key (first v))))
        data (get-bits (val (first v)))
        padding (- (bit-width-of (typeof expr))
                   (count tag)
                   (count data))
        padding (vec (repeat padding 0))]
    (vec (concat tag padding data))))

(defmethod from-bits
  :union
  [type bs]
  (let [enum-size (bit-width-of (:enum type))
        tag-bits (subvec bs 0 enum-size)
        val-bits (subvec bs enum-size)
        enum-val (from-bits (:enum type) tag-bits)
        val-type (get (:schema type) enum-val)
        val-bits (subvec val-bits 0 (bit-width-of val-type))]
    {enum-val (from-bits val-type val-bits)}))

(defmethod constrain
  :union
  [type val]
  (when (> (count val) 1)
    (throw+ (error "must have 1 elemen")))
  (let [[k v] (first val)]
    {k (cast (get (:schema type) k) v)}))

(defmethod check
  :union
  [inst]
  (let [schema (:schema (typeof inst))
        m (value inst)
        k (key (first m))
        v (val (first m))]
    (when-not (clj/= (class (typeof inst)) piplin.math.Union)
      (throw+ (error "Union has wrong class")))
    (when-not (clj/= 1 (count m))
      (throw+ (error m "must have 1 key/value pair")))
    (when-not (get schema k)
      (throw+ (error k "not in schema:" schema)))
    (when-not (clj/= (typeof v) (get schema k))
      (throw+ (error (typeof v) "should be type" (get schema k))))
    )
  inst)

(defn get-value
  [k u]
  "Gets the value of the union if the key is k.
  Otherwise fails somehow..."
  (let [e (-> (typeof u) :enum)]
    (if (pipinst? u)
      (let [v (-> (value u) first)]
          (comment (throw (RuntimeException. (str "invalid union: "
                                         "expected " k 
                                         " but got " (key v))))) 
        (if (clj/= (key v) k)
          (val v)
          (let [vtype (-> (typeof u)
                                 :schema
                                 (get k))]
          (deserialize vtype (cast (-> vtype
                                     bit-width-of
                                     bits) 0)))) 
          )
      (mkast (get (-> (typeof u) :schema) k)
             :get-value
             [u]
             (partial get-value k)))))

(defn get-tag
  "Gets the tag of the union"
  [u]
  (let [e (-> (typeof u) :enum)]
    (if (pipinst? u)
      (e (-> (value u) first key)) 
      (mkast e :get-tag [u] get-tag))))


(defmacro union-match
  "Takes a union u and a clause for each
  key in the schema. Clauses are of the form:

  (:key ...)

  where :key is the keyword and ... is an
  implicit do within a cond-like form."
  [u & clauses]
  (let [u-sym (gensym "u")
        clauses (map (fn [[k name & body]]
                       [k `(let [~name (get-value ~k ~u-sym)]
                             ~@body)])
                     clauses)]
    `(do
       (let [~u-sym ~u]
         (when-not (sym-diff (-> (typeof ~u-sym)
                               :enum
                               :keymap
                               :keys
                               set)
                             (set '~(map first clauses)))
           (throw+ (error "keys don't match"))) 
         (condp = (get-tag ~u-sym)
           ~@(apply concat (butlast clauses))
           ~(second (last clauses)))))))

(defn str-to-bits
  [s]
  (let [bit-vec (->> s
                  (filter (partial clj/not= \_))
                  (map
                    #(condp clj/= % \0 0 \1 1
                       (do
                         (throw (IllegalArgumentException.
                                (str "invalid bit: " %))))))
                  vec)]
    `((bits (count ~bit-vec)) ~bit-vec)))

(defn binary-literal
  [rdr letter-b]
  (loop [v []
         ch (clojure.lang.LispReader/read1 rdr)]
    (if (#{(int \0) (int \1) (int \_)} ch)
      (do
        (recur (if (= (int \_) ch)
                 v
                 (conj v ch))
               (clojure.lang.LispReader/read1 rdr)))
      (let [bit-vec (vec (map {(int \0) 0 (int \1) 1} v))]
        (.unread rdr ch)
        `((bits ~(count bit-vec)) ~bit-vec)))))

(defn dispatch-reader-macro  [ch fun]
  (let  [dm  (.get  (doto  (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                      (.setAccessible true))
                   nil)]
    (aset dm  (int ch) fun)))

(dispatch-reader-macro \b binary-literal)
