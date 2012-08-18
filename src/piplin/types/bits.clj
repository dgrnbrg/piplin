(ns piplin.types.bits
  "This namespace contains logic to manage bit types.
  
  First, it defines the `bits` type, which supports `bit-and`, `bit-or`,
  `bit-xor`, and `bit-not`.
  
  It defines the important multimethods `bit-width-of`, `from-bits`,
  and `get-bits`, which must be implemented by every type that wants
  to have its immediate forms synthesizable, as well as to participate
  in any bit-width related calculations. The need only operate on pipinsts.
  
  `serialize` and `deserialize` form the standard API for converting
  between any type and bits.
  
  Finally, this namespaces adds the reader literal form `#b0101` for bits."
  (:use [slingshot.slingshot])
  (:refer-clojure :exclude [cast])
  (:use [piplin protocols types])
  (:require [piplin.types.core-impl :as impl])
  (:use [piplin.types.binops :only [defbinopimpl defunopimpl]]))

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
    (when-not (= (count v) n)
      (throw+ (error "bit vector must be of length" n "but was" (count v)))))
  inst)

(defmulti from-bits
  (fn [type bits] (:kind type))
  :hierarchy types)

(defmethod from-bits
  :default
  [type bits]
  (throw+ (error "No way to convert bits to " type)))

(defmethod from-bits
  :bits
  [type bits]
  bits)

(defmulti bit-width-of
  "Takes a type and returns the number
  of bits needed to represent that type"
  (fn bit-width-dispatch [type]
    (if (pipinst? type)
      :default
      (:kind type)))
  :hierarchy types)

(defmethod bit-width-of
  :default
  [expr]
  (throw+ (error "Don't know how to get bit width of" expr "(this takes a type)")))

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
    (when-not (= (kindof bits) :bits)
      (throw+ (error bits "must be of kind :bits")))
    (when-not (= n (bit-width-of type))
      (throw+ (error type "has bit width" (bit-width-of type) "but should be" n)))
    (if (pipinst? bits)
      (cast type (from-bits type (value bits)))
      (mkast type :deserialize [bits] (partial deserialize type)))))

(defmethod get-bits
  :default
  [expr]
  (throw+ (error "Cannot convert " expr " to bits")))

(defmethod get-bits
  :bits
  [expr]
  (value expr))

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
(defn bit-slice
  "Takes an expr of type bits and returns a subrange
  of the bits."
  [expr low high]
  (if-not (= (kindof expr) :bits)
    (throw+ (error "Can only slice bits, not " expr))
    (let [type (bits (- high low))]
      (if (pipinst? expr)
        (slice-impl expr low high)
        (mkast type :slice [expr low high] slice-impl)))))

;TODO: this should use mkast-explicit-keys to support
;varargs in the ast node, which will be faster in sim and
;simpler to read in verilog
(defn bit-cat
  ([]
   (instance (bits 0) []))
  ([bs]
   bs)
  ([b1 b2]
   (let [type (bits (+ (-> b1 typeof :n)
                       (-> b2 typeof :n)))]
     (if (and (pipinst? b1) (pipinst? b2))
       (instance type
                 (vec (concat (value b1) (value b2))))
       (mkast type :bit-cat [b1 b2] bit-cat))))
  ([b1 b2 & more]
   (if more
     (recur (bit-cat b1 b2) (first more) (next more))
     (bit-cat b1 b2))))

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
    (when-not (= (:n type) n)
      (throw+ (error "Bit size mismatch"))))
  (condp isa-type? (kindof obj)
    :bits obj
    :j-integral (instance type
                          (long-to-bitvec obj
                                          (:n type)))
    (throw+ (error "Cannot promote" obj "to bits"))))

(defbinopimpl impl/bit-and :bits [:j-integral]
  [x y]
  (vec (map #(bit-and %1 %2)
            (value x) (value y))))

(defbinopimpl impl/bit-or :bits [:j-integral]
  [x y]
  (vec (map #(bit-or %1 %2)
            (value x) (value y))))

(defbinopimpl impl/bit-xor :bits [:j-integral]
  [x y]
  (vec (map #(bit-xor %1 %2)
            (value x) (value y))))

(defunopimpl impl/bit-not :bits 
  [x]
  (vec (map (partial - 1) (value x))))

(defn str-to-bits
  [s]
  (let [bit-vec (->> s
                  (filter (partial not= \_))
                  (map
                    #(condp = % \0 0 \1 1
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
        (.unread ^java.io.PushbackReader rdr ch)
        `((bits ~(count bit-vec)) ~bit-vec)))))

(defn dispatch-reader-macro  [ch fun]
  (let  [dm  (.get  (doto  (.getDeclaredField clojure.lang.LispReader "dispatchMacros")
                      (.setAccessible true))
                   nil)]
    (aset ^"[Lclojure.lang.IFn;" dm (int ch) fun)))

(dispatch-reader-macro \b binary-literal)
