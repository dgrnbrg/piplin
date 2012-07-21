(ns piplin.types.union
  (:refer-clojure :exclude [cast])
  (:use [slingshot.slingshot])
  (:require [piplin.mux :as mux])
  (:require clojure.set)
  (:require [piplin.types.binops :as binops])
  (:require [piplin.util :as util])
  (:use [piplin.types]
        [piplin.types bits enum]))

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
    (when-not (= (:kind enum) :enum)
      (throw+ (error "not an enum: " enum)))
    (when-not (= (clojure.set/intersection (set (keys (:keymap enum)))
                               (set (keys schema))))
      (throw+ (error "Schema and enum must have same keys")))
    (merge (->Union schema enum)
           {:kind :union})))

(defmethod promote
  :union
  [type obj]
  (cond
    (= (typeof obj) type) obj
    (map? obj)
    (let [tag (key (first obj))
          v (val (first obj))]
      (when-not (= (count obj) 1)
        (throw+ (error "Union map must have 1 element")))
      (if-let [val-type (get (:schema type) tag)] 
        (let [v (cast val-type v)]
          (if (pipinst? v)
            (instance type obj :constrain)
            ;TODO: should composite types need to make ast in promote
            ;or should cast be smarter?
            (mkast-explicit-keys type :make-union
                                 [:tag :val] {:tag tag :val v}
                                 #(promote type {%1 %2}))))
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
    (when-not (= (class (typeof inst)) Union)
      (throw+ (error "Union has wrong class")))
    (when-not (= 1 (count m))
      (throw+ (error m "must have 1 key/value pair")))
    (when-not (get schema k)
      (throw+ (error k "not in schema:" schema)))
    (when-not (= (typeof v) (get schema k))
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
        (if (= (key v) k)
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
         (when-not (util/sym-diff (-> (typeof ~u-sym)
                               :enum
                               :keymap
                               :keys
                               set)
                             (set '~(map first clauses)))
           (throw+ (error "keys don't match"))) 
         (mux/condp binops/= (get-tag ~u-sym)
           ~@(apply concat (butlast clauses))
           ~(second (last clauses)))))))
