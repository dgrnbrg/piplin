(ns piplin.types.bundle
  "This namespaces provides an implementation of bundles, which
  are piplin's record type. It provides `assoc`, `assoc-in`, and `get`
  that are compatible with bundles and clojure maps."
  (:refer-clojure :exclude [get assoc assoc-in cast])
  (:require [clojure.core :as clj])
  (:require [piplin.util :as util])
  (:use [slingshot.slingshot])
  (:use [piplin.types])
  (:use [piplin.types.bits]))

(defn get
  "Gets the given key from the bundle"
  [bund key]
  (clj/condp = (piplin-clojure-dispatch bund) 
    :use-core-impl 
    (clj/get bund key) 
    :bundle 
    (if (pipinst? bund)
      (clj/get (value bund) key)
      (mkast (get-in (typeof bund) [:schema key])
             :bundle-key
             [bund key]
             get))
    (throw+ (error "Don't know how to get from" bund))))

(defn assoc
  "Returns a new bundle whose key k is equal to v.
  All other keys are unchanged."
  ([bund k v]
   (clj/condp = (piplin-clojure-dispatch bund)
     :use-core-impl
     (clj/assoc bund k v)
     :bundle
     (let [schema (:schema (typeof bund))
           v-type (clj/get schema k)
           v (cast v-type v)]
       (if (pipinst? bund)
         ((typeof bund) (clj/assoc (value bund) k v))
         (mkast (typeof bund)
                :bundle-assoc
                [bund k v]
                assoc)))
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

(defmethod piplin.types/valAt-multi
  :bundle
  ([bundle key]
   (piplin.types/valAt-multi bundle key nil))
  ([bundle key notfound]
   (get bundle key)))

(defpiplintype Bundle [schema])
(defn bundle
  "Takes a map of keys to types and returns
  a bundle type with that schema."
  [schema]
  (clj/cond
    (not (map? schema))
    (throw+ (error "Schema must be a map"))
    (some (comp not keyword?) (keys schema))
    (throw+ (error "keys must be keywords"))
    (some #(not (or (:kind %) (class? %))) (vals schema))
    (throw+ (error "values must be piplin or java types:" schema))
    :else
    (merge (Bundle. schema)
           {:kind :bundle})))


(defmethod promote
  :bundle
  [type obj]
  (clj/cond
    (clj/= (typeof obj) type) obj
    (map? obj)
    (let  [schema (:schema type)]
      (when-let [bad (seq (util/sym-diff (set (keys schema))
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
    (when-let [bad (seq (util/sym-diff (set (keys schema))
                                  (set (keys (value inst)))))]
      (throw+ (error "These keys either didn't have a value"
                     "or aren't part of the schema")))
    (when-not (every? identity correct)
      (throw+ (error (value inst)
                     "doesn't match schema"
                     schema)))) 
  inst)

