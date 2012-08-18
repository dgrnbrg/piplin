(ns piplin.types.array
  "This namespace provides a fixed-length homogenous array type."
  (:refer-clojure :exclude [cast])
  (:use [slingshot.slingshot])
  (:use [piplin protocols types])
  (:use [clojure.core.incubator :only [seqable?]])
  (:use [piplin.types bits [uintm :only [uintm]]]))

(defpiplintype Array [array-len array-type])
(defn array
  [type length]
  (when-not (integer? length)
    (throw+ "Length must be an integer"))
  (when-not (:kind type)
    (throw+ (str type " must be a piplin type")))
  (merge (Array. length type)
         {:kind :array}))

(defmethod promote
  :array
  [type obj]
  ;TODO: if type or obj is an error, return it instead, or maybe do best effort, or ...
  (if (and (pipinst? obj)
           (= (kindof obj) :array))
    (let [{atype :array-type alen :array-len} (typeof obj)
          s (value obj)]
      (if-not (= alen (:array-len type))
        (ast-error type (str "Array is not of length"
                             (:array-len type) 
                             "=>" obj))
        (instance type (into {}
                             (map (fn [[k v]]
                                    [k (promote (:array-type type) v)])
                                  s)))))
    (do
      (when-not (seqable? obj)
        (throw+ "Can only promote seqables to piplin array"))
      (let [s (seq obj)
            {array-type :array-type array-len :array-len} type]
        (when-not (= (count s) array-len)
          (throw+ (str "Expected " array-len
                       " elements, but got "
                       (count s) "elements")))
        (let [casted-obj (zipmap (->> (range array-len)
                                   (map (comp keyword str)))
                                 (map (partial cast array-type)
                                      s))] 
          (if (every? pipinst? (vals casted-obj))
            (instance type casted-obj :constrain)
            (mkast-explicit-keys type :make-array
                                 (map keyword (range array-len))
                                 casted-obj
                                 (fn [& args]
                                   (promote type args)))))))))

(defmethod check
  :array
  [obj]
  (when (and (pipinst? obj)
             (some (comp not pipinst?)  (vals (value obj))))
    (throw+ (error "Array looks like a pipinst, but contains non-pipinst values:" (vals (value obj)))))
  obj)

(defmethod bit-width-of
  :array
  [{:keys [array-type array-len]}]
  (* array-len (bit-width-of array-type)))

(defmethod get-bits
  :array
  [expr]
  (let [inst (value expr)
        keys (->> (typeof expr)
               :array-len
               range
               (map (comp keyword str)))]
    (->> keys
      (map #(serialize (inst %1)))
      (apply bit-cat)
      value)))

(defmethod from-bits
  :array
  [type bs]
  (let [{array-type :array-type array-len :array-len} type
        objs (partition (bit-width-of array-type) bs)] 
    (mapv (partial from-bits array-type) objs)))

(defmethod piplin.types/nth-multi
  :array
  ([array i]
   (piplin.types/nth-multi array i nil))
  ([array i notfound]
   (get array i)))

;TODO: ensure that i is the correct bit width, or a constant
(defmethod piplin.types/valAt-multi
  :array
  ([array i]
   (piplin.types/valAt-multi array i nil))
  ([array i notfound]
   (let [i (cast (uintm (log2 (-> array typeof :array-len))) i)]
     (if (and (pipinst? i)
            (pipinst? array))
     (get (value array) (-> i value str keyword) notfound)
     (mkast (:array-type (typeof array))
            :array-get 
            [array i]
            get)))))

(defmethod piplin.types/entryAt-multi
  :array
  ([array i]
   (let [alen (:array-len (typeof array))]
     (when (>= i alen)
       (throw+ (str "Array is only " alen
                    "long; tried to access index " i))) 
     (get array i))))

(defmethod piplin.types/assoc-multi
  :array
  [array index v]
  (let [{:keys [array-type array-len]} (typeof array)
        v (cast array-type v)]
    (if (and (pipinst? array)
             (pipinst? index)
             (pipinst? v))
      (do
        (when (or (neg? (value index))
                  (>= (value index) array-len))
          (throw+ "Index must be between 0 and" array-len
                  "but was" (value index)))
        ((typeof array) (assoc (value array) ((comp keyword str) (value index)) v)))
      (mkast (typeof array)
             :array-assoc
             [array index v]
             assoc))))

(defmethod piplin.types/count-multi
  :array
  [array]
  (:array-len (typeof array)))

(defmethod piplin.types/seq-multi
  :array
  [array]
  (when-not (pipinst? array)
    (throw+ (error "Can only use seq on pipinst arrays")))
  (let [{:keys [array-len]} (typeof array)
        val-map (value array)]
    (->> (range array-len)
      (map (comp keyword str))
      (map val-map))))
