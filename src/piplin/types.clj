(ns piplin.types)

(defprotocol IPromotable
  "Used for instance objects"
  (dopromote [type obj]
             "Produces an instance of the given "
             "obj with the type changed"))

(defprotocol IType
  "Used for type objects"
  (kindof [this] "Returns kind of this type."))

(defprotocol ITyped
  "Used for instance objects"
  (type [this] "Returns type instance. "
        "Any parameters have concrete values")
  (kind [this] "Returns kind. Parameters "
        "are ignored"))

(defrecord CompilerError [msg]
  ITyped
  (type [this] :error)
  (kind [this] :error))

(defn error [& args]
  (CompilerError. (apply print-str args)))

(defn error? [a]
  (and (satisfies? ITyped a)
       (= (kind a) :error)))

(defn error-unify* [f]
  "Takes a function a function and produces "
  "a new function that returns a set of errors "
  "if any of the arguments were errors, and "
  "otherwise returns the result of invoking "
  "the function normally."
  (letfn [(error-coll? [c]
            (and (coll? c) (every? error? c)))]
    (fn [& args]
      (if-let [errors (seq (filter
                             #(or (error? %)
                                  (error-coll? %))
                             args))]
        (flatten errors)
        (apply f args)))))

(defn promote [typeinst obj]
  (cond 
    (not (satisfies? IPromotable typeinst))
    (error typeinst "is not a valid type instance")
    (not (satisfies? ITyped obj))
    (error "Cannot promote" obj "to" typeinst)
    :else
    (dopromote typeinst obj)))

(def type-unify
  (error-unify*
    (fn [target-kind a b]
      "Takes a target kind and two other "
      "objects. Determines which of them is the "
      "target kind, then promotes the other "
      "to that type instance. Returns errors "
      "objects if any errors occurred, or if "
      "any of the objects were errors"
      (cond
        (not (satisfies? ITyped a))
        (error a "doesn't have a type")
        (not (satisfies? ITyped b))
        (error b "doesn't have a type")
        (= (kind a) target-kind);
        ;TODO: this needs to propagate the error
        (let [b (promote (type a) b)]
          (if-not (error? b)
            [a b]
            b))
        (= (kind b) target-kind)
        (let [[b a] (type-unify target-kind b a)]
          [a b])
        :else
        (error "Neither" a "nor" b "is of kind" target-kind)))))
