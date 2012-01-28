(ns piplin.types)

(comment
;  TODO: email jim@dueys.net questions about monads
  since the error handling is really quite monadic)

(def types (atom (make-hierarchy)))

(defn kindof [a]
  (or (:kind a) (class a)))

(defmulti promote
  "Produces an instance of the given
  obj with the type changed. Takes
  a type instance and an object to be
  casted."
  (fn [type obj] (kindof type))
  :hierarchy types)

(defrecord CompilerError [msg filename lineno])

(defn derive-type
  [child parent]
  (swap! types derive child parent))

(defn isa-type?
  [unknown type]
  (isa? @types unknown type))

(derive-type CompilerError :error)

(defn error
  "Convenience function to define a compiler error"
  [& args]
  (CompilerError. (apply print-str args) 0 0))

(defmethod promote :default
  [type obj]
  (error type "is not a valid type instance"))

(defn error?
  "True iff a is a reportable error"
  [a]
  (or (isa-type? (class a) :error)
      (and (coll? a) (seq a) (every? error? a))))

(defn unify-error-args
  "Takes a function and produces 
  a new function that returns a set of errors 
  if any of the arguments were errors, and 
  otherwise returns the result of invoking 
  the function normally."
  [f]
  (fn [& args]
      (if-let [errors (seq (filter error? args))]
        (flatten errors)
        (apply f args))))

(defn unify-error-results
  "takes a function that returns a seq and
  applies unify-error to the returned seq"
  [f]
  (fn [& args]
    ((unify-error-args identity)
       (apply f args))))

(defmacro let-safe
  "Similar to let, but if any expression satisfies
  the predicate error?, the let is short-circuited
  and the error is returned"
  ([bindings & body]
   (if (> (count bindings) 2)
     `(let-safe [~@(take 2 bindings)]
                (let-safe [~@(drop 2 bindings)]
                          ~@body))
     (let [[vars expr] bindings]
       `(let [result# ~expr]
          (if-not (error? result#)
            (let [~vars result#]
              (do ~@body))
            result#))))))

(def map-safe
  ;"Does a map, but unifies errors."
  (unify-error-results map))

(defn type-unify
  "Takes a target kind and two other
  objects. Determines which of them is the
  target kind, then promotes the other
  to that type instance. Returns errors
  objects if any errors occurred, or if
  any of the objects were errors"
  [target-kind a b]
  ((unify-error-args
     (fn [target-kind a b]
       (cond
         (= (:kind a) target-kind)
         (let-safe [b (promote (:type a) b)]
           (if-not (error? b)
             [a b]
             b))
         (= (:kind b) target-kind)
         (let-safe [[b a] (type-unify target-kind b a)]
           [a b])
         :else
         (error "Neither" a "nor" b "is of kind" target-kind))))
     target-kind a b))
