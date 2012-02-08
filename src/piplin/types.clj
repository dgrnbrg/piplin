(ns piplin.types
  (:use [slingshot.slingshot :only [throw+ try+]]))

(comment
;  TODO: email jim@dueys.net questions about monads
  since the error handling is really quite monadic)

(def types (atom (make-hierarchy)))

(defn kindof [a]
  (or (get-in a [:type :kind]) (class a)))

(defmulti promote
  "Produces an instance of the given
  obj with the type changed. Takes
  a type instance and an object to be
  casted."
  (fn [type obj] (:kind type))
  :hierarchy types)

(defrecord CompilerError [msg])

(defn derive-type
  [child parent]
  (swap! types derive child parent))

(defn isa-type?
  [type unknown]
  (isa? @types unknown type))

(derive-type CompilerError :error)

(defn error
  "Convenience function to define a compiler error"
  [& args]
  (CompilerError. (apply print-str args)))

(defmethod promote :default
  [type obj]
  (throw+ (error type "is not a valid type instance")))

(defn error?
  "True iff a is a reportable error"
  [a]
  (or (isa-type? :error (class a))
      (and (coll? a) (seq a) (every? error? a))))

(defmacro try-errors
  "Executes the forms in an implicit do and
  returns the result or errors."
  [& forms]
  `(try+
     ~@forms
     (catch error? ~'e
       ~'e)))

(defn type-unify
  "Takes a target kind and two other
  objects. Determines which of them is the
  target kind, then promotes the other
  to that type instance. Returns errors
  objects if any errors occurred, or if
  any of the objects were errors"
  [target-kind a b]
  (cond
    (= (kindof a) target-kind)
    (let [b (promote (:type a) b)]
      [a b])
    (= (kindof b) target-kind)
    (let [[b a] (type-unify target-kind b a)]
      [a b])
    :else
    (throw+ (error "Neither" a "nor" b "is of kind" target-kind))))
