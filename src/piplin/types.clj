(ns piplin.types
  (:use [clojure.tools.macro :only [name-with-attributes]])
  (:use [slingshot.slingshot :only [throw+ try+]]))

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

(defrecord CompilerError [msg])

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
  (CompilerError. (apply print-str args)))

(defmethod promote :default
  [type obj]
  (error type "is not a valid type instance"))

(defn error?
  "True iff a is a reportable error"
  [a]
  (or (isa-type? (class a) :error)
      (and (coll? a) (seq a) (every? error? a))))

(defn try-error
  "Evaluates a thunk. If it returns a value,
  try-error returns that value. If it throws
  an error, it adds it to the error coll and
  returns nil."
  [thunk error-coll]
  (try+
    (thunk)
    (catch error? e
      (swap! error-coll conj e))))

(defmacro join-errors
  "Takes a list of forms and evaluates them.
  Any that throw errors will have the errors
  caught and stored in a coll of all the thrown
  errors. If nothing threw an error, returns
  a seq of the values of the forms. If any
  threw errors, rethrows the coll of errors."
  [& args]
  (let [error-coll (gensym "error-coll")
        arg-thunks (map (fn [arg]
                          `(try-error (fn [] ~arg)
                                      ~error-coll))
                        args)]
    `(let [~error-coll (atom [])
           args# [~@arg-thunks]
           ec# (flatten @~error-coll)]
       (if (seq ec#)
         (throw+ ec#)
         args#))))

(defmacro try-errors
  "Executes the forms in an implicit do and
  returns the result or errors."
  [& forms]
  `(try+
     ~@forms
     (catch error? ~'e
       ~'e)))

(defmacro defn-errors
  "Like defn, but allows any number of the
  argument to throw errors, using join-errors"
  [name & args]
  (let [[name [params & body]] (name-with-attributes
                               name args)
        param-sym (gensym "params")]
    `(defmacro ~name [& ~param-sym]
       `(apply (fn [~@'~params] ~@'~body)
                           (join-errors
                             ~@~param-sym)))))

(defn-errors type-unify
  "Takes a target kind and two other
  objects. Determines which of them is the
  target kind, then promotes the other
  to that type instance. Returns errors
  objects if any errors occurred, or if
  any of the objects were errors"
  [target-kind a b]
  (letfn [(type-unify [target-kind a b]
            (cond
              (= (:kind a) target-kind)
              (let [b (promote (:type a) b)]
                (if-not (error? b)
                  [a b]
                  b))
              (= (:kind b) target-kind)
              (let [[b a] (type-unify target-kind b a)]
                [a b])
              :else
              (throw+ (error "Neither" a "nor" b "is of kind" target-kind))))]
    (type-unify target-kind a b)))
