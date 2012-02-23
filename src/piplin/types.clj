(ns piplin.types
  (:use [slingshot.slingshot :only [throw+ try+]]))

(comment
;  TODO: email jim@dueys.net questions about monads
  since the error handling is really quite monadic)

(def types (atom (make-hierarchy)))

(defprotocol ITyped
  "Things with types in piplin implement this."
  (typeof [this] "Return type obj for this obj.")
  (value [this] "Return the value of this")
  (pipinst? [this] "Returns true if this is an
                   instance of the type (as opposed
                   to a symbolic representation)"))

(extend-protocol ITyped
  Object
  (typeof [this] (:type this))
  (value [this] (throw (IllegalArgumentException.
                         (str "Cannot get value of "
                              this))))
  (pipinst? [this] false))

(defn kindof [a]
  (if (satisfies? ITyped a)
    (-> a typeof :kind)
    (class a)))

(defn anontype
  "Returns an anonymous type of
  the given kind. Useful to promote
  to jvm types."
  [kind]
  {:kind kind})

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
    (let [b (promote (typeof a) b)]
      [a b])
    (= (kindof b) target-kind)
    (let [[b a] (type-unify target-kind b a)]
      [a b])
    :else
    (throw+ (error "Neither" a "nor" b "is of kind" target-kind))))

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

;This is a scalar instance
(deftype Instance [type val metamap]
  java.lang.Object
  (equals [this other]
    (and (= (.type other) type)
         (= (.val other) val)))
  (hashCode [this]
    (+ (.hashCode type)
       (.hashCode val)))
  (toString [this]
    (print-str "{:type" type ":val" val "}"))
  ITyped 
  (typeof [this] type) 
  (value [this] val)
  (pipinst? [this] true)
  clojure.lang.ILookup 
  (valAt 
    [this key] 
    (get val key))
  clojure.lang.IMeta
  (meta [this] metamap)
  clojure.lang.IObj
  (withMeta [this metamap] (Instance. type val metamap)))

(defn instance
  "Creates an instance of the type with value val"
  [type val & more]
  (let [val (if (some #{:constrain} more)
              (constrain type val)
              val)
        inst (Instance. type val {})]
    (vary-meta
      (check inst)
      assoc :sim-factory [#(apply instance
                                  type
                                  val
                                  more) []])))

(defmacro defpiplintype
  "Creates a piplin type record that implements
  the necessary protocols and has the vector of
  members as the record's members."
  [name args]
  `(defrecord ~name ~args
     clojure.lang.IFn
     (invoke [~'this ~'x]
       (instance ~'this ~'x))))

(defprotocol IAugmented
  "Like IMeta, but counts for equality and hashcode"
  (aug [this] "Gets the augment data")
  (with-aug [this aug]
    "Returns this with the new augment data"))

(defn alter-aug
  "Takes an augmented object, a function, and args,
  and returns the object's augmentation to
  (f old-aug args)"
  [obj f & args]
  (let [old-aug (aug obj)
        new-aug (apply f old-aug args)]
    (with-aug obj new-aug)))

(deftype ASTNode [type map metamap]
  java.lang.Object
  (equals [this other]
    (and (= (.type other) type)
         (= (.map other) map)))
  (hashCode [this]
    (+ (.hashCode type) (* 17 (.hashCode map))))
  (toString [this]
    (print-str "ASTNode" type map))

  IAugmented
  (aug [this] map)
  (with-aug [this aug] (ASTNode. type aug metamap))

  clojure.lang.ILookup 
  (valAt 
    [this key] 
    (get map key))

  clojure.lang.IMeta
  (meta [this] metamap)
  clojure.lang.IObj
  (withMeta [this metamap] (ASTNode. type map metamap))

  piplin.types.ITyped
  (typeof [this] type)
  (value [this] (throw+ (error "ASTNode has no value")))
  (pipinst? [this] false))

(defmacro mkast
  "Takes the type, op, args, and function and
  returns an ast fragment."
  [type op args f]
  (let [kwargs (vec (map (comp keyword name) args))
        argmap (zipmap kwargs args)]
    `(vary-meta
       (ASTNode. ~type
                 {:op ~op 
                  :args ~argmap}
                 {})
       assoc :sim-factory [~f ~kwargs])))
