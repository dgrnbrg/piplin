(ns piplin.types
  (:use [slingshot.slingshot :only [throw+ try+]]
        [clojure.pprint]))

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

  (comment
    ;TODO: running an experiment to see if this
    ;could return "this" and make it easier to have
    ;if and cond support arbitrary objects at sim time
    (value [this] (throw (IllegalArgumentException.
                         (str "Cannot get value of "
                              this)))))
(extend-protocol ITyped
  Object
  (typeof [this] (:type this))
  (value [this] this)
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

(defn- unsupported
  [& args]
  (throw (UnsupportedOperationException. (str "Not supported! " (first args)))))

(deftype ASTNode [type map metamap]
  java.lang.Object
  (equals [this other]
    (and (instance? ASTNode other)
         (= (.type other) type)
         (= (.map other) map)))
  (hashCode [this]
    (int (mod (+ (.hashCode type) (* 17 (.hashCode map)))
         Integer/MAX_VALUE)))
  (toString [this]
    (print-str "(ASTNode" type map ")"))

  clojure.lang.ILookup 
  (valAt 
    [this key]
    ((get type
          :valAt
          unsupported)
       this key))

  clojure.lang.IMeta
  (meta [this] metamap)
  clojure.lang.IObj
  (withMeta [this metamap] (ASTNode. type map metamap))

  piplin.types.ITyped
  (typeof [this] type)
  (value [this] map)
  (pipinst? [this]
    (let [x ((get metamap :pipinst?) this)]
      x)))

(defn astnode-dispatch  [f]
  (clojure.pprint/simple-dispatch
    (if (instance? ASTNode f)
      (let [value (value f)
            type (typeof f)]
        (comment (if (map? value) 
          (assoc value :type type))) 
        [:type type :value value])
      f)))

    (comment (cl-format true
      "~<{~;~<type ~_~w~:>, ~_~<map ~_~w~:>, ~_~<metamap ~_~w~:>~;}~:>"
      [[(.type f)]  [(.map f)]  [(.metamap f)]])) 

(defn pprint-ast [f]
  (with-pprint-dispatch astnode-dispatch
    (pprint f)))

(defn instance
  "Creates an instance of the type with value val"
  [type val & more]
  (let [val (if (some #{:constrain} more)
              (constrain type val)
              val)
        inst (ASTNode. type val
                       {:pipinst?
                        (get (meta type)
                             :pipinst?
                             identity)
                        :sim-factory
                        (get (meta val)
                             :sim-factory
                             [(fn []
                                (apply instance 
                                       type 
                                       val 
                                       more)) []])})
        checked (check inst)]
    checked))

(defmacro defpiplintype
  "Creates a piplin type record that implements
  the necessary protocols and has the vector of
  members as the record's members."
  [name args]
  `(defrecord ~name ~args
     clojure.lang.IFn
     (invoke [~'this ~'x]
       (instance ~'this ~'x))))

(defn alter-value
  "Takes an ASTNode and alters its value"
  [astnode f & more]
  (ASTNode. (.type astnode)
            (apply f (.map astnode) more)
            (.metamap astnode)))

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
                 {:pipinst? (fn [& ~'a] false)})
       assoc :sim-factory [~f ~kwargs])))

(defn uninst
  "Takes a pipinst and makes it into an AST frag"
  [pipinst]
  (when-not (pipinst? pipinst)
    (throw+ (error pipinst "must be a pipinist")))
  (let [pipinst (if-not (instance? ASTNode pipinst)
                  (mkast (typeof pipinst)
                         :noop
                         []
                         (fn [] pipinst))
                  pipinst)]
    (vary-meta (alter-value pipinst assoc :args [])
               assoc :pipinst? (fn [x] false))))
