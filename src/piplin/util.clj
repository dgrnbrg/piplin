(ns piplin.util
  (:require [clojure.set]))

(defn sym-diff
  "symmetric different of 2 sets"
  [s1 s2]
  (clojure.set/union
    (clojure.set/difference s1 s2)
    (clojure.set/difference s2 s1)))

(defmacro let'
  "This is a `let` macro that also includes metadata about
  what name was used for each form if that form supports
  metadata."
  [bindings & body]
  (let [bindings'
        (mapcat
          (fn [[name form]]
            (let [tmp-name (gensym)
                  meta-form `(if (and
                                   (instance? clojure.lang.IObj ~tmp-name) 
                                   (instance? clojure.lang.IMeta ~tmp-name))
                               (->> 
                                 (assoc (meta ~tmp-name)
                                        :let-name '~name)
                                 (with-meta ~tmp-name))
                               ~tmp-name)]
              [tmp-name form name meta-form]))
          (partition 2 bindings))]
    (list* `let
           (vec bindings')
           body)))
