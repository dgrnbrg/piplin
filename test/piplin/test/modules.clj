(ns piplin.test.modules
  (:use clojure.test)
  (:use [clojure.core.logic :only [== run fresh lvar]])
  (:use piplin.modules))

(deftest module-expansion
  (is (seq
        (run 1 [q]
             (fresh [token]
                    (==
                      (module [:inputs
                               [foo "foo"]
                               :outputs
                               [baz 12
                                quux 22]
                               :feedback
                               [bar "bar"]]
                              foo
                              quux
                              bar)
                      {:token token,
                       :outputs {:baz 12 :quux 22},
                       :inputs {:foo "foo"},
                       :modules nil,
                       :type :module,
                       :feedback {:bar "bar"},
                       :body
                       [{:token token
                         :port :foo
                         :type "foo"}
                        {:token token
                         :port :quux
                         :type nil}
                        {:token token
                         :port :bar
                         :type nil}]})))))
  ;TODO: ensure error-checking works
  )
