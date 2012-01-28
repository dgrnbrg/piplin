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
               :kind :module,
               :type :module,
               :feedback {:bar "bar"},
               :body
               [{:token token
                 :port :foo
                 :kind nil
                 :type "foo"}
                {:token token
                 :port :quux
                 :kind nil
                 :type nil}
                {:token token
                 :kind nil
                 :port :bar
                 :type nil}]})))))
  ;TODO: ensure error-checking works
  )
