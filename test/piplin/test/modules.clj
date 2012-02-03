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
                (connect quux foo)
                bar)
              {:token token,
               :outputs {:baz 12 :quux 22},
               :inputs {:foo "foo"},
               :modules nil,
               :kind :module,
               :type :module,
               :feedback {:bar "bar"},
               :body
               [{:type :connection
                 :kind :connection
                 :args {
                        :reg {:token token
                              :port :quux
                              :kind nil
                              :type nil}
                        :expr {:token token
                               :port :foo
                               :kind nil
                               :type "foo"}}}]})))))
  ;TODO: ensure error-checking works
  )
