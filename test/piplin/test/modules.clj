(ns piplin.test.modules
  (:use clojure.test)
  (:use [clojure.core.logic :only [== run fresh lvar]])
  (:require [clojure.zip :as z])
  (:use piplin.modules))

(deftest map-zipper-test
  (let [mz (map-zipper (module [:inputs [a 22]
                                :feedback [b 3]]
                         (connect b a))
                       )]
    (is (= (entry :a 22)
           (z/node (z/up (go-path-down mz [:inputs :a])))))
    (is (= 22
           (z/node (go-path-down mz [:inputs :a]))))))

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
