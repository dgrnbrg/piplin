(ns piplin.test.modules
  (:use clojure.test)
  (:use piplin.modules))

(deftest module-expansion
  (is (= (macroexpand-1
           '(piplin.modules/module []))
         {:outputs nil
          :inputs nil
          :type :module
          :body []
          :feedback nil
          :modules nil}))
  (is (= (macroexpand-1
            '(piplin.modules/module [:inputs
                      [a type
                       b type]
                      :outputs
                      [o1 init-val]
                      :feedback
                      [x init-val]
                      :modules
                      [sub1 (instantiate)
                       sub2 (instantiate)]]
                     body...))
          {:outputs {:o1 'init-val}
           :inputs {:a 'type :b 'type}
           :modules {:sub1 '(instantiate)
                     :sub2 '(instantiate)}
           :type :module
           :feedback {:x 'init-val}
           :body ['body...]})))
