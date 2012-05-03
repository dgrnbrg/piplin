(ns piplin.test.modules
  (:use clojure.test)
  (:use [piplin types math sim modules]))

(deftest module*-counter
  (let [m (module* "counter" :outputs {:x ((uintm 8) 0)}
                   :connections [#(connect (make-port* :x (uintm 8))
                                          (inc (make-port* :x (uintm 8))))])
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0) [:x]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 5) [:x]) ((uintm 8) 5)))  
    (is (= (get (exec-sim state fns 10) [:x]) ((uintm 8) 10)))))
