(ns piplin.test.modules
  (:use clojure.test)
  (:refer-clojure :as clj :exclude [not= bit-or cond bit-xor + - * bit-and assoc assoc-in inc dec bit-not condp < > <= >= = cast get not])
  (:use [piplin types math sim modules]))

(deftest module*-counter
  (let [m (module* 'counter :outputs {:x ((uintm 8) 0)}
                   :connections [#(connect (make-port* :x (uintm 8) :register)
                                          (inc (make-port* :x (uintm 8) :register)))])
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0) [:x]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 5) [:x]) ((uintm 8) 5)))  
    (is (= (get (exec-sim state fns 10) [:x]) ((uintm 8) 10)))))

(deftest module-counter
  (let [m (module counter [:outputs [x ((uintm 8) 0)]] 
                  (connect x
                           (inc x)))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0) [:x]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 5) [:x]) ((uintm 8) 5)))  
    (is (= (get (exec-sim state fns 10) [:x]) ((uintm 8) 10)))))

;This test should see a counter that grows arithmatically
(deftest submodule-test
  (let [m (module step-counter [:inputs [step (uintm 8)]
                                :outputs [x ((uintm 8) 0)]]
                  (connect x (+ x step)))
        root (module generator [:outputs [x ((uintm 8) 0)]
                                :feedback [step ((uintm 8) 0)]
                                :modules [sub m]]
                     (connect step (inc step))
                     (connect (subport sub :sub :step) step))
        [state fns] (make-sim root)]
    (is (= (get (exec-sim state fns 0) [:sub :x]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 1) [:sub :x]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 2) [:sub :x]) ((uintm 8) 1)))  
    (is (= (get (exec-sim state fns 3) [:sub :x]) ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 4) [:sub :x]) ((uintm 8) 6)))  
    (is (= (get (exec-sim state fns 5) [:sub :x]) ((uintm 8) 10)))  
    (is (= (get (exec-sim state fns 6) [:sub :x]) ((uintm 8) 15)))  
    (is (= (get (exec-sim state fns 7) [:sub :x]) ((uintm 8) 21)))  
    (is (= (get (exec-sim state fns 8) [:sub :x]) ((uintm 8) 28)))  
    (is (= (get (exec-sim state fns 9) [:sub :x]) ((uintm 8) 36)))))
