(ns piplin.test.modules
  (:use clojure.test)
  (:refer-clojure :as clj :exclude [not= + - * inc dec < > <= >= = cast not bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right])
  (:use [piplin.types boolean numbers core-impl binops uintm])
  (:use plumbing.core)
  (:use [piplin types math sim modules connect]))

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

(deftest module-counter'
  (let [m (compile-root (modulize :root {:x (fnk [x] (inc x))}
                                  {:x ((uintm 8) 0)}))]
    (are [x] (= x (get (last (sim m x)) [:root :x]))
         0 5 10)))

;This test should see a counter that grows arithmatically
(deftest submodule-test
  (let [m (modulize
            {:x (fnk [step x] (+ x step))}
            {:x ((uintm 8) 0)})
        root (modulize :root
               {:step (fnk [step] (inc step))
                :output (fnk [step]
                             (:x (m :step step)))}
               {:step ((uintm 8) 0)})
        system (compile-root root)
        do-sim (fn [cycles]
                 (get (last (sim system cycles)) [:root :output]))]
    (is (= (do-sim 0) ((uintm 8) 0)))  
    (is (= (do-sim 1) ((uintm 8) 0)))  
    (is (= (do-sim 2) ((uintm 8) 1)))  
    (is (= (do-sim 3) ((uintm 8) 3)))  
    (is (= (do-sim 4) ((uintm 8) 6)))  
    (is (= (do-sim 5) ((uintm 8) 10)))  
    (is (= (do-sim 6) ((uintm 8) 15)))  
    (is (= (do-sim 7) ((uintm 8) 21)))  
    (is (= (do-sim 8) ((uintm 8) 28)))  
    (is (= (do-sim 9) ((uintm 8) 36)))))

;This test should see a counter that grows arithmatically
(deftest submodule-test
  (let [m (module step-counter [:inputs [step (uintm 8)]
                                :outputs [x ((uintm 8) 0)]]
                  (connect x (+ x step)))
        root (module generator [:outputs [x ((uintm 8) 0)]
                                :feedback [step ((uintm 8) 0)]
                                :modules [sub m]]
                     (connect step (inc step))
                     (connect sub$step step))
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

(defmodule counter [n]
  [:outputs [x ((uintm n) 0)]]
  (connect x (inc x)))

(defmodule delayer []
  [:inputs [in (uintm 8)]
   :outputs [out ((uintm 8) 0)]]
  (connect out in))

(defmodule delayer-holder []
  [:modules [c (counter 8)
             d (delayer)]]
  (connect d$in c$x))

(def counter'
  (modulize {:x (fnk [x] (inc x))} {:x ((uintm 8) 0)}))

(def delayer'
  (modulize {:out (fnk [in] in)} {:out ((uintm 8) 0)}))

(def delayer-holder'
  (modulize :root {:out (fnk []
                             (let [counter (counter')
                                   delayer (delayer' :in (:x counter))]
                               (:out delayer)))}
            nil))

(deftest delayer-test
  (let [m (compile-root delayer-holder')]
    (are [cycle value] (= (get (last (sim m cycle)) [:root :out]) value)
         0 0
         1 0
         2 1
         3 2
         4 3
         5 4
         6 5)))

(deftest delayer-test
  (let [m (delayer-holder)
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0) [:d :out]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 1) [:d :out]) ((uintm 8) 0)))  
    (is (= (get (exec-sim state fns 2) [:d :out]) ((uintm 8) 1)))  
    (is (= (get (exec-sim state fns 3) [:d :out]) ((uintm 8) 2)))
    (is (= (get (exec-sim state fns 4) [:d :out]) ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 5) [:d :out]) ((uintm 8) 4)))  
    (is (= (get (exec-sim state fns 6) [:d :out]) ((uintm 8) 5)))))
