(ns piplin.test.aggregates
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or])
  (:use [piplin.types bundle uintm enum bits union boolean core-impl binops])
  (:use [piplin types mux modules sim connect protocols])
  (:import clojure.lang.ExceptionInfo) 
  (:use clojure.test))

(deftest bundle-test
  (let [b1 (bundle {:a (uintm 3)
                    :b (enum #{:foo :bar})})]
    (is (instance b1 {:a (cast (uintm 3) 2)
                      :b (cast (enum #{:foo :bar})
                               :foo)}))
    (is (thrown? ExceptionInfo
                 (cast b1 {:a 1})))
    (is (thrown? ExceptionInfo
                 (instance b1 {:a 1
                               :b (cast (enum #{:foo :bar})
                                        :foo)})))
    (is (= (:a (cast b1 {:a 4 :b :bar})) ((uintm 3) 4)))
    (let [{:keys [a b]} (cast b1 {:a 2 :b :bar})]
      (is (= a ((uintm 3) 2)))
      (is (= b ((enum #{:foo :bar}) :bar))))
    (let [mod (module [:outputs [o (cast b1 {:a 0 :b :foo})]]
                      (let [{:keys [a b]} o
                            a' (inc a)
                            b' (mux2 (= b :foo)
                                     :bar :foo)]
                        (connect o (cast b1 {:a a' :b b'}))))
          [state fns] (make-sim mod)] 
      (is (= (get (exec-sim state fns 0)
                  [:o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [:o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [:o])
             (cast b1 {:a 2 :b :foo}))))
    (let [mod (module [:outputs [o (cast b1 {:a 0 :b :foo})]]
                      (let [a' (inc (get o :a))
                            b' (mux2 (= (get o :b)
                                        (cast (enum #{:foo :bar})
                                              :foo))
                                     :bar :foo)]
                        (connect o (cast b1 {:a a' :b b'}))))
          [state fns] (make-sim mod)] 
      (is (= (get (exec-sim state fns 0)
                  [:o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [:o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [:o])
             (cast b1 {:a 2 :b :foo})))
      )))

(deftest assoc-test
  (let [b (bundle {:x (uintm 4) :y (anontype :boolean)})
        x (cast b {:x 3 :y false})
        {x-x :x x-y :y} x
        x' (assoc x :y true)
        {x'-x :x x'-y :y} x']
    (is (= x-x 3))
    (is (= x-y false))
    (is (= (typeof x') b))
    (is (= x'-x 3))
    (is (= x'-y true))) 
  (let [b1 (bundle {:x (uintm 4) :y (uintm 3)})
        b2 (bundle {:a b1 :b (anontype :boolean)})
        x (cast b2 {:a {:x 2 :y 1} :b false})
        x' (assoc-in x [:a :x] 3)
        {{x-val :x} :a} x
        {{x'-val :x} :a} x']
    (is (= x-val 2))
    (is (= (typeof x') b2))
    (is (= x'-val 3)))
  (is (= (assoc {} :x 3) {:x 3}))
  (is (= (assoc-in {"foo" "bar" :zoo {:a "q"}} [:zoo :b] :x)
         {"foo" "bar" :zoo {:a "q" :b :x}}))
  (is (= (assoc [1 2 3] 1 :x) [1 :x 3])))


(deftest enhanced-bits-test
  (let [e (enum #{:a :b :c})
        b (bundle {:e e :a (uintm 3) :b (anontype :boolean)})]
    ;enums
    (let [e-inst (e :a)
          e-bits (serialize e-inst)
          e-inst2 (deserialize e e-bits)]
      (is (= (:n (typeof e-bits)) 2))
      (is (= e-inst e-inst2) "Unsuccessful serialization roundtrip"))  
    (let [e-inst (e :b)
          e-bits (serialize e-inst)
          e-inst2 (deserialize e e-bits)]
      (is (= (:n (typeof e-bits)) 2))
      (is (= e-inst e-inst2) "Unsuccessful serialization roundtrip"))  
    (let [e-inst (e :c)
          e-bits (serialize e-inst)
          e-inst2 (deserialize e e-bits)]
      (is (= (:n (typeof e-bits)) 2))
      (is (= e-inst e-inst2) "Unsuccessful serialization roundtrip"))
    (let [b-inst (cast b {:e :a :a 4 :b true})
          b-bits (serialize b-inst)
          b-inst2 (deserialize b b-bits)]
      (is (= (kindof b-bits) :bits))
      (is (= 6 (:n (typeof b-bits))))
      (is (= b-inst b-inst2) "Unsuccessful serialization roundtrip"))  
    (let [b-inst (cast b {:e :b :a 3 :b false})
          b-bits (serialize b-inst)
          b-inst2 (deserialize b b-bits)]
      (is (= (kindof b-bits) :bits))
      (is (= 6 (:n (typeof b-bits))))
      (is (= b-inst b-inst2) "Unsuccessful serialization roundtrip"))))

(deftest union-test
  (let [e (enum #{:a :b :c})
        b (bundle {:car e :cdr (uintm 4)})
        u (union {:x (uintm 5) :y b})
        m (module [:outputs [v (u {:x ((uintm 5) 0)})
                             o ((uintm 5) 22)]]
            (union-match v
              (:x x
                  (connect o 22)
                  (connect v (cast u {:y {:car :b 
                                           :cdr 3}})))
              (:y {:keys [car cdr]}
                  (connect o 31)
                  (mux2 (< cdr 7)
                        (connect v (cast u {:y {:car :c 
                                                :cdr (inc cdr)}}))
                        (connect v (cast u {:x ((uintm 5) 3)}))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [:v])
           (u {:x ((uintm 5) 0)})))  
    (is (= (get (exec-sim state fns 0)
                [:o])
           ((uintm 5) 22)))

    (is (= (get (exec-sim state fns 1)
                [:v])
           (u {:y (cast b {:car :b
                           :cdr 3}) })))  

    (is (= (get (exec-sim state fns 1)
                [:o])
           ((uintm 5) 22)))
    ))

;TODO: write a test that uses a union-match expr as a value.
;this should give more info on the wrongly-taken braken issue

(deftest union-test-2
  (let [e (enum #{:a :b :c})
        b (bundle {:car e :cdr (uintm 4)})
        u (union {:x (uintm 5) :y b})
        m (module [:outputs [v (u {:x ((uintm 5) 0)})
                             o ((uintm 5) 22)]]

            (mux2
              (= (get-tag v) :x)
              (do
                (connect o (uninst 22))
                (connect v (cast u {:y {:car :b 
                                        :cdr 3}}))) 
              (let [{:keys [car cdr]} (get-value :y v)] 
                (connect o (uninst 31))
                (mux2 (< cdr 7)
                      (connect v (cast u {:y {:car :c 
                                              :cdr (inc cdr)}}))
                      (connect v (cast u {:x ((uintm 5) 3)}))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [:v])
           (u {:x ((uintm 5) 0)})))  
    (is (= (get (exec-sim state fns 0)
                [:o])
           ((uintm 5) 22)))

    (is (= (get (exec-sim state fns 1)
                [:v])
           (u {:y (cast b {:car :b
                           :cdr 3}) })))

    (is (= (get (exec-sim state fns 1)
                [:o])
           ((uintm 5) 22)))
    ))

(deftest maybe-test
  (let [maybe-uintm8 (maybe (uintm 8))
        just-uintm8 (cast maybe-uintm8 {:just 3})
        nothing (cast maybe-uintm8 {:nothing nil})]
    (is (= (union-match just-uintm8
                        (:just x :success) 
                        (:nothing _ :fail))
           :success))
    (is (= (union-match nothing
                        (:just x :fail) 
                        (:nothing _ :success))
           :success))))
