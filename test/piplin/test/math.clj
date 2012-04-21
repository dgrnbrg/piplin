(ns piplin.test.math
  (:use clojure.test)
  (:import slingshot.ExceptionInfo)
  (:use [slingshot.slingshot :only [throw+]])
  (:require [clojure.core :as clj])
  (:use [piplin types math modules sim]))

(deftest j-long-test
  (is (= 3 (+ 1 2)))
  (is (= 6 (* 2 3)))
  (is (thrown? ExceptionInfo (promote :j-long "lx"))))

(deftest float-test
  (is (= 3.5 (+ 1.5 2))))

(deftest =test
  (is (= 1 1 1))
  (is (not (= 1 1 2)))
  (is (= "hello" "hello"))
  (is (= [1 \a] [1 \a]))
  (is (< 3 7))
  (is (> 0 -1.2))
  (is (>= -3.3 -3.3))
  (is (not (<= 4 1)))
  (is (not= 1 2))
  (is (not (not= 0 0))))

(deftest uintm-test
  (letfn [(um8 [v] (instance (uintm 8) v))]
    (is (= (+ (um8 200) 1) (um8 201)))
    (is (= (+ (um8 200) 60) (um8 4)))
    (is (= (+ 1 2 3 (um8 4)) (um8 10)))
    (is (= (instance (uintm 8) -1 :constrain) (um8 255)))
    (is (= (instance (uintm 8) 256 :constrain) (um8 0)))
    (is (thrown? ExceptionInfo (promote (uintm 3) "lx")))
    (is (thrown? ExceptionInfo (promote (uintm 3) -1)))
    (is (thrown? ExceptionInfo (promote (uintm 3) 100)))
    (is (= (- 0 (um8 1)) (um8 255)))))

(deftest binop-error-test
  (let [e (error "hi")]
    (is (= (try-errors (+ 0 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 2 3 (throw+ e))) e))
    (is (= (try-errors (+ 0 1 (throw+ e) 3 (throw+ e))) e))
    (is (= (try-errors (+ (throw+ e) 0)) e))
    (is (= (try-errors (+ (throw+ e) (throw+ e))) e))
    (is (= (try-errors (bit-cat (throw+ e) (throw+ e))) e))))

(deftest bits-test
  (let [b8 #(promote (bits 8) %)]
    (is (= (value (b8 0xa0)) [1 0 1 0 0 0 0 0]))
    (is (= (value (bit-cat (b8 0xa0) (bit-cat (b8 0xfe))))
           [1 0 1 0 0 0 0 0 1 1 1 1 1 1 1 0]))
    (is (= (value (bit-slice (bit-cat (b8 0xf) (b8 0xf0)) 4 12))
           [1 1 1 1 1 1 1 1]))
    (is (= (value ((make-sim-fn (bit-slice (bit-cat (uninst (b8 0xf)) (b8 0xf0)) 4 12))))
           [1 1 1 1 1 1 1 1]))
    (is (= (bit-and (b8 0xf) 0x3c) (b8 0xc)))
    (is (= (bit-or 0xf0 (b8 0xf)) (b8 0xff)))
    (is (= (bit-xor (instance (uintm 8) 0x3c) (b8 0xf))
           (b8 0x33))))
  (is (= (get-bits true) [1]))
  (is (= (get-bits false) [0]))
  (is (= (serialize true) (cast (bits 1) 1)))
  (is (= (serialize false) (cast (bits 1) 0)))
  (is (= (count (value (serialize
                         (promote (enum #{:a :b}) :a))))
         1)) 
  (is (= (count (value (serialize
                         (promote (enum #{:a :b :c}) :a))))
         2)))  

(deftest sim-uintm-bits-test
  (let [mod (module [:outputs [c (promote (uintm 8) 0)
                               d (promote (bits 8) 0)]]
                    (connect c (+ c 1))
                    (connect d (bit-slice (serialize c) 0 4)))
        sim (make-sim mod)
        init-state (first sim)
        init-fns (second sim)]
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :c])
           (instance (uintm 8) 10))
        "ran and counted up to 10")
    (is (= (get (exec-sim init-state
                          init-fns
                          10)
                [(:token mod) :d])
           (instance (bits 4) [1 0 0 1]))
        "ran and counted up to 10 bits")
    (is (= (get (exec-sim init-state
                          init-fns
                          18)
                [(:token mod) :d])
           (instance (bits 4) [0 0 0 1]))
        "ran and counted up to 10 bits")))

(deftest sim-cast-test
  (let [mod (module [:outputs [odd false
                               c (promote (uintm 3) 0)]]
                    (let [new-c (inc c)]
                      (connect odd (cast (anontype :boolean) 
                                         (bit-slice (serialize new-c) 0 1)))
                      (connect c new-c)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :c])
           (promote (uintm 3) 1)))

    (is (= (get (exec-sim state fns 1)
                [(:token mod) :odd])
           true))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :odd])
           false))
    (is (= (get (exec-sim state fns 3)
                [(:token mod) :odd])
           true))

    (is (= (get (exec-sim state fns 30)
                [(:token mod) :c])
           (instance (uintm 3) 30 :constrain)))
    (is (= (get (exec-sim state fns 30)
                [(:token mod) :odd])
           false))))

(deftest sim-mux2-test
  (let [mod (module [:outputs [flip false]]
                    (connect flip (mux2 flip
                                        false
                                        true)))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :flip])
           true))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 10)
                [(:token mod) :flip])
           false))
    (is (= (get (exec-sim state fns 11)
                [(:token mod) :flip])
           true))))

(deftest sim-equals-test
  (let [mod (module [:outputs [triggered false]
                     :feedback [c ((uintm 2) 0)]]
                    (let [c' (inc c)]
                      (connect c c')
                      (connect triggered (= c' ((uintm 2) 3)))))
        [state fns] (make-sim mod)]
    (is (= (get (exec-sim state fns 0)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 1)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 2)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 3)
                [(:token mod) :triggered])
           true))
    (is (= (get (exec-sim state fns 4)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 5)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 6)
                [(:token mod) :triggered])
           false))
    (is (= (get (exec-sim state fns 7)
                [(:token mod) :triggered])
           true))))

(deftest enum-initialize-test
  (let [e (enum #{:a :b :c})]
    (enum (:keymap e)))
  (is (thrown? ExceptionInfo
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 2) 0)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      2 (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a (cast (bits 2) 0)
                      :b (cast (bits 3) 1)})))
  (is (thrown? ExceptionInfo 
               (enum {:a 0
                      :b (cast (bits 2) 1)})))
  (is (thrown? ExceptionInfo 
               (enum #{:a 1 "a"}))))

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
                  [(:token mod) :o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [(:token mod) :o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [(:token mod) :o])
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
                  [(:token mod) :o])
             (cast b1 {:a 0 :b :foo})))
      (is (= (get (exec-sim state fns 1)
                  [(:token mod) :o])
             (cast b1 {:a 1 :b :bar})))
      (is (= (get (exec-sim state fns 2)
                  [(:token mod) :o])
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

(deftest cond-test
  (is (= (cond false 22)
         (clojure.core/cond false 22)))
  (is (= (cond
           (= true true) 0
           false 1)
         (clojure.core/cond
           (= true true) 0
           false 1)))
  (is (= (cond
           (= true true) 0
           :else true)
         (clojure.core/cond
           (= true true) 0
           :else true)))
  (is (thrown-with-msg? ExceptionInfo #"Must include :else.*"
                        (cond
                          (= (uninst true) true) 0
                          false 1)))
  (is (thrown-with-msg? ExceptionInfo #".*different types.*"
                        (cond
                          (= (uninst true) true) 0
                          :else true)))
  (is (= 
        (cond
          true 1
          false 0)  
        (clojure.core/cond
          true 1
          false 0)))
  (is (not=
        1
            (make-sim-fn (cond
                           ;41 != 42
                           (= (uninst 41) 42) ((uintm 3) 2)
                           ;300 > 0
                           (> ((uintm 22) 300) 0) ((uintm 3) 1)
                           :else ((uintm 3) 0)))) "unevaluated sim")
  (is (= 1
         ((make-sim-fn (cond
                         ;41 != 42
                         (= (uninst 41) 42) ((uintm 3) 2)
                         ;300 > 0
                         (> ((uintm 22) 300) 0) ((uintm 3) 1)
                         :else ((uintm 3) 0))))) "evaluted sim"))

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

(deftest mux2-connect-test
  (let [m (module [:outputs [o false
                             i ((uintm 8) 3)]]
                  (mux2 o
                        (do
                          (connect o false)
                          (connect i (+ i i))) 
                        (do
                          (connect o true)
                          (connect i (dec i)))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [(:token m) :o])
           false))  
    (is (= (get (exec-sim state fns 0)
                [(:token m) :i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 1)
                [(:token m) :o])
           true))
    (is (= (get (exec-sim state fns 1)
                [(:token m) :i])
           ((uintm 8) 2)))  
    (is (= (get (exec-sim state fns 2)
                [(:token m) :o])
           false))  
    (is (= (get (exec-sim state fns 2)
                [(:token m) :i])
           ((uintm 8) 4)))  
    (is (= (get (exec-sim state fns 3)
                [(:token m) :o])
           true))  
    (is (= (get (exec-sim state fns 3)
                [(:token m) :i])
           ((uintm 8) 3)))  
    ))

(deftest mux2-connect-test-nested
  (let [e (enum #{:a :b :c}) 
        m (module [:outputs [o false
                             i ((uintm 8) 3)
                             x (e :a)]]
                  (let [lowbit (bit-slice (serialize i) 0 1)]
                    (mux2 o
                          (do
                            (connect o false)
                            (connect i (+ i i))
                            (mux2 (= lowbit ((bits 1) [1]))
                                  (connect x :a)
                                  (connect x :b)))
                          (do
                            (connect o true)
                            (connect x :c)
                            (connect i (dec i))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [(:token m) :o])
           false))  
    (is (= (get (exec-sim state fns 0)
                [(:token m) :i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 0)
                [(:token m) :x])
           (e :a)))  
    (is (= (get (exec-sim state fns 1)
                [(:token m) :o])
           true))
    (is (= (get (exec-sim state fns 1)
                [(:token m) :i])
           ((uintm 8) 2)))  
    (is (= (get (exec-sim state fns 1)
                [(:token m) :x])
           (e :c)))  
    (is (= (get (exec-sim state fns 2)
                [(:token m) :o])
           false))  
    (is (= (get (exec-sim state fns 2)
                [(:token m) :i])
           ((uintm 8) 4)))  
    (is (= (get (exec-sim state fns 2)
                [(:token m) :x])
           (e :b)))  
    (is (= (get (exec-sim state fns 3)
                [(:token m) :o])
           true))  
    (is (= (get (exec-sim state fns 3)
                [(:token m) :i])
           ((uintm 8) 3)))  
    (is (= (get (exec-sim state fns 3)
                [(:token m) :x])
           (e :c)))))

(deftest cond-connect-test
  (let [e (enum #{:small :medium :big})
        m (module [:outputs [size (e :small)]
                   :feedback [i ((uintm 8) 0)]]
                  (connect i (inc i))
                  (cond
                    (< i 100) (connect size (e :small))
                    (< i 200) (connect size (e :medium))
                    :else (connect size (e :big))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 10)
                [(:token m) :size])
           (e :small)))  
    (is (= (get (exec-sim state fns 70)
                [(:token m) :size])
           (e :small)))  
    (is (= (get (exec-sim state fns 110)
                [(:token m) :size])
           (e :medium)))
    (is (= (get (exec-sim state fns 150)
                [(:token m) :size])
           (e :medium)))
    (is (= (get (exec-sim state fns 220)
                [(:token m) :size])
           (e :big)))))

(deftest condp-test
  (let [e (enum #{:a :b :c :d})
        f #(piplin.math/condp = (uninst (e %))
             :a ((uintm 8) 22)
             :b ((uintm 8) 32)
             :c ((uintm 8) 44)
             :d ((uintm 8) 0)
             ((uintm 8) 222))]
    (is (= ((make-sim-fn (f :a))) ((uintm 8) 22)))  
    (is (= ((make-sim-fn (f :b))) ((uintm 8) 32)))  
    (is (= ((make-sim-fn (f :c))) ((uintm 8) 44)))  
    (is (= ((make-sim-fn (f :d))) ((uintm 8) 0)))
    (is (thrown? ExceptionInfo
                 (piplin.math/condp = (uninst (e :a))
                   :a ((uintm 8) 22)
                   :e ((uintm 8) 32)
                   :c ((uintm 8) 44)
                   :d ((uintm 8) 0)))
        "invalid key to enum") 
    (is (thrown? ExceptionInfo
                 (piplin.math/condp = (uninst (e :a))
                   :a ((uintm 8) 22)
                   :b ((uintm 7) 32)
                   :c ((uintm 8) 44)
                   :d ((uintm 8) 0)))
        "not matching bitwidths") 
    ))

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
                  (connect o 33)
                  (mux2 (< cdr 7)
                        (connect v (cast u {:y {:car :c 
                                                :cdr (inc cdr)}}))
                        (connect v (cast u {:x ((uintm 5) 3)}))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [(:token m) :v])
           (u {:x ((uintm 5) 0)})))  
    (is (= (get (exec-sim state fns 0)
                [(:token m) :o])
           ((uintm 5) 22)))

    (is (= (get (exec-sim state fns 1)
                [(:token m) :v])
           (u {:y (cast b {:car :b
                           :cdr 3}) })))  

    (is (= (get (exec-sim state fns 1)
                [(:token m) :o])
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
                (connect o (uninst 33))
                (mux2 (< cdr 7)
                      (connect v (cast u {:y {:car :c 
                                              :cdr (inc cdr)}}))
                      (connect v (cast u {:x ((uintm 5) 3)}))))))
        [state fns] (make-sim m)]
    (is (= (get (exec-sim state fns 0)
                [(:token m) :v])
           (u {:x ((uintm 5) 0)})))  
    (is (= (get (exec-sim state fns 0)
                [(:token m) :o])
           ((uintm 5) 22)))

    (is (= (get (exec-sim state fns 1)
                [(:token m) :v])
           (u {:y (cast b {:car :b
                           :cdr 3}) })))

    (is (= (get (exec-sim state fns 1)
                [(:token m) :o])
           ((uintm 5) 22)))
    ))

