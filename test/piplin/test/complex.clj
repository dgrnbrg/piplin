(ns piplin.test.complex
  (:refer-clojure :exclude [cond condp cast not = not= > >= < <= + - * inc dec bit-and bit-or bit-xor bit-not and or bit-shift-left bit-shift-right pos? neg? zero?])
  (:use [piplin.types bundle sfxpts bits boolean core-impl binops uintm complex])
  (:use [piplin types mux modules sim connect protocols verilog]
        plumbing.core)
  (:import clojure.lang.ExceptionInfo)
  (:use clojure.test
        piplin.test.util))

(deftest complex-on-sfxpts
  (let [sfxpts-type (sfxpts 8 8)
        complex-type (complex sfxpts-type sfxpts-type)
        c1 (cast complex-type [2.0 2.0])
        c2 (cast complex-type [2.8 1.5])
        c1+c1 (cast complex-type [4.0 4.0])
        c1+c2 (cast complex-type [4.8 3.5])
        c1*c1 (cast complex-type [0.0 8.0])
        c1*c2 (cast complex-type [2.59375 8.59375])
        c1*c2*c2 (cast complex-type [-5.63671875 27.92578125])
        c2*c2*c1 (cast complex-type [-5.643 27.9257])
        c1*c2*c2+c1 (cast complex-type [-3.63671875 29.92578125])]
    (is (= c1 (->> c1
                serialize
                (deserialize complex-type))))
    (is (= c2 (->> c2
                serialize
                (deserialize complex-type))))
    (is (= c1*c2*c2+c1
           (->> c1*c2*c2+c1
             serialize
             (deserialize complex-type))))
    (is (= (real-part c2) (cast sfxpts-type 2.8)))
    (is (= (imag-part c2) (cast sfxpts-type 1.5)))
    (is (= ((make-sim-fn (+ (uninst c1) c2))) c1+c2))
    (is (= ((make-sim-fn (* (uninst c1) c2))) c1*c2))
    (is (= (+ c1 c1) c1+c1))
    (is (= (+ c1 c2) c1+c2))
    (is (= (+ c2 c1) c1+c2))
    (is (= (* c1 c1) c1*c1))
    (is (= (* c1 c2) c1*c2))
    (is (= (* c2 c1) c1*c2))
    (is (= (* c1 c2 c2) c1*c2*c2))
    (is (= (* c2 c2 c1) c2*c2*c1))
    (is (= (+ c1 (* c1 c2 c2)) c1*c2*c2+c1))))

(let [sfxpts-type (sfxpts 8 8)
      complex-type (complex sfxpts-type sfxpts-type)
      const (cast complex-type [1.2 -0.73])]
  (def complex-add (modulize
                     {:x (fnk [x] (+ x x))}
                     {:x (cast complex-type [0.1 1.7])}))
  (def complex-mul (modulize
                     {:x (fnk [x] (* x x))}
                     {:x (cast complex-type [0.1 1.7])}))
  (def complex-fractal (modulize
                         {:x (fnk [x] (+ (* x x) const))}
                         {:x (cast complex-type [0.1 1.7])})))

(deftest complex-math-verilog
  (icarus-test (verify complex-add 500))
  (icarus-test (verify complex-mul 500))
  (icarus-test (verify complex-fractal 500)))
