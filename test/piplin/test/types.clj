(ns piplin.test.types
  (:refer-clojure :exclude [cast])
  (:use clojure.test)
  (:use clojure.tools.macro)
  (:use [slingshot.slingshot :only [throw+]])
  (:use [piplin protocols types]))

(deftest test-error
  (let [e (error "some bad" "stuff")]
    (is (error? e) "Error function should make an error")
    (is (:msg e) "some bad stuff")) "check stringification")

(deftest test-let
  (let [e (error "lol")]
    (symbol-macrolet [t-e (throw+ e)]
      (is (= 3 (try-errors
                 (let [x 1 y 2]
                   (+ x y)))) "no errors")
      (is (= e (try-errors
                 (let [x 1 y t-e]
                   (+ x y)))) "error")
      (is (= e (try-errors
                 (let [x t-e y 1]
                   (+ x y)))) "error")
      (is (= e (try-errors
                 (let [x t-e y t-e]
                   (+ x y)))) "error"))))

;TODO: test type-unify

(deftest test-type-unify
  (is (error?
        (try-errors (type-unify :frob #{} #()))) "detect non-unifiable")
  (is (error?
        (try-errors (type-unify :j-long ['a 'b] 3))) "detect non-unifiable")
  (is (error?
        (try-errors (type-unify :x #{:kind :x} 3))) "detect non-unifiable"))
