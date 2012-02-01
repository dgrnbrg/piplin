(ns piplin.test.types
  (:use clojure.test)
  (:use clojure.tools.macro)
  (:use [slingshot.slingshot :only [throw+]])
  (:use piplin.types))

(deftest test-error
  (let [e (error "some bad" "stuff")]
    (is (error? e) "Error function should make an error")
    (is (:msg e) "some bad stuff")) "check stringification")

(defn-errors
  unify-error-arg-tester
  [x y z]
  "called fn")

(deftest test-unify-error-args
  (let [e (error "lol")]
    (symbol-macrolet [t-e (throw+ e)
                      t2-e (throw+ [e e])]
      (is (= (try-errors
               (unify-error-arg-tester 1 2 3)) "called fn") "no errors to unify")
      (is (= (try-errors
               (unify-error-arg-tester t-e 2 3)) [e]) "return single error")
      (is (= (try-errors
               (unify-error-arg-tester t-e t-e 3)) [e e]) "return several errors")
      (is (= (try-errors
               (unify-error-arg-tester 1 t-e t2-e)) [e e e]) "flatten errors")
      (is (= (try-errors
               (unify-error-arg-tester 1 t2-e t2-e)) [e e e e]) "merge errors")
      (is (= (try-errors
               (unify-error-arg-tester t2-e t-e t2-e)) [e e e e e]) "merge errors")
      (is (= (try-errors
               (unify-error-arg-tester t2-e t2-e t-e)) [e e e e e]) "merge errors"))))

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
