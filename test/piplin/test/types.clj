(ns piplin.test.types
  (:use clojure.test)
  (:use piplin.types))

(deftest test-error
  (let [e (error "some bad" "stuff")]
    (is (error? e) "Error function should make an error")
    (is (:msg e) "some bad stuff")) "check stringification")

(deftest test-unify-error-args
  (letfn [(f-inner [& more] "called fn")]
    (let [f (unify-error-args f-inner)
          e (error "lol")]
      (is (= (f 1 2 3) "called fn") "no errors to unify")
      (is (= (f e 2 3) [e]) "return single error")
      (is (= (f e e 3) [e e]) "return several errors")
      (is (= (f 1 e [e e]) [e e e]) "flatten errors")
      (is (= (f 1 [e e] [e e]) [e e e e]) "merge errors")
      (is (= (f [e e] e [e e]) [e e e e e]) "merge errors")
      (is (= (f [e e] [e e] e) [e e e e e]) "merge errors"))))

(deftest test-let-safe
  (let [e (error "lol")]
    (is (= 3 (let-safe [x 1 y 2]
              (+ x y))) "no errors")
    (is (= e (let-safe [x 1 y e]
              (+ x y))) "error")
    (is (= e (let-safe [x e y 1]
              (+ x y))) "error")
    (is (= e (let-safe [x e y e]
              (+ x y))) "error")))

;TODO: test type-unify

(deftest test-type-unify
  (is (error?
        (type-unify :frob #{} #())) "detect non-unifiable")
  (is (error?
        (type-unify :j-long ['a 'b] 3)) "detect non-unifiable")
  (is (error?
        (type-unify :x #{:kind :x} 3)) "detect non-unifiable"))
