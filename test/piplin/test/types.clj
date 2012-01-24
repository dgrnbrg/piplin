(ns piplin.types.test
  (:use clojure.test)
  (:use piplin.types))

(deftest test-error
  (let [e (error "some bad" "stuff")]
    (is (error? e))
    (is (:msg e) "some bad stuff")))
