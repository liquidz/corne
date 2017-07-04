(in-package :cl-user)
(defpackage corne/test/util
  (:use :cl
        :corne/src/util
        :rove))
(in-package :corne/test/util)

(deftest join-test
  (ok (equal (join '("a" "b" "c")) "abc"))
  (ok (equal (join '("a" "b" "c") "-") "a-b-c"))
  (ng (join nil)))

(deftest repeat-test
  (ok (equal (repeat "a" 3) '("a" "a" "a"))))

(deftest align-cons-test
  (let ((cons '(("foo" . "bar") ("hello" . "world"))))
    (ok (equal (align-cons cons)
               '("foo      bar" "hello    world")))
    (ok (equal (align-cons cons :space 2)
               '("foo    bar" "hello  world")))
    (ok (equal (align-cons cons :delm "-")
               '("foo------bar" "hello----world")))))

