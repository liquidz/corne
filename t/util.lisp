(in-package :cl-user)
(defpackage corne-test.util
  (:use :cl
        :corne.util
        :prove))
(in-package :corne-test.util)

(plan 3)

(subtest "join"
  (is (join '("a" "b" "c")) "abc")
  (is (join '("a" "b" "c") "-") "a-b-c")
  (is nil (join nil)))

(subtest "repeat"
  (is (repeat "a" 3) '("a" "a" "a")))

(subtest "align-cons"
  (let ((cons '(("foo" . "bar") ("hello" . "world"))))
    (is (align-cons cons)
        '("foo      bar" "hello    world"))
    (is (align-cons cons :space 2)
        '("foo    bar" "hello  world"))
    (is (align-cons cons :delm "-")
        '("foo------bar" "hello----world"))))

(finalize)
