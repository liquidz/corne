(in-package :cl-user)
(defpackage corne-test.argument
  (:use :cl
        :corne.argument
        :prove))
(in-package :corne-test.argument)

;; NOTE: To run this test file, execute `(asdf:test-system :corne)' in your Lisp.

(plan 2)

(subtest "@arg"
  (is (macroexpand-1 '(@arg foo "bar"))
      '(make-instance 'argument :name "foo" :help "bar"))
  (is (macroexpand-1 '(@arg foo))
      '(make-instance 'argument :name "foo" :help ""))
  (is (macroexpand-1 '(@arg foo bar))
      '(make-instance 'argument :name "foo" :help "")))

(subtest "argument help"
  (is (help (@arg foo "bar")) '("<FOO>" . "bar")))

(finalize)
