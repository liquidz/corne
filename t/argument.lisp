(in-package :cl-user)
(defpackage cola-test.argument
  (:use :cl
        :cola.argument
        :prove))
(in-package :cola-test.argument)

;; NOTE: To run this test file, execute `(asdf:test-system :cola)' in your Lisp.

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
