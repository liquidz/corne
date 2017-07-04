(in-package :cl-user)
(defpackage corne/test/argument
  (:use :cl
        :corne/src/argument
        :rove))
(in-package :corne/test/argument)

(deftest help-test
  (ok (equal (help (arg "foo")) '("<FOO>" . "")))
  (ok (equal (help (arg "foo" :help "bar")) '("<FOO>" . "bar"))))

