(in-package :cl-user)
(defpackage corne/test/option
  (:use :cl
        :corne/src/option
        :rove))
(in-package :corne/test/option)

(deftest optionp-test
  (ok (optionp "-h"))
  (ok (optionp "--help"))
  (ng (optionp "help")))

(deftest help-test
  (ok (equal (help (opt "flag" :short "f" :long "flag" :help "foo")) '("-f, --flag" . "foo")))
  (ok (equal (help (opt "flag" :short "f" :help "foo")) '("-f" . "foo")))
  (ok (equal (help (opt "flag" :long "flag" :help "foo")) '("--flag" . "foo")))
  (ok (equal (help (opt "valu" :short "v" :long "value" :takes-value t :help "bar")) '("-v, --value <VALUE>" . "bar"))))

