(in-package :cl-user)
(defpackage corne-test.option
  (:use :cl
        :corne.option
        :prove))
(in-package :corne-test.option)

(plan 3)

(subtest "optionp"
  (ok (optionp "-h"))
  (ok (optionp "--help"))
  (ok (not (optionp "help"))))

(subtest "option help"
  (is (help (opt "flag" :short "f" :long "flag" :help "foo")) '("-f, --flag" . "foo"))
  (is (help (opt "flag" :short "f" :help "foo")) '("-f" . "foo"))
  (is (help (opt "flag" :long "flag" :help "foo")) '("--flag" . "foo"))
  (is (help (opt "valu" :short "v" :long "value" :takes-value t :help "bar")) '("-v, --value <VALUE>" . "bar")))

(finalize)
