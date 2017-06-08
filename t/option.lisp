(in-package :cl-user)
(defpackage cola-test.option
  (:use :cl
        :cola.option
        :prove))
(in-package :cola-test.option)

(plan 3)

(subtest "optionp"
  (ok (optionp "-h"))
  (ok (optionp "--help"))
  (ok (not (optionp "help"))))

(subtest "@option"
  (is (macroexpand-1 '(@option foo -f --foo +takes-value "bar"))
      '(make-instance 'option :name "foo" :short "f" :long "foo" :takes-value t :help "bar"))
  (is (macroexpand-1 '(@option foo -f --foo "bar"))
      '(make-instance 'option :name "foo" :short "f" :long "foo" :help "bar"))
  (is (macroexpand-1 '(@option foo -f "bar"))
      '(make-instance 'option :name "foo" :short "f" :help "bar"))
  (is (macroexpand-1 '(@option foo --foo "bar"))
      '(make-instance 'option :name "foo" :long "foo" :help "bar"))
  (is (macroexpand-1 '(@option foo -f))
      '(make-instance 'option :name "foo" :short "f" :help "")))

(subtest "option help"
  (is (help (@option flag -f --flag "foo")) '("-f, --flag" . "foo"))
  (is (help (@option flag -f "foo")) '("-f" . "foo"))
  (is (help (@option flag --flag "foo")) '("--flag" . "foo"))
  (is (help (@option valu -v --value +takes-value "bar")) '("-v, --value <VALUE>" . "bar")))

(finalize)
