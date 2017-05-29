(in-package :cl-user)
(defpackage cola-test
  (:use :cl
        :cola
        :prove))
(in-package :cola-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cola)' in your Lisp.

(plan 1)

(ok (typep (cola:option :name "help" :short "h" :long "help" :help "print help") 'command))

;; blah blah blah.

(finalize)
