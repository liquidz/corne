(in-package :cl-user)
(defpackage cola-test
  (:use :cl
        :cola
        :prove))
(in-package :cola-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cola)' in your Lisp.

(plan 4)

(ok (typep (cola:option :name "help" :short "h" :long "help" :help "print help") 'option))

; optionp
(ok (cola:optionp "-h"))
(ok (cola:optionp "--help"))
(ok (not (cola:optionp "help")))

; parse
(let* ((foo-opt-v (make-instance 'option :short "v" :long "verbose" :help "baz"))
       (foo-cmd   (make-instance 'command :name "foo" :about "foobar" :options (list foo-opt-v)))
       (opt-h     (make-instance 'option :name "help" :short "h" :long "help" :help "print help"))
       (opt-f     (make-instance 'option :name "foo" :short "f" :long "foo" :takes-value t :help "bar"))
       (cmd       (make-instance 'command :name "sample" :about "test"
                                 :subcommands (list foo-cmd)
                                 :options (list opt-h opt-f))))
  (print (cola:parse cmd '("foo" "-v" "bar")
  ;(multiple-value-bind (a b c) (cola:parse cmd '("foo" "-v" "bar"))
  ;  (is b "foo")
  ;  ;(print c)
  ;  )
  ))

(finalize)
