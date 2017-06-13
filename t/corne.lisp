(in-package :cl-user)
(defpackage corne-test
  (:use :cl
        :corne
        :prove))
(in-package :corne-test)

;; NOTE: To run this test file, execute `(asdf:test-system :corne)' in your Lisp.

(plan nil)


;
;;; parse
;(let* ((arg-a (make-instance
;                'argument :name "aaa" :help "help aaa"))
;       (arg-b (make-instance
;                'argument :name "bbb" :help "help bbb"))
;       (foo-opt-v (make-instance
;                    'option :name "verbose"
;                    :short "v" :long "verbose" :help "baz"))
;       (foo-cmd (make-instance
;                  'command :name "foo" :about "foobar"
;                  :options (list foo-opt-v)
;                  :arguments (list arg-a)))
;       (opt-h (make-instance
;                'option :name "help"
;                :short "h" :long "help" :help "print help"))
;       (opt-f (make-instance
;                'option :name "foo" :short "f" :long "foo"
;                :takes-value t :help "bar"))
;       (cmd (make-instance
;              'command :name "sample" :about "test"
;              :subcommands (list foo-cmd)
;              :options (list opt-h opt-f)
;              :arguments (list arg-a arg-b))))
;  (is-values (corne:parse cmd '("foo" "-v" "bar"))
;             (list '(("verbose" . t)) "foo" '("bar")))
;  (is-values (corne:parse cmd '("arg1" "arg2"))
;             (list nil nil '("arg1" "arg2")))
;  (is-error (corne:parse cmd '("-x")) 'corne:option-error)
;  (is-error (corne:parse cmd '("fewarg")) 'corne:argument-error)
;  (is-error (corne:parse cmd '("too" "much" "arg")) 'corne:argument-error))
;
;
;
;;;; command help

(finalize)
