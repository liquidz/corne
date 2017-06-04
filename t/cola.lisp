(in-package :cl-user)
(defpackage cola-test
  (:use :cl
        :cola
        :prove))
(in-package :cola-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cola)' in your Lisp.

(plan nil)

(ok (typep (cola:option :name "help" :short "h" :long "help" :help "print help") 'option))

; optionp
(ok (cola:optionp "-h"))
(ok (cola:optionp "--help"))
(ok (not (cola:optionp "help")))

;(defmacro parser-test (parser cmd args &rest tests)
;  (let ((result-name (gensym)))
;    `(let* ((,result-name (,parser ,cmd ,args))
;            (command (first ,result-name))
;            (arguments (second ,result-name)))
;       (multiple-value-bind (options subcommand arguments) (result->values (third ,result-name))
;         ,@tests))))

;;; parse-subcommand
(let* ((cc (make-instance 'command :name "ccc"))
       (bc (make-instance 'command :name "bbb" :subcommands (list cc)))
       (ac (make-instance 'command :name "aaa" :subcommands (list bc))))
  (is-values
    (cola:parse-subcommand ac '("bbb" "ccc"))
    (list cc '() "bbb.ccc"))
  (is-values
    (cola:parse-subcommand ac '("bbb" "ddd"))
    (list bc '("ddd") "bbb"))
  (is-values
    (cola:parse-subcommand ac '("ddd"))
    (list ac '("ddd") nil)))

;;; parse-option
(let* ((ho (make-instance 'option :name "help" :short "h" :long "help"))
       (fo (make-instance 'option :name "file" :short "f" :long "file" :takes-value t))
       (cmd (make-instance 'command :name "aaa" :options (list ho fo))))
  (is-values
    (cola:parse-option cmd '("-h" "aaa"))
    (list '("aaa") '(("help" . t))))
  (is-values
    (cola:parse-option cmd '("-f" "aaa" "bbb"))
    (list '("bbb") '(("file" . "aaa"))))
  (is-values
    (cola:parse-option cmd '("-h" "-f" "aaa" "bbb"))
    (list '("bbb") '(("help" . t)
                         ("file" . "aaa"))))
  (is-values
    (cola:parse-option cmd '("-x" "aaa"))
    (list '("-x" "aaa") '()))
  )

;; parse
(let* ((foo-opt-v (make-instance
                    'option :name "verbose"
                    :short "v" :long "verbose" :help "baz"))
       (foo-cmd (make-instance
                  'command :name "foo" :about "foobar"
                  :options (list foo-opt-v)))
       (opt-h (make-instance
                'option :name "help"
                :short "h" :long "help" :help "print help"))
       (opt-f (make-instance
                'option :name "foo" :short "f" :long "foo"
                :takes-value t :help "bar"))
       (cmd (make-instance
              'command :name "sample" :about "test"
              :subcommands (list foo-cmd)
              :options (list opt-h opt-f))))
  (is-values (cola:parse cmd '("foo" "-v" "bar"))
             (list '(("verbose" . t)) "foo" '("bar")))
  (is-error (cola:parse cmd '("-x")) 'simple-error)
 )

(finalize)
