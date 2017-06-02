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

(defmacro parser-test (parser cmd args &rest tests)
  (let ((result-name (gensym)))
    `(let* ((,result-name (,parser ,cmd ,args))
            (command (first ,result-name))
            (arguments (second ,result-name)))
       (multiple-value-bind (options subcommand arguments) (result->values (third ,result-name))
         ,@tests))))


; parse-command
(let* ((cc (make-instance 'command :name "ccc"))
       (bc (make-instance 'command :name "bbb" :subcommands (list cc)))
       (ac (make-instance 'command :name "aaa" :subcommands (list bc))))
  (parser-test cola:parse-command ac '("bbb" "ccc")
    )
  (let ((ret (cola:parse-command a '("bbb" "ccc"))))
    (is (first ret) c)
    (is (second ret) '())
    (is (gethash "subcommand" (third ret)) "bbb.ccc"))
  (let ((ret (cola:parse-command a '("bbb" "ddd"))))
    (is (first ret) b)
    (is (second ret) '("ddd"))
    (is (gethash "subcommand" (third ret)) "bbb"))
  (let ((ret (cola:parse-command a '("ddd"))))
    (is (first ret) a)
    (is (second ret) '("ddd"))
    (ok (not (gethash "subcommand" (third ret))))))

;; parse
;(let* ((foo-opt-v (make-instance 'option :short "v" :long "verbose" :help "baz"))
;       (foo-cmd   (make-instance 'command :name "foo" :about "foobar" :options (list foo-opt-v)))
;       (opt-h     (make-instance 'option :name "help" :short "h" :long "help" :help "print help"))
;       (opt-f     (make-instance 'option :name "foo" :short "f" :long "foo" :takes-value t :help "bar"))
;       (cmd       (make-instance 'command :name "sample" :about "test"
;                                 :subcommands (list foo-cmd)
;                                 :options (list opt-h opt-f))))
;  (print (cola:parse cmd '("foo" "-v" "bar")
;  ;(multiple-value-bind (a b c) (cola:parse cmd '("foo" "-v" "bar"))
;  ;  (is b "foo")
;  ;  ;(print c)
;  ;  )
;  ))

(finalize)
