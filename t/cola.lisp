(in-package :cl-user)
(defpackage cola-test
  (:use :cl
        :cola
        :prove))
(in-package :cola-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cola)' in your Lisp.

(plan nil)

; optionp
(ok (cola:optionp "-h"))
(ok (cola:optionp "--help"))
(ok (not (cola:optionp "help")))

;;; @option

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

(subtest "@arg"
  (is (macroexpand-1 '(@arg foo "bar"))
      '(make-instance 'argument :name "foo" :help "bar"))
  (is (macroexpand-1 '(@arg foo))
      '(make-instance 'argument :name "foo" :help ""))
  (is (macroexpand-1 '(@arg foo bar))
      '(make-instance 'argument :name "foo" :help "")))

(subtest "@command"
  (is (macroexpand-1 '(@command foo :about "bar" :version "1.0"))
      '(make-instance 'command :name "foo" :about "bar" :version "1.0"))
  (is (macroexpand-1 '(@command foo))
      '(make-instance 'command :name "foo"))
  (is (macroexpand-1 '(@command foo (@option foo) (@option bar)))
      '(make-instance 'command :name "foo"
                      :options (list (@option foo) (@option bar))))
  (is (macroexpand-1 '(@command foo (@option foo) (@arg bar)))
      '(make-instance 'command :name "foo"
                      :options (list (@option foo))
                      :arguments (list (@arg bar))))
  (is (macroexpand-1 '(@command foo (@command bar (@option baz)) (@option foo) (@arg bar)))
      '(make-instance 'command :name "foo"
                      :subcommands (list (@command bar (@option baz)))
                      :options (list (@option foo)) :arguments (list (@arg bar)))))

;;;; parse-subcommand
;(let* ((cc (make-instance 'command :name "ccc"))
;       (bc (make-instance 'command :name "bbb" :subcommands (list cc)))
;       (ac (make-instance 'command :name "aaa" :subcommands (list bc))))
;  (is-values
;    (cola:parse-subcommand ac '("bbb" "ccc"))
;    (list cc '() "bbb.ccc"))
;  (is-values
;    (cola:parse-subcommand ac '("bbb" "ddd"))
;    (list bc '("ddd") "bbb"))
;  (is-values
;    (cola:parse-subcommand ac '("ddd"))
;    (list ac '("ddd") nil)))
;
;;;; parse-option
;(let* ((ho (make-instance 'option :name "help" :short "h" :long "help"))
;       (fo (make-instance 'option :name "file" :short "f" :long "file" :takes-value t))
;       (cmd (make-instance 'command :name "aaa" :options (list ho fo))))
;  (is-values
;    (cola:parse-option cmd '("-h" "aaa"))
;    (list '("aaa") '(("help" . t))))
;  (is-values
;    (cola:parse-option cmd '("-f" "aaa" "bbb"))
;    (list '("bbb") '(("file" . "aaa"))))
;  (is-values
;    (cola:parse-option cmd '("-h" "-f" "aaa" "bbb"))
;    (list '("bbb") '(("help" . t)
;                         ("file" . "aaa"))))
;  (is-values
;    (cola:parse-option cmd '("-x" "aaa"))
;    (list '("-x" "aaa") '())))
;
;;; parse-argument
;(let* ((arg-a (make-instance 'argument :name "aaa" :help "foo"))
;       (arg-b (make-instance 'argument :name "bbb" :help "bar"))
;       (cmd (make-instance
;              'command :name "sample" :about "test"
;              :arguments (list arg-a arg-b))))
;  (is-values (parse-argument cmd '("foo" "bar"))
;             (list '("foo" "bar") () ()))
;  (is-values (parse-argument cmd '("foo"))
;             (list '("foo") (list arg-b) ()))
;  (is-values (parse-argument cmd '("foo" "bar" "baz"))
;             (list '("foo" "bar") () '("baz"))))
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
;  (is-values (cola:parse cmd '("foo" "-v" "bar"))
;             (list '(("verbose" . t)) "foo" '("bar")))
;  (is-values (cola:parse cmd '("arg1" "arg2"))
;             (list nil nil '("arg1" "arg2")))
;  (is-error (cola:parse cmd '("-x")) 'cola:option-error)
;  (is-error (cola:parse cmd '("fewarg")) 'cola:argument-error)
;  (is-error (cola:parse cmd '("too" "much" "arg")) 'cola:argument-error))
;
;;; option help
;(let (
;      (flg-opt (make-instance 'option :name "flag" :short "f" :long "flag" :help "foo"))
;      (flg-opt-only-short (make-instance 'option :name "flag" :short "f" :help "foo"))
;      (flg-opt-only-long (make-instance 'option :name "flag" :long "flag" :help "foo"))
;      (val-opt (make-instance 'option :name "value" :short "v" :long "value" :takes-value t :help "bar"))
;      )
;  (is (cola:help flg-opt) '("-f, --flag" . "foo"))
;  (is (cola:help flg-opt-only-short) '("-f" . "foo"))
;  (is (cola:help flg-opt-only-long) '("--flag" . "foo"))
;  (is (cola:help val-opt) '("-v, --value <VALUE>" . "bar")))
;
;;;; argument help
;(let ((arg (make-instance 'argument :name "foo" :help "bar")))
;  (is (cola:help arg) '("<FOO>" . "bar")))
;
;;;; command help

(finalize)
