(in-package :cl-user)
(defpackage corne-test.command
  (:use :cl
        :corne.command
        :prove))
(in-package :corne-test.command)

(plan nil)

(subtest "@command"
  (is (macroexpand-1 '(@command foo :about "bar" :version "1.0"))
      '(make-instance 'corne.command::command :name "foo" :about "bar" :version "1.0"))
  (is (macroexpand-1 '(@command foo))
      '(make-instance 'corne.command::command :name "foo"))
  (is (macroexpand-1 '(@command foo (@option foo) (@option bar)))
      '(make-instance 'corne.command::command :name "foo"
                      :options (list (@option foo) (@option bar))))
  (is (macroexpand-1 '(@command foo (@option foo) (@arg bar)))
      '(make-instance 'corne.command::command :name "foo"
                      :options (list (@option foo))
                      :arguments (list (@arg bar))))
  (is (macroexpand-1 '(@command foo (@command bar (@option baz)) (@option foo) (@arg bar)))
      '(make-instance 'corne.command::command :name "foo"
                      :subcommands (list (@command bar (@option baz)))
                      :options (list (@option foo)) :arguments (list (@arg bar)))))

(subtest "parse-subcommand"
  (let* (
         ;(cc (@command ccc))
         ;(bc (@command bbb :subcommands (list cc)))
         ;(ac (@command aaa :subcommands (list bc)))
         (ac (@command aaa (@command bbb (@command ccc))))
         )
    (is-values
      (parse-subcommand ac '("bbb" "ccc"))
      (list cc '() "bbb.ccc"))
    (is-values
      (parse-subcommand ac '("bbb" "ddd"))
      (list bc '("ddd") "bbb"))
    (is-values
      (parse-subcommand ac '("ddd"))
      (list ac '("ddd") nil))))

;(subtest "parse-option"
;  (let* ((ho (make-instance 'option :name "help" :short "h" :long "help"))
;         (fo (make-instance 'option :name "file" :short "f" :long "file" :takes-value t))
;         (cmd (make-instance 'command :name "aaa" :options (list ho fo))))
;    (is-values
;      (parse-option cmd '("-h" "aaa"))
;      (list '("aaa") '(("help" . t))))
;    (is-values
;      (parse-option cmd '("-f" "aaa" "bbb"))
;      (list '("bbb") '(("file" . "aaa"))))
;    (is-values
;      (parse-option cmd '("-h" "-f" "aaa" "bbb"))
;      (list '("bbb") '(("help" . t)
;                       ("file" . "aaa"))))
;    (is-values
;      (parse-option cmd '("-x" "aaa"))
;      (list '("-x" "aaa") '()))))
;
;(subtest "parse-argument"
;  (let* ((arg-a (make-instance 'argument :name "aaa" :help "foo"))
;         (arg-b (make-instance 'argument :name "bbb" :help "bar"))
;         (cmd (make-instance
;                'command :name "sample" :about "test"
;                :arguments (list arg-a arg-b))))
;    (is-values (parse-argument cmd '("foo" "bar"))
;               (list '("foo" "bar") () ()))
;    (is-values (parse-argument cmd '("foo"))
;               (list '("foo") (list arg-b) ()))
;    (is-values (parse-argument cmd '("foo" "bar" "baz"))
;               (list '("foo" "bar") () '("baz")))))

(finalize)
