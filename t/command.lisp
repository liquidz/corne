(in-package :cl-user)
(defpackage corne-test.command
  (:use :cl
        :corne.command
        :prove)
  (:import-from :corne.test.util
                :binding)
  (:import-from :corne.result
                :parse-result
                :equivalent))
(in-package :corne-test.command)

(plan nil)

(subtest "parse-subcommand"
  (let* ((cc (cmd "ccc"))
         (bc (cmd "bbb" :subcommands (list cc)))
         (ac (cmd "aaa" :subcommands (list bc))))
    (is-values
      (parse-subcommand ac '("bbb" "ccc"))
      (list cc nil "bbb.ccc"))
    (is-values
      (parse-subcommand ac '("bbb" "ddd"))
      (list bc '("ddd") "bbb"))
    (is-values
      (parse-subcommand ac '("bbb"))
      (list bc nil "bbb"))
    (is-values
      (parse-subcommand ac '("ddd"))
      (list ac '("ddd") nil))))

(subtest "parse-option"
  (let* ((ho  (opt "help" :short "h" :long "help"))
         (fo  (opt "file" :short "f" :long "file" :takes-value t))
         (cmd (cmd "aaa" :options (list ho fo))))
    (is-values
      (parse-option cmd '("-h" "aaa"))
      (list '("aaa") '(("help" . t))))
    (is-values
      (parse-option cmd '("-h"))
      (list nil '(("help" . t))))
    (is-values
      (parse-option cmd '("-f" "aaa" "bbb"))
      (list '("bbb") '(("file" . "aaa"))))
    (is-values
      (parse-option cmd '("-h" "-f" "aaa" "bbb"))
      (list '("bbb") '(("help" . t)
                       ("file" . "aaa"))))
    (is-values
      (parse-option cmd '("-x" "aaa"))
      (list '("-x" "aaa") '()))))

(subtest "parse-argument"
  (let* ((arg-a (arg "aaa" :help "foo"))
         (arg-b (arg "bbb" :help "bar"))
         (cmd (cmd "sample" :arguments (list arg-a arg-b))))
    (is-values (parse-argument cmd '("foo" "bar"))
               (list '(("aaa" . "foo") ("bbb" . "bar")) () ()))
    (is-values (parse-argument cmd '("foo"))
               (list '(("aaa" . "foo")) (list arg-b) ()))
    (is-values (parse-argument cmd '("foo" "bar" "baz"))
               (list '(("aaa" . "foo") ("bbb" . "bar")) () '("baz")))))

(subtest "parse"
  (let* ((arg-a (arg "aaa" :help "help aaa"))
         (arg-b (arg "bbb" :help "help bbb"))
         (foo-opt-v (opt "verbose" :short "v" :long "verbose"))
         (foo-cmd (cmd "foo" :options (list foo-opt-v) :arguments (list arg-a)))
         (opt-h (opt "help" :short "h" :long "help"))
         (opt-f (opt "foo" :short "f" :long "foo" :takes-value t))
         (cmd (cmd "sample" :subcommands (list foo-cmd)
                   :options (list opt-h opt-f)
                   :arguments (list arg-a arg-b))))
    (ok (equivalent
          (parse cmd '("foo" "-v" "bar"))
          (make-instance 'parse-result :option '(("verbose" . t)) :command "foo" :valid-arg '(("aaa" . "bar")))))
    (ok (equivalent
          (parse cmd '("arg1" "arg2"))
          (make-instance 'parse-result :valid-arg '(("aaa" . "arg1") ("bbb" . "arg2")))))
    (ok (equivalent
          (parse cmd '("fewarg"))
          (make-instance 'parse-result :valid-arg '(("aaa" . "fewarg")) :missing-arg (list arg-b))))
    (ok (equivalent
          (parse cmd '("too" "many" "arg"))
          (make-instance 'parse-result :valid-arg '(("aaa" . "too") ("bbb" . "many")) :too-many-arg '("arg"))))
    ;(is-error (parse cmd '("-x")) 'corne.option::option-error)
    ;(is-error (parse cmd '("fewarg")) 'corne.argument::argument-error)
    ;(is-error (parse cmd '("too" "much" "arg")) 'corne.argument::argument-error)
    ))

(subtest "add-option!"
  (let ((c (cmd "test")))
    (add-option! c (opt "foo" :short "f"))
    (is-values
      (parse-option c '("-f" "aaa"))
      (list '("aaa") '(("foo" . t))))))

(subtest "cmd"
  (is-values (parse-option (cmd "test") '("-h"))
             (list nil '(("help" . t))))
  (is-values (parse-option (cmd "test") '("--help"))
             (list nil '(("help" . t))))

  (binding ((*add-help-automatically* nil))
    (is-values (parse-option (cmd "test") '("-h"))
               (list '("-h") nil)))

  (binding ((*default-help-option* (opt "test" :short "t" :long "test")))
    (is-values (parse-option (cmd "test") '("-t"))
               (list nil '(("test" . t))))
    (is-values (parse-option (cmd "test") '("--test"))
               (list nil '(("test" . t))))))


(finalize)
