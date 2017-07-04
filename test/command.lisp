(in-package :cl-user)
(defpackage corne/test/command
  (:use :cl
        :corne/src/command
        :rove)
  (:import-from :corne/src/result
                :parse-result
                :equivalent))
(in-package :corne/test/command)

(defmacro binding (binds &rest body)
  (let* ((syms (loop for _ in binds collect (gensym)))
         (lets (mapcar (lambda (b s) (list s (first b))) binds syms))
         (before-setfs (mapcar (lambda (b) (cons 'setf b)) binds))
         (after-setfs (mapcar (lambda (b s) (list 'setf (first b) s)) binds syms)))
    `(let ,lets
       ,@before-setfs
       ,@body
       ,@after-setfs)))

(defmacro values-equal (vs ls)
  `(equal (multiple-value-list ,vs) ,ls))

(deftest parse-subcommand-test
  (let* ((cc (cmd "ccc"))
         (bc (cmd "bbb" :subcommands (list cc)))
         (ac (cmd "aaa" :subcommands (list bc))))
    (ok (values-equal
          (parse-subcommand ac '("bbb" "ccc"))
          (list cc nil)))
    (ok (values-equal
          (parse-subcommand ac '("bbb" "ddd"))
          (list bc '("ddd"))))
    (ok (values-equal
          (parse-subcommand ac '("bbb"))
          (list bc nil)))
    (ok (values-equal
          (parse-subcommand ac '("ddd"))
          (list ac '("ddd"))))))

(deftest parse-option-test
  (testing "with option setting"
    (let* ((ho  (opt "help" :short "h" :long "help"))
           (fo  (opt "file" :short "f" :long "file" :takes-value t))
           (cmd (cmd "aaa" :options (list ho fo))))
      (ok (values-equal
            (parse-option cmd '("-h" "aaa"))
            (list '("aaa") '(("help" . t)))))
      (ok (values-equal
            (parse-option cmd '("-h"))
            (list nil '(("help" . t)))))
      (ok (values-equal
            (parse-option cmd '("-f" "aaa" "bbb"))
            (list '("bbb") '(("file" . "aaa")))))
      (ok (values-equal
            (parse-option cmd '("-h" "-f" "aaa" "bbb"))
            (list '("bbb") '(("help" . t)
                             ("file" . "aaa")))))
      (ok (values-equal
            (parse-option cmd '("-x" "aaa"))
            (list '("-x" "aaa") '())))))
  (testing "without option setting"
    (let ((cmd (cmd "aaa")))
      (ok (values-equal (parse-option cmd '("-x"))
                        (list '("-x") '()))))))

(deftest parse-argument-test
  (testing "with argument setting"
    (let* ((arg-a (arg "aaa" :help "foo"))
           (arg-b (arg "bbb" :help "bar"))
           (cmd (cmd "sample" :arguments (list arg-a arg-b))))
      (ok (values-equal (parse-argument cmd '("foo" "bar"))
                        (list '(("aaa" . "foo") ("bbb" . "bar")) () ())))
      (ok (values-equal (parse-argument cmd '("foo"))
                        (list '(("aaa" . "foo")) (list arg-b) ())))
      (ok (values-equal (parse-argument cmd '("foo" "bar" "baz"))
                        (list '(("aaa" . "foo") ("bbb" . "bar")) () '("baz"))))))

  (testing "without argument setting"
    (let ((cmd (cmd "sample")))
      (ok (values-equal (parse-argument cmd '("foo"))
                        '(() () ("foo"))))
      )
    )
  )

(deftest parse-test
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
          (make-instance 'parse-result :valid-arg '(("aaa" . "too") ("bbb" . "many")) :too-many-arg '("arg"))))))

(deftest add-option!-test
  (let ((c (cmd "test")))
    (add-option! c (opt "foo" :short "f"))
    (ok (values-equal
          (parse-option c '("-f" "aaa"))
          (list '("aaa") '(("foo" . t)))))))

(deftest cmd-test
  (testing "parent instance setting"
    (let* ((child (cmd "baz"))
           (parent (cmd "foo" :subcommands (list child))))
      (ok (eq parent (get-parent child)))))

  (testing "auto help option"
    (ok (values-equal (parse-option (cmd "test") '("-h"))
                      (list nil '(("help" . t)))))
    (ok (values-equal (parse-option (cmd "test") '("--help"))
                      (list nil '(("help" . t))))))

  (testing "disabling auto help option"
    (binding ((*add-help-automatically* nil))
             (ok (values-equal (parse-option (cmd "test") '("-h"))
                               (list '("-h") nil)))))

  (testing "custom help option"
    (binding ((*default-help-option* (opt "test" :short "t" :long "test")))
             (ok (values-equal (parse-option (cmd "test") '("-t"))
                               (list nil '(("test" . t)))))
             (ok (values-equal (parse-option (cmd "test") '("--test"))
                               (list nil '(("test" . t))))))))


