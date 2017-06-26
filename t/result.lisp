(in-package :cl-user)
(defpackage corne-test.result
  (:use :cl
        :corne.result
        :prove))
(in-package :corne-test.result)

(plan nil)

(subtest "equivalent"
  (ok (equivalent
        (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")
        (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")))
  (ok (not (equivalent
             (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")
             (make-instance 'parse-result :option "b" :command "c" :valid-arg "d" :missing-arg "e" :too-many-arg "f")))))

(subtest "get-option"
  (let ((res (make-instance 'parse-result :option '(("foo" . "bar")))))
    (is (get-option res "foo") "bar")
    (is (get-option res "bar") nil))
  (let ((res (make-instance 'parse-result)))
    (is (get-option res "foo") nil)
    (is (get-option res "bar") nil)))

(subtest "get-arg"
  (let ((res (make-instance 'parse-result :valid-arg '(("foo" . "bar")))))
    (is (get-arg res "foo") "bar")
    (is (get-arg res "bar") nil)))

(subtest "get-error"
  (is (get-error (make-instance 'parse-result))
      nil)
  (is (get-error (make-instance 'parse-result :valid-arg '(("foo" . "bar"))))
      nil)
  (like (first (get-error (make-instance 'parse-result :valid-arg '(("help" . "-h")))))
        "Invalid option: ")
  ; TODO: missing-arg
  ; TODO: too-many-arg
  )

(finalize)
