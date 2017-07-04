(in-package :cl-user)
(defpackage corne/test/result
  (:use :cl
        :corne/src/result
        :rove))
(in-package :corne/test/result)

(deftest equivalent-test
  (ok (equivalent
        (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")
        (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")))
  (ng (equivalent
        (make-instance 'parse-result :option "a" :command "b" :valid-arg "c" :missing-arg "d" :too-many-arg "e")
        (make-instance 'parse-result :option "b" :command "c" :valid-arg "d" :missing-arg "e" :too-many-arg "f"))))

(deftest get-option-test
  (testing "with options"
    (let ((res (make-instance 'parse-result :option '(("foo" . "bar")))))
      (ok (equal (get-option res "foo") "bar"))
      (ok (equal (get-option res "bar") nil))))
  (testing "without options"
    (let ((res (make-instance 'parse-result)))
      (ok (equal (get-option res "foo") nil))
      (ok (equal (get-option res "bar") nil)))))

(deftest get-arg-test
  (let ((res (make-instance 'parse-result :valid-arg '(("foo" . "bar")))))
    (ok (equal (get-arg res "foo") "bar"))
    (ok (equal (get-arg res "bar") nil))))

(deftest get-error-test
  (testing "no result"
    (ng (get-error (make-instance 'parse-result))))
  (testing "with valid args"
    (ng (get-error (make-instance 'parse-result :valid-arg '(("foo" . "bar"))))))
  (testing "invalid option error"
    (ok (search "Invalid option: "
                (first (get-error (make-instance 'parse-result :valid-arg '(("help" . "-h"))))))))
  (testing "missing argument error"
    (skip "skipped for now"))
  (testing "too many argument error"
    (skip "skipped for now")))

