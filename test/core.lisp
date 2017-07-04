(in-package :cl-user)
(defpackage corne/test/core
  (:use :cl
        :corne
        :rove))
(in-package :corne/test/core)

(deftest command-case-test
  (ok (equal '(let ((it res))
                (cond ((equal "a" (get-subcommand it)) "b")
                      ((equal "c" (get-subcommand it)) "d")))
             (macroexpand-1 '(command-case res ("a" "b") ("c" "d")))))
  (ok (equal '(let ((it res))
                (cond ((equal "a" (get-subcommand it)) "b")
                      (t "d")))
             (macroexpand-1 '(command-case res ("a" "b") (otherwise "d"))))))
