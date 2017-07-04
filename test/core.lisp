(in-package :cl-user)
(defpackage corne/test/core
  (:use :cl
        :corne
        :rove))
(in-package :corne/test/core)

(deftest command-case-test
  (ok (equal '(let ((it res))
                (cond ((equal "a" (get-subcommand res)) "b")
                      ((equal "c" (get-subcommand res)) "d")))
             (macroexpand-1 '(command-case res ("a" "b") ("c" "d")))))
  (ok (equal '(let ((it res))
                (cond ((equal "a" (get-subcommand res)) "b")
                      (t "d")))
             (macroexpand-1 '(command-case res ("a" "b") (otherwise "d"))))))
