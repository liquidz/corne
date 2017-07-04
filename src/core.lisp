(in-package :cl-user)
(defpackage corne
  (:nicknames :corne/src/core)
  (:use :cl)
  (:import-from :corne/src/command
                :*add-help-automatically*
                :*default-help-option*
                :*delm*
                :arg
                :cmd
                :help
                :opt)
  (:import-from :corne/src/result
                :get-arg
                :get-error
                :get-help
                :get-option
                :get-subcommand)
  (:export
    :*add-help-automatically*
    :*default-help-option*
    :*delm*
    :arg
    :cmd
    :command-case
    :get-arg
    :get-help
    :get-option
    :get-subcommand
    :it
    :opt
    :parse))
(in-package :corne)

(defmacro command-case (parse-result &rest clauses)
  (let ((clauses (mapcar (lambda (x)
                           (if (eql 'otherwise (first x))
                             `(t ,@(rest x))
                             `((equal ,(first x) (get-subcommand it))
                               ,@(rest x)))) clauses)))
    `(let ((it ,parse-result))
       (cond ,@clauses))))

(defun parse (cmd argv &key (auto-help t) (auto-error t))
  (let ((res (corne/src/command:parse cmd argv)))
    (when (and auto-help (get-option res "help"))
      (format t "~A" (get-help res))
      (uiop:quit 0))
    (when (and auto-help (= 0 (length argv)))
      (format t "~A" (get-help res))
      (uiop:quit 1))
    (when (and auto-error (get-error res))
      (format t "kiteru ~S~%" argv)
      (loop for e in (get-error res)
            do (format t "~A~%" e))
      (uiop:quit 1))
    res))
