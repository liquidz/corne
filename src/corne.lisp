(in-package :cl-user)
(defpackage corne
  (:use :cl)
  (:import-from :corne.command
                :*add-help-automatically*
                :*default-help-option*
                :*delm*
                :arg
                :cmd
                :help
                :opt)
  (:import-from :corne.result
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

(defun exit (&optional code)
  #+sbcl (sb-ext:exit :code code)
  #+ccl (ccl:quit)
  #+ecl (si:quit)
  #+abcl (cl-user::quit)
  #+allegro (excl:exit code)
  #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
  #+cmu (ext:quit code))

(defmacro command-case (parse-result &rest clauses)
  (let ((clauses (mapcar (lambda (x)
                           (if (eql 'otherwise (first x))
                             `(t ,@(rest x))
                             `((equal ,(first x) (get-subcommand ,parse-result))
                               ,@(rest x)))) clauses)))
    `(let ((it ,parse-result))
       (cond ,@clauses))))

(defun parse (cmd argv &key (auto-help t) (auto-error t))
  (let ((res (corne.command:parse cmd argv)))
    (when (and auto-help (get-option res "help"))
      (format t "~A" (get-help res))
      (exit 0))
    (when (and auto-help (= 0 (length argv)))
      (format t "~A" (get-help res))
      (exit 1))
    (when (and auto-error (get-error res))
      (loop for e in (get-error res)
            do (format t "~A~%" e))
      (exit 1))
    res))
