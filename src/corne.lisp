(in-package :cl-user)
(defpackage corne
  (:use :cl)
  (:import-from :corne.command
                :*add-help-automatically*
                :*default-help-option*
                :*delm*
                :arg
                ;:argument-error
                :cmd
                :help
                :opt
                :parse
                ;:option-error
                )
  (:import-from :corne.result
                :get-subcommand
                :get-option
                :get-arg
                :get-help
                :get-error
                )
  (:export
    :*add-help-automatically*
    :*default-help-option*
    :*delm*
    :arg
    ;:argument-error
    :cmd
    :help
    :opt
    :parse
    ;:option-error

    :get-subcommand
    :get-option
    :get-arg

    :parse2
    ))
(in-package :corne)

 (defun exit (&optional code)
   #+sbcl (sb-ext:exit :code code)
   #+ccl (ccl:quit)
   #+ecl (si:quit)
   #+abcl (cl-user::quit)
   #+allegro (excl:exit code)
   #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
   #+cmu (ext:quit code))

(defun parse2 (cmd argv &key (auto-help t) (auto-error t))
  (let ((res (parse cmd argv)))
    (when (and auto-help (get-option res "help"))
      (format t "~A" (get-help res))
      (exit 0))
    (when (and auto-error (get-error res))
      (loop for e in (get-error res)
            do (format t "~A~%" e))
      (exit 1)
      )
    res))
