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
    ))
(in-package :corne)

;(defun parse2 (cmd argv &key ((auto-help t)))
;  (let ((res (parse cmd argv)))
;    (if (get-option res "help")
;      (help cmd)
;      )
;    res
;    )
;  )
