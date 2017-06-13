(in-package :cl-user)
(defpackage corne
  (:use :cl)
  (:import-from :corne.argument
                :arg
                :argument-error
                )
  (:import-from :corne.option
                :opt
                :option-error
                )
  (:export
    ;:command
    :opt
    :arg
    ;:parse-subcommand
    ;:find-option
    ;:parse-option
    ;:parse-argument
    ;:parse
    ;:help
    ))
(in-package :corne)

(defvar *delm* "    ")





(defun join (coll delm)
  (reduce (lambda (res s)
            (format nil "~A~A~A" res delm s))
          coll))

;(defmethod help ((cmd command))
;  (let* ((name (get-name cmd))
;         (about (slot-value cmd 'about))
;         (version (slot-value cmd 'version))
;         (subcommands (slot-value cmd 'subcommands))
;         (options (slot-value cmd 'options))
;         (arguments (slot-value cmd 'arguments)))
;    (list
;      (format nil "~A ~A~%" name ver)
;      (format nil "USAGE: ~A~A~A~A"
;              name
;              (if options " [OPTIONS]" "")
;              (if subcommands " [SUBCOMMAND]" "")
;              (if (and arguments (not subcommands))
;                (format nil " ~A" (join (mapcar #'to-str  arguments) ", "))
;                ""))
;      (if subcommands
;        (format nil "SUBCOMMANDS:")
;        )
;      (if options
;        (format nil "OPTIONS:")
;        )
;      )
;    )
;  )

