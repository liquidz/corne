(in-package :cl-user)
(defpackage corne.command
  (:use :cl)
  (:export
    :commmand
    :2command)
  )
(in-package :corne.command)

(defclass command ()
  ((name :initform "" :initarg :name :reader get-name)
   (about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)
   (arguments   :initform () :initarg :arguments)))

;;; (@command name
;;;   :about "aaa"
;;;   :version "1.0"
;;;   (@command sub ...)
;;;   (@option ...) (@option ...)
;;;   (@arg ...) (@arg ...))
(defmacro @command (name &rest args)
  (let* ((name (string-downcase (symbol-name name)))
         (keys (loop for x in args while (not (listp x)) collect x))
         (lists (loop for x in args when (listp x) collect x))
         (subcommands (loop for x in lists when (eql '@command (car x)) collect x))
         (options (loop for x in lists when (eql '@option (car x)) collect x))
         (arguments (loop for x in lists when (eql '@arg (car x)) collect x)))
    `(make-instance 'command
                    :name ,name
                    ,@keys
                    ,@(if subcommands (list :subcommands (cons 'list subcommands)))
                    ,@(if options (list :options (cons 'list options)))
                    ,@(if arguments (list :arguments (cons 'list arguments))))))

