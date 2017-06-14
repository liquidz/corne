(in-package :cl-user)
(defpackage corne
  (:use :cl)
  (:import-from :corne.command
                :cmd
                :opt
                :arg
                :help
                :add-option!
                :option-error
                :argument-error)
  (:export
    :cmd
    :opt
    :arg
    :help
    :option-error
    :argument-error
    ))
(in-package :corne)

(defvar *default-help-option*
  (opt "help" :short "h" :long "help" :help "Prints help information"))

(defmacro defcommand (name &rest args)
  `(let ((c (cmd ,name ,@args)))
     (add-option! c *default-help-option*)
     c))

(let ((c (defcommand "foo"
            :about "aa"
            :subcommands (list (cmd "neko" :about "aaa"))
            :options (list (opt "verbose" :short "v" :long "verbose" :help "vvvvv")))))
  (help c)
  )

;(let ((c (cmd "foo" :options (list (opt "verbose" :short "v" :help "bar")))))
;  (format t "~A~%" c)
;  )

;(defmacro defcommand ()
;  )




