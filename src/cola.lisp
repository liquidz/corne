(in-package :cl-user)
(defpackage cola
  (:use :cl))
(in-package :cola)

(defclass option ()
  ((short       :initform () :initarg :short)
   (long        :initform () :initarg :long)
   (help        :initform "" :initarg :help)
   (takes-value :initform () :initarg :takes-value)))

(defclass command ()
  ((name        :iniform  "" :initarg :name))
   (about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)))

(defun command (&rest args)
  (apply #'make-instance 'command args))
(defun option (&rest args)
  (apply #'make-instance 'option args))

;(command :about "test" :version "1.0"
;         :subcommands (list
;                        (command )
;                        )
;         :options (list (option :short "h" :long "help" :help "print help"))
;
;(option :short "v" :long "verbose")
