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
  ((name        :initform "" :initarg :name :reader get-name)
   (about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)))

(defun command (&rest args)
  (apply #'make-instance 'command args))
(defun option (&rest args)
  (apply #'make-instance 'option args))

(defmethod find-subcommand ((cmd command) (subcmd string))
  (let ((ls (slot-value cmd 'subcommands)))
    (find subcmd ls :key #'get-name :test #'equal)))

(defmethod equivalent ((opt option) (s string))
  (let ((short (concatenate "-"  (slot-value opt 'short)))
        (long  (concatenate "--" (slot-value opt 'long))))
    (or (equal s short)
        (equal s long))))

(equivalent (option :short "h" :long "help" :help "print help")
            "h")
;(defmethod find-option ((cmd command) (opt string))
;  (let ((ls (slot-value cmd 'options)))
;    )
;  )

(setq _sample_command_
  (command :name "sample" :about "test" :version "1.0"
           :subcommands (list
                          (command :name "foo" :about "foobar" :version "1.0"
                                   :options (list (option :short "v" :long "verbose" :help "baz"))))
           :options (list (option :short "h" :long "help" :help "print help"))
           ))




(setq _sample_args_
      '("-h")
      ;'("--help")
      ;'("foo")
      ;'("foo" "-v")
      ;'("foo" "--verbose")
      ;'("-e")
      ;'("--error")
      )
