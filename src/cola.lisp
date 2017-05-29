(in-package :cl-user)
(defpackage cola
  (:use :cl)
  (:export
    :option
    :command)
  )
(in-package :cola)

(defclass option ()
  ((name        :initform "" :initarg :name :reader get-name)
   (short       :initform () :initarg :short)
   (long        :initform () :initarg :long)
   (help        :initform "" :initarg :help)
   (takes-value :initform () :initarg :takes-value)))

(defclass command ()
  ((name        :initform "" :initarg :name :reader get-name)
   (about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)))

(defstruct result
  subcommand
  options
  arguments)

(defun add-result-option! (result option)
  (let ((o (result-options result)))
    (setf (result-options result)
          (nconc o (list option)))))

(defun command (&rest args)
  (apply #'make-instance 'command args))
(defun option (&rest args)
  (apply #'make-instance 'option args))

(defmethod find-command ((cmd command) (subcmd string))
  (let ((ls (slot-value cmd 'subcommands)))
    (find subcmd ls :key #'get-name :test #'equal)))

(defmethod equivalent ((s string) (opt option))
  (let ((short (format () "-~A"  (slot-value opt 'short)))
        (long  (format () "--~A" (slot-value opt 'long))))
    (or (equal s short)
        (equal s long))))

(defmethod find-option ((cmd command) (opt string))
  (let ((ls (slot-value cmd 'options)))
    (find opt ls :test #'equivalent)))

;(setq cmd _sample_command_)
;(setq args '("-h"))
;
;(let* ((args '("-h" "foo"))
;       (arg (first args))
;       (result (make-result))
;       (cmd _sample_command_)
;       (c (find-command cmd arg))
;       (o (find-option cmd arg)))
;  (if (slot-value o 'takes-value)
;    t
;    (progn (setf (result-options result)
;
;                 ))
;    ()
;    )
;  )

(defmethod parse ((cmd command) args &optional (result (make-result)))
  (unless args
    result
    (let* ((arg (first args))
           (c (find-command cmd arg))
           (o (find-option cmd arg)))
      (cond
        (c
          ;(cdr args)
          )
        (o
          "kiteru"
          ;(if (slot-value o 'takes-value)
          ;  (progn
          ;    (add-result-option! result )
          ;    )
          ;  (parse cmd (cddr args))
          ;  (parse cmd (cdr args)))
          )
        (t
          )
        )
      )
    )
  )


(setq _sample_command_
  (command :name "sample" :about "test" :version "1.0"
           :subcommands (list
                          (command :name "foo" :about "foobar" :version "1.0"
                                   :options (list (option :short "v" :long "verbose" :help "baz"))))
           :options (list
                      (option :name "help" :short "h" :long "help" :help "print help")
                      (option :name "foo" :short "f" :long "foo" :takes-value t :help "bar")
                      )
           ))





