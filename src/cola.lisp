(in-package :cl-user)
(defpackage cola
  (:use :cl)
  (:export
    :option
    :command)
  )
(in-package :cola)

(defmacro aif (pred true-part &optional else-part)
  `(let ((it ,pred))
     (if it ,true-part ,else-part)))

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

(defun add-result-option! (result option &optional (value t))
  (let ((o (result-options result)))
    (setf (result-options result)
          (cons (cons option value) o))))

(defun add-result-subcommand! (result cmd)
  (let ((c (result-subcommand result)))
    (setf (result-subcommand result)
          (if c
            (format () "~A.~A" c cmd)
            cmd)))))

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

(defmethod parse ((cmd command) args &optional (result (make-result)))
  (if (not  args)
    result
    (let* ((arg (first args))
           (c (find-command cmd arg))
           (o (find-option cmd arg)))
      (cond
        (c
          (add-result-subcommand! result arg)
          (parse c (cdr args) result))
        (o
          (if (slot-value o 'takes-value)
            (progn
              (add-result-option! result arg (second args))
              (parse cmd (cddr args) result))
            (progn
              (add-result-option! result arg)
              (parse cmd (cdr args) result))))
        (t
          (setf (result-arguments result) args)
          result)
        )
      )))


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
