(in-package :cl-user)
(defpackage corne/src/option
  (:use :cl)
  (:export
    :get-name
    :equivalent
    :help
    :opt
    :option-error
    :optionp
    :takes-valuep))

(in-package :corne/src/option)

(defclass option ()
  ((name        :initform "" :initarg :name :reader get-name)
   (short       :initform () :initarg :short)
   (long        :initform () :initarg :long)
   (help        :initform "" :initarg :help)
   (takes-value :initform () :initarg :takes-value :reader takes-valuep)))

(defun opt (name &rest args)
  (apply #'make-instance 'option :name name args))

(defmethod equivalent ((s string) (opt option))
  (let ((short (format () "-~A"  (slot-value opt 'short)))
        (long  (format () "--~A" (slot-value opt 'long))))
    (or (equal s short)
        (equal s long))))

(defun optionp (s)
  (position #\- s))

(define-condition option-error (simple-error)
  ((option :initarg :option :reader option-error-option))
  (:report (lambda (c s)
             (format s "Invalid option: ~A" (option-error-option c)))))

#|
FLAGS:
    -h, --help             Prints help information
    -V, --version          Prints version information

OPTIONS:
    -r, --rule <FILE>    rule TOML file
|#
(defmethod help ((opt option))
  (let ((s (slot-value opt 'short))
        (l (slot-value opt 'long))
        (ret (cons "" (slot-value opt 'help))))
    (cond
      ((and s l)
       (setf (car ret) (format nil "-~A, --~A" s l)))
      (s
        (setf (car ret) (format nil "-~A" s)))
      (l
        (setf (car ret) (format nil "--~A" l))))
    (when (slot-value opt 'takes-value)
      (setf (car ret) (format nil "~A <VALUE>" (car ret))))
    ret))
