(in-package :cl-user)
(defpackage corne.option
  (:use :cl)
  (:export :option
           :@option
           :equivalent
           :find-option
           :option-error
           :optionp
           :help))

(in-package :corne.option)

(defclass option ()
  ((name        :initform "" :initarg :name :reader get-name)
   (short       :initform () :initarg :short)
   (long        :initform () :initarg :long)
   (help        :initform "" :initarg :help)
   (takes-value :initform () :initarg :takes-value)))

;;; (@option name -h --help +takes-value "Prints help information")
(defmacro @option (name &rest args)
  (let* ((name (string-downcase (symbol-name name)))
         (syms (loop for x in args while (symbolp x) collect (symbol-name x)))
         (options (loop for x in syms while (optionp x)
                        collect (string-downcase x)))
         (short (find-if (lambda (opt) (= 1 (mismatch opt "--"))) options))
         (long (find-if (lambda (opt) (= 2 (mismatch opt "--"))) options))
         (takes-value (position "+TAKES-VALUE" syms :test #'equal))
         (help (or (find-if #'stringp args) "")))
    `(make-instance 'option
                    :name ,name
                    ,@(if short (list :short (subseq short 1)))
                    ,@(if long (list :long (subseq long 2)))
                    ,@(if takes-value (list :takes-value t))
                    :help ,help)))

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
