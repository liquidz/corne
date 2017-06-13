(in-package :cl-user)
(defpackage corne.argument
  (:use :cl)
  (:export
    ;:@arg
    ;:argument
    :arg
    :argument-error
    :get-name
    :help
    :to-str
    ))

(in-package :corne.argument)

(defclass argument ()
  ((name :initform "" :initarg :name :reader get-name)
   (help :initform "" :initarg :help)))

(defun arg (name &rest args)
  (apply #'make-instance 'argument :name name args))

;;; (@arg name "this is help")
(defmacro @arg (name &optional help)
  (let ((name (string-downcase (symbol-name name)))
        (help (if (and help (stringp help)) help "")))
    `(make-instance 'argument :name ,name :help ,help)))

(defmethod help ((arg argument))
  (let ((name (get-name arg))
        (help (slot-value arg 'help)))
    (cons (format nil "<~A>" (string-upcase name))
          help)))

(defmethod to-str ((arg argument))
  (format nil "<~A>" (string-upcase (get-name arg))))

(define-condition argument-error (simple-error)
  ;; ARGUMENTS shoud be list of string
  ;; REASON shoud be string
  ((arguments :initarg :arguments :reader argument-error-arguments)
   (reason :initarg :reason :reader argument-error-reason))
  (:report (lambda (c s)
            (format s "Invalid arguments: ~S, reason: ~A"
                    (argument-error-arguments c) (argument-error-reason c)))))
