(in-package :cl-user)
(defpackage corne.result
  (:use :cl)
  (:import-from :corne.util
                :join)
  ;(:import-from :corne.option
  ;              :optionp)
  (:export
    :parse-result
    :equivalent
    :get-subcommand
    :get-option
    :get-arg
    :get-help
    :get-error))

(in-package :corne.result)

(defclass parse-result ()
  ((option       :initform nil :initarg :option)
   (command      :initform nil :initarg :command :reader get-subcommand)
   (valid-arg    :initform nil :initarg :valid-arg)
   (missing-arg  :initform nil :initarg :missing-arg)
   (too-many-arg :initform nil :initarg :too-many-arg)
   (help         :initform nil :initarg :help :reader get-help)))

(defmethod equivalent ((r1 parse-result) (r2 parse-result))
  (let ((slots '(option command valid-arg missing-arg too-many-arg)))
    (equal
      (mapcar (lambda (s) (slot-value r1 s)) slots)
      (mapcar (lambda (s) (slot-value r2 s)) slots))))

(defmethod get-option ((res parse-result) (key string))
  (let* ((option (slot-value res 'option))
         (val (find key option :key #'car :test #'equal)))
    (and val (cdr val))))

(defmethod get-arg ((res parse-result) (key string))
  (let* ((argument (slot-value res 'valid-arg))
         (val (find key argument :key #'car :test #'equal)))
    (and val (cdr val))))

(defmethod get-error ((res parse-result))
  (let ((valid-arg (slot-value res 'valid-arg))
        (missing-arg (slot-value res 'missing-arg))
        (too-many-arg (slot-value res 'too-many-arg))
        (errors nil))

    (when (and valid-arg (corne.option::optionp (cdar valid-arg)))
      (setf errors (cons (format nil "Invalid option: ~A" (first valid-arg)) errors)))

    (when missing-arg
      (let ((args (join (mapcar #'corne.argument::get-name missing-arg) ", ")))
        (setf errors (cons (format nil "Missing arguments: ~A" args) errors))))

    (when too-many-arg
      (let ((args (join too-many-arg ", ")))
        (setf errors (cons (format nil "Too many arguments: ~A" args) errors))))

    (reverse errors)))

#|
(let ((r (make-instance 'parse-result
                        :valid-arg '("-a")
                        :missing-arg (list (corne.argument::arg "foo") (corne.argument::arg "bar"))
                        :too-many-arg '("nekt")
                        ))
      )
  (get-error r)
  )
|#
