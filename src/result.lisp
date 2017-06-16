(in-package :cl-user)
(defpackage corne.result
  (:use :cl)
  ;(:import-from :corne.option
  ;              :optionp)
  (:export
    :parse-result
    :equivalent
    :get-subcommand
    :get-option
    :get-arg
    :get-help))

(in-package :corne.result)

(defclass parse-result ()
  ((option       :initform nil :initarg :option)
   (command      :initform nil :initarg :command :reader get-subcommand)
   (valid-arg    :initform nil :initarg :valid-arg)
   (missing-arg  :initform nil :initarg :missing-arg)
   (too-many-arg :initform nil :initarg :too-many-arg)
   (help         :initform nil :initarg :help :reader get-help)))

(defmethod equivalent ((r1 parse-result) (r2 parse-result))
  (let ((slots '(option command valid-arg missing-arg too-many-arg help)))
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
        (errors nil)
        )

    (when (corne.option::optionp (first valid-arg))
      (setf errors (append errors (format nil "Invalid option: ~A" (first valid-arg))))
      )
    
    (when missing-arg

      ;(mapcar #'corne.argument::get-name missing-arg)
      (setf errors (append errors (format nil "Missing arguments: ")))
      )

    errors
    ;(when missing-args
    ;  (error 'argument-error
    ;         :arguments (mapcar #'corne.argument::get-name missing-args)
    ;         :reason "missing arguments"))
    ;(when too-much-args
    ;  (error 'argument-error
    ;         :arguments too-much-args
    ;         :reason "too much arguments"))

    )
  )

(let ((r (make-instance 'parse-result :valid-arg '("-a")))
      )
  (get-error r)
  )
