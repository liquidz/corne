(in-package :cl-user)
(defpackage cola
  (:use
    :cl
    :cola.util
    )
  (:export
    :option
    :command
    :optionp
    :parse-subcommand
    :parse-option
    :parse
    ))
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
  arguments
  subcommand
  options)

;(defcommand
;  :name "hello"
;  :about "foo"
;  :version "1.0"
;  (subcommand
;    :name "bar"
;    :about "baz"
;    (option "help" -h --help "this is help"
;      )
;    )
;  )

(defun add-result-option! (result-hash option &optional (value t))
  (asetf (gethash "options" result-hash)
         (cons (cons option value) it)))

(defun result->values (result)
  (values (result-options result)
          (result-subcommand result)
          (result-arguments result)))

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

(defun optionp (s)
  (position #\- s))

(defmethod parse-subcommand ((cmd command) args &optional subcmds)
  (let* ((arg (first args))
         (c   (and arg (find-command cmd arg))))
    (if c
      (parse-subcommand c (cdr args) (cons arg subcmds))
      (values cmd args
              (and subcmds (format nil "~{~a~^.~}" (reverse subcmds)))))))

(defmethod parse-option ((cmd command) args &optional opts)
  (let ((o (find-option cmd (first args))))
    (if o
      (if (slot-value o 'takes-value)
        (parse-option cmd (cddr args)
                      (cons (cons (get-name o) (second args)) opts))
        (parse-option cmd (cdr args) (cons (cons (get-name o) t) opts)))
      (values cmd args (reverse opts)))))


;(defmethod parse ((cmd command) args &optional (result (make-result)))
;  "return (values option subcommand rest-args)"
;  (if (not  args)
;    (result->values result)
;    (let* ((arg (first args))
;           (c (find-command cmd arg))
;           (o (find-option cmd arg)))
;      (cond
;        (c
;          (add-result-subcommand! result arg)
;          (parse c (cdr args) result))
;        (o
;          (if (slot-value o 'takes-value)
;            (progn
;              (add-result-option! result arg (second args))
;              (parse cmd (cddr args) result))
;            (progn
;              (add-result-option! result arg)
;              (parse cmd (cdr args) result))))
;        (t
;          ;(when (optionp arg)
;          ;  (error "Invalid option: ~S." arg))
;          (setf (result-arguments result) args)
;          (parse () () result))
;        ))))

