(in-package :cl-user)
(defpackage corne.command
  (:use :cl)
  (:import-from :corne.argument
                :arg
                :argument-error)
  (:import-from :corne.option
                :equivalent
                :opt
                :option-error
                :optionp
                :takes-valuep)
  (:export
    :arg
    :cmd
    :add-option!
    :find-command
    :find-option
    :get-name
    :opt
    :parse
    :parse-argument
    :parse-option
    :parse-subcommand))
(in-package :corne.command)

(defvar *delm* "    ")

(defclass command ()
  ((name        :initform "" :initarg :name :reader get-name)
   (about       :initform nil :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform nil :initarg :subcommands)
   (options     :initform nil :initarg :options)
   (arguments   :initform nil :initarg :arguments)))

(defun cmd (name &rest args)
  (apply #'make-instance 'command :name name args))

(defmethod find-option ((cmd command) (opt string))
  (let ((ls (slot-value cmd 'options)))
    (find opt ls :test #'equivalent)))

(defmethod find-command ((cmd command) (subcmd string))
  (let ((ls (slot-value cmd 'subcommands)))
    (find subcmd ls :key #'get-name :test #'equal)))

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
      (if (takes-valuep o)
        (parse-option cmd (cddr args)
                      (cons (cons (corne.option::get-name o) (second args)) opts))
        (parse-option cmd (cdr args) (cons (cons (corne.option::get-name o) t) opts)))
      (values args (reverse opts)))))

(defmethod parse-argument ((cmd command) user-args)
  "returns (values VALID_USER_ARGS MISSING_ARGS TOO_MUCH_ARGS)"
  (let* ((args (slot-value cmd 'arguments))
         (args-len (length args))
         (user-args-len (length user-args)))
    (if (or (not args) (= args-len user-args-len))
      (values user-args () ())
      (if (> user-args-len args-len)
        (values (subseq user-args 0 args-len) () (subseq user-args args-len))
        (values user-args (subseq args user-args-len) ())))))

(defmethod parse ((cmd command) args)
  "return (values option subcommand rest-args)"
  (multiple-value-bind (cmd args subcommand) (parse-subcommand cmd args)
    (multiple-value-bind (args options) (parse-option cmd args)
      (multiple-value-bind (valid-args missing-args too-much-args) (parse-argument cmd args)
        (when (optionp (first valid-args))
          (error 'option-error :option (first valid-args)))
        (when missing-args
          (error 'argument-error
                 :arguments (mapcar #'corne.argument::get-name missing-args)
                 :reason "missing arguments"))
        (when too-much-args
          (error 'argument-error
                 :arguments too-much-args
                 :reason "too much arguments"))
        (values options subcommand args)))))

(defmethod add-option! ((cmd command) option)
  (let ((options (slot-value cmd 'options)))
    (setf (slot-value cmd 'options) (append options (list option)))))

(defun _join (coll &optional (delm ""))
  (reduce (lambda (res s)
            (format nil "~A~A~A" res delm s))
          coll))

(defun _repeat (x n)
  (loop for _ from 0 below n collect x))

(defun _align-cons (coll &key (space (length *delm*)) (delm " "))
  (let* ((car-max-len (apply #'max (loop for x in coll collect (length (car x)))))
         (delm-len (+ space car-max-len))
         (gen-delm (lambda (x) (_join (_repeat delm (- delm-len (length x))))))
         )
    (mapcar (lambda (x)
           (format nil "~A~A~A" (car x) (funcall gen-delm (car x)) (cdr x))
           ) coll)))

(defmethod usage ((cmd command))
  (let* ((name (get-name cmd))
         (subcommands (slot-value cmd 'subcommands))
         (options (slot-value cmd 'options))
         (arguments (slot-value cmd 'arguments)))
    (format nil "USAGE: ~A~A~A~A"
            name
            (if options " [OPTIONS]" "")
            (if subcommands " [SUBCOMMAND]" "")
            (if (and arguments (not subcommands))
              (format nil " ~A" (_join (mapcar #'corne.argument::to-str  arguments) " "))
              ""))))

(defmethod help ((cmd command))
  (let* ((name (get-name cmd))
         (about (slot-value cmd 'about))
         (version (slot-value cmd 'version))
         (subcommands (slot-value cmd 'subcommands))
         (options (slot-value cmd 'options))
         (arguments (slot-value cmd 'arguments)))
    (with-output-to-string (s)
      (format s "~A ~A~%" name version)
      (when about
        (format s "~A~%" about))
      (format s "~%~A~%" (usage cmd))
      (when subcommands
        (format s "~%SUBCOMMANDS:~%")
        ;(mapcar (lambda (c)
        ;          (cons (get-name c) (slot-value c 'about))
        ;          ) subcommands)
        (loop for c in subcommands
              do (format s "~A~A~%" *delm* (get-name c))))
      (when options
        (format s "~%OPTIONS:~%")
          (loop for x in (_align-cons (mapcar #'corne.option::help options))
                do (format s "~A~A~%" *delm* x)))
      (when (and arguments (not subcommands))
        (format s "~%ARGUMENTS:~%")
        (loop for x in (_align-cons (mapcar #'corne.argument::help arguments))
              do (format s "~A~A~%" *delm* x))))))
