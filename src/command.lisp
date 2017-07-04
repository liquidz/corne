(in-package :cl-user)
(defpackage corne/src/command
  (:use :cl)
  (:import-from :corne/src/argument
                :arg
                :argument-error)
  (:import-from :corne/src/option
                :opt
                :option-error
                :optionp
                :takes-valuep)
  (:import-from :corne/src/result
                :parse-result)
  (:import-from :corne/src/util
                :join
                :align-cons)
  (:export
    :arg
    :cmd
    :add-option!
    :find-command
    :find-option
    :get-name
    :get-parent
    :opt
    :parse
    :parse-argument
    :parse-option
    :parse-subcommand
    :*delm*
    :*add-help-automatically*
    :*default-help-option*))
(in-package :corne/src/command)

(defvar *delm* "    ")
(defvar *add-help-automatically* t)
(defvar *default-help-option*
  (opt "help" :short "h" :long "help" :help "Prints help information"))

(defclass command ()
  ((name        :initform ""  :initarg :name :reader get-name)
   (help        :initform nil :initarg :help)
   (version     :initform ""  :initarg :version)
   (subcommands :initform nil :initarg :subcommands)
   (options     :initform nil :initarg :options)
   (arguments   :initform nil :initarg :arguments)
   (parent      :initform nil :initarg :parent :reader get-parent)))

(defmethod add-option! ((cmd command) option)
  (let ((options (slot-value cmd 'options)))
    (setf (slot-value cmd 'options) (append options (list option)))))

(defmethod find-option ((cmd command) (opt string))
  (let ((ls (slot-value cmd 'options)))
    (find opt ls :test #'corne/src/option::equivalent)))

(defmethod find-command ((cmd command) (subcmd string))
  (let ((ls (slot-value cmd 'subcommands)))
    (find subcmd ls :key #'get-name :test #'equal)))

(defmethod parse-subcommand ((cmd command) args)
  (let* ((arg (first args))
         (c   (and arg (find-command cmd arg))))
    (if c
      (parse-subcommand c (cdr args))
      (values cmd args))))

(defmethod parse-option ((cmd command) args &optional opts)
  "Returns (values REST_ARGS PARSED_OPTION_CONS).

  REST_ARGS:
    List of string
  PARSED_OPTION_CONS:
    ex. '((\"help\" . t))
  "
  (let* ((arg (first args))
         (o   (and arg (find-option cmd arg))))
    (if o
      (if (takes-valuep o)
        (parse-option cmd (cddr args)
                      (cons (cons (corne/src/option::get-name o) (second args)) opts))
        (parse-option cmd (cdr args) (cons (cons (corne/src/option::get-name o) t) opts)))
      (values args (reverse opts)))))

(defun _mapping-args (defined-args user-args)
  (mapcar (lambda (da ua)
            (cons (corne/src/argument::get-name da) ua))
          defined-args user-args))

(defmethod parse-argument ((cmd command) user-args)
  "Returns (values VALID_USER_ARGS MISSING_ARGS TOO_MUCH_ARGS).

  VALID_USER_ARGS:
    ex. '((\"message\" . \"hello\"))
  MISSING_ARGS:
    List of `corne/src/argument:argument` instance
  TOO_MUCH_ARGS:
    List of string
  "
  (let* ((args          (slot-value cmd 'arguments))
         (args-len      (length args))
         (user-args-len (length user-args)))
    (cond
      ((and args (= args-len user-args-len))
       (values (_mapping-args args user-args) () ()))
      ;; no args definition, but user args is not empty
      ((and (not args) (> user-args-len 0))
       (values () () user-args))
      ;; too many arguments
      ((> user-args-len args-len)
       (values (_mapping-args args (subseq user-args 0 args-len)) () (subseq user-args args-len)))
      ;; too few arguments
      (t
        (values (_mapping-args args user-args) (subseq args user-args-len) ())))))

(defmethod get-command-list ((cmd command))
  (reverse
    (loop for c = cmd
          then (get-parent c)
          while c
          collect (get-name c))))

(defmethod parse ((cmd command) args)
  "Returns `corne/src/result:parse-result` instance."
  (multiple-value-bind (cmd args) (parse-subcommand cmd args)
    (multiple-value-bind (args option) (parse-option cmd args)
      (multiple-value-bind (valid-arg missing-arg too-many-arg) (parse-argument cmd args)
        (make-instance 'parse-result
                       :option option
                       :command (join (cdr (get-command-list cmd)) ".")
                       :valid-arg valid-arg
                       :missing-arg missing-arg
                       :too-many-arg too-many-arg
                       :help (help cmd))))))

(defmethod usage ((cmd command))
  "Returns command's usage string."
  (let* ((name (join (get-command-list cmd) " "))
         (subcommands (slot-value cmd 'subcommands))
         (options     (slot-value cmd 'options))
         (arguments   (slot-value cmd 'arguments)))
    (format nil "USAGE: ~A~A~A~A"
            name
            (if options " [OPTIONS]" "")
            (if subcommands " [SUBCOMMAND]" "")
            (if (and arguments (not subcommands))
              (format nil " ~A" (join (mapcar #'corne/src/argument::to-str  arguments) " "))
              ""))))

(defmethod help ((cmd command))
  "Returns command's help string."
  (let* ((name        (get-name cmd))
         (help        (slot-value cmd 'help))
         (version     (slot-value cmd 'version))
         (subcommands (slot-value cmd 'subcommands))
         (options     (slot-value cmd 'options))
         (arguments   (slot-value cmd 'arguments)))
    (with-output-to-string (s)
      (format s "~A ~A~%" name version)
      (when help
        (format s "~A~%" help))
      (format s "~%~A~%" (usage cmd))
      (when subcommands
        (format s "~%SUBCOMMANDS:~%")
        (loop for x in (align-cons (mapcar (lambda (c) (cons (get-name c) (slot-value c 'help)))
                                           subcommands))
              do (format s "~A~A~%" *delm* x)))
      (when options
        (format s "~%OPTIONS:~%")
          (loop for x in (align-cons (mapcar #'corne/src/option::help options))
                do (format s "~A~A~%" *delm* x)))
      (when (and arguments (not subcommands))
        (format s "~%ARGUMENTS:~%")
        (loop for x in (align-cons (mapcar #'corne/src/argument::help arguments))
              do (format s "~A~A~%" *delm* x))))))

(defun cmd (name &rest args)
  (let ((c (apply #'make-instance 'command :name name args)))
    ;; set parent command
    (loop for sc in (slot-value c 'subcommands)
          do (setf (slot-value sc 'parent) c))
    (when (and *add-help-automatically* (not (find-option c "--help")))
      (add-option! c *default-help-option*))
    c))
