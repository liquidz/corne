(in-package :cl-user)
(defpackage corne.command
  (:use :cl)
  (:import-from :corne.argument
                :arg
                :argument-error)
  (:import-from :corne.option
                :opt
                :option-error
                :optionp
                :takes-valuep)
  (:import-from :corne.result
                :parse-result)
  (:import-from :corne.util
                :join
                :align-cons)
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
    :parse-subcommand
    :*delm*
    :*add-help-automatically*
    :*default-help-option*))
(in-package :corne.command)

(defvar *delm* "    ")
(defvar *add-help-automatically* t)
(defvar *default-help-option*
  (opt "help" :short "h" :long "help" :help "Prints help information"))

(defclass command ()
  ((name        :initform ""  :initarg :name :reader get-name)
   (about       :initform nil :initarg :about)
   (version     :initform ""  :initarg :version)
   (subcommands :initform nil :initarg :subcommands)
   (options     :initform nil :initarg :options)
   (arguments   :initform nil :initarg :arguments)))

(defmethod add-option! ((cmd command) option)
  (let ((options (slot-value cmd 'options)))
    (setf (slot-value cmd 'options) (append options (list option)))))

(defmethod find-option ((cmd command) (opt string))
  (let ((ls (slot-value cmd 'options)))
    (find opt ls :test #'corne.option::equivalent)))

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
  (let* ((arg (first args))
         (o   (and arg (find-option cmd arg))))
    (if o
      (if (takes-valuep o)
        (parse-option cmd (cddr args)
                      (cons (cons (corne.option::get-name o) (second args)) opts))
        (parse-option cmd (cdr args) (cons (cons (corne.option::get-name o) t) opts))
        )
      (values args (reverse opts)))))

(defun _mapping-args (defined-args user-args)
  (mapcar (lambda (da ua)
            (cons (corne.argument::get-name da) ua))
          defined-args user-args))

(defmethod parse-argument ((cmd command) user-args)
  "returns (values VALID_USER_ARGS MISSING_ARGS TOO_MUCH_ARGS)"
  (let* ((args          (slot-value cmd 'arguments))
         (args-len      (length args))
         (user-args-len (length user-args)))
    (if (or (not args) (= args-len user-args-len))
      (values (_mapping-args args user-args) () ())
      (if (> user-args-len args-len)
        (values (_mapping-args args (subseq user-args 0 args-len)) () (subseq user-args args-len))
        (values (_mapping-args args user-args) (subseq args user-args-len) ())))))

(defmethod parse ((cmd command) args)
  "return (values option subcommand rest-args)"
  (multiple-value-bind (cmd args subcommand) (parse-subcommand cmd args)
    (multiple-value-bind (args option) (parse-option cmd args)
      (multiple-value-bind (valid-arg missing-arg too-many-arg) (parse-argument cmd args)
        (make-instance 'parse-result
                       :option option
                       :command subcommand
                       :valid-arg valid-arg
                       :missing-arg missing-arg
                       :too-many-arg too-many-arg
                       :help (help cmd))
        ))))

(defmethod usage ((cmd command))
  (let* ((name        (get-name cmd))
         (subcommands (slot-value cmd 'subcommands))
         (options     (slot-value cmd 'options))
         (arguments   (slot-value cmd 'arguments)))
    (format nil "USAGE: ~A~A~A~A"
            name
            (if options " [OPTIONS]" "")
            (if subcommands " [SUBCOMMAND]" "")
            (if (and arguments (not subcommands))
              (format nil " ~A" (join (mapcar #'corne.argument::to-str  arguments) " "))
              ""))))

(defmethod help ((cmd command))
  (let* ((name        (get-name cmd))
         (about       (slot-value cmd 'about))
         (version     (slot-value cmd 'version))
         (subcommands (slot-value cmd 'subcommands))
         (options     (slot-value cmd 'options))
         (arguments   (slot-value cmd 'arguments)))
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
          (loop for x in (align-cons (mapcar #'corne.option::help options))
                do (format s "~A~A~%" *delm* x)))
      (when (and arguments (not subcommands))
        (format s "~%ARGUMENTS:~%")
        (loop for x in (align-cons (mapcar #'corne.argument::help arguments))
              do (format s "~A~A~%" *delm* x))))))

(defun cmd (name &rest args)
  (let ((c (apply #'make-instance 'command :name name args)))
    (when (and *add-help-automatically* (not (find-option c "--help")))
      (add-option! c *default-help-option*))
    c))
