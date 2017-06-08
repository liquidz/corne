(in-package :cl-user)
(defpackage corne
  (:use :cl)
  (:import-from :corne.argument
                :@arg
                :argument
                :argument-error
                :to-str)
  (:import-from :corne.option
                :@option
                :option
                :option-error
                :optionp
                )
  (:export
    :option
    :command
    :@arg
    :@option
    :@command
    :parse-subcommand
    :find-option
    :parse-option
    :parse-argument
    :parse
    :help
    ))
(in-package :corne)

(defvar *delm* "    ")

(defclass has-name ()
  ((name :initform "" :initarg :name :reader get-name)))


(defclass command (has-name)
  ((about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)
   (arguments   :initform () :initarg :arguments)))

;;; (@command name
;;;   :about "aaa"
;;;   :version "1.0"
;;;   (@command sub ...)
;;;   (@option ...) (@option ...)
;;;   (@arg ...) (@arg ...))
(defmacro @command (name &rest args)
  (let* ((name (string-downcase (symbol-name name)))
         (keys (loop for x in args while (not (listp x)) collect x))
         (lists (loop for x in args when (listp x) collect x))
         (subcommands (loop for x in lists when (eql '@command (car x)) collect x))
         (options (loop for x in lists when (eql '@option (car x)) collect x))
         (arguments (loop for x in lists when (eql '@arg (car x)) collect x)))
    `(make-instance 'command
                    :name ,name
                    ,@keys
                    ,@(if subcommands (list :subcommands (cons 'list subcommands)))
                    ,@(if options (list :options (cons 'list options)))
                    ,@(if arguments (list :arguments (cons 'list arguments))))))

#+nil (@command hello :about "aa" :version "1.0"
          (@command foo (@option verbose -v "detail"))
          (@option help -h --help "show help")
          (@arg file "foo bar"))

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
      (if (slot-value o 'takes-value)
        (parse-option cmd (cddr args)
                      (cons (cons (get-name o) (second args)) opts))
        (parse-option cmd (cdr args) (cons (cons (get-name o) t) opts)))
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




(defun join (coll delm)
  (reduce (lambda (res s)
            (format nil "~A~A~A" res delm s))
          coll))

;(defmethod help ((cmd command))
;  (let* ((name (get-name cmd))
;         (about (slot-value cmd 'about))
;         (version (slot-value cmd 'version))
;         (subcommands (slot-value cmd 'subcommands))
;         (options (slot-value cmd 'options))
;         (arguments (slot-value cmd 'arguments)))
;    (list
;      (format nil "~A ~A~%" name ver)
;      (format nil "USAGE: ~A~A~A~A"
;              name
;              (if options " [OPTIONS]" "")
;              (if subcommands " [SUBCOMMAND]" "")
;              (if (and arguments (not subcommands))
;                (format nil " ~A" (join (mapcar #'to-str  arguments) ", "))
;                ""))
;      (if subcommands
;        (format nil "SUBCOMMANDS:")
;        )
;      (if options
;        (format nil "OPTIONS:")
;        )
;      )
;    )
;  )

(defmethod parse ((cmd command) args)
  "return (values option subcommand rest-args)"
  (multiple-value-bind (cmd args subcommand) (parse-subcommand cmd args)
    (multiple-value-bind (args options) (parse-option cmd args)
      (multiple-value-bind (valid-args missing-args too-much-args) (parse-argument cmd args)
        (when (optionp (first valid-args))
          (error 'option-error :option (first valid-args)))
        (when missing-args
          (error 'argument-error
                 :arguments (mapcar #'get-name missing-args)
                 :reason "missing arguments"))
        (when too-much-args
          (error 'argument-error
                 :arguments too-much-args
                 :reason "too much arguments"))
        (values options subcommand args)
        )
      )))
