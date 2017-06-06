(in-package :cl-user)
(defpackage cola
  (:use
    :cl
    :cola.util
    )
  (:export
    :option
    :command
    :@option
    :@arg
    :argument
    :optionp
    :parse-subcommand
    :parse-option
    :parse-argument
    :parse
    :option-error
    :argument-error
    :help
    ))
(in-package :cola)

(defvar *delm* "    ")


(defmacro aif (pred true-part &optional else-part)
  `(let ((it ,pred))
     (if it ,true-part ,else-part)))

(defclass has-name ()
  ((name :initform "" :initarg :name :reader get-name)))

(defclass option (has-name)
  ((short       :initform () :initarg :short)
   (long        :initform () :initarg :long)
   (help        :initform "" :initarg :help)
   (takes-value :initform () :initarg :takes-value)))

(defclass argument (has-name)
  ((help :initform "" :initarg :help)))

(defclass command (has-name)
  ((about       :initform "" :initarg :about)
   (version     :initform "" :initarg :version)
   (subcommands :initform () :initarg :subcommands)
   (options     :initform () :initarg :options)
   (arguments   :initform () :initarg :arguments)))

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

; (@option name -h --help "Prints help information")
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

(defmacro @arg (name &optional help)
  (let ((name (string-downcase (symbol-name name)))
        (help (if (and help (stringp help)) help "")))
    `(make-instance 'argument :name ,name :help ,help)))

;(defclass argument (has-name)
;  ((help :initform "" :initarg :help)))
;(defun command (&rest args)
;  (apply #'make-instance 'command args))
;(defun option (&rest args)
;  (apply #'make-instance 'option args))

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

(defmethod to-str ((arg argument))
  (format nil "<~A>" (string-upcase (get-name arg))))

(define-condition option-error (simple-error)
  ((option :initarg :option :reader option-error-option))
  (:report (lambda (c s)
             (format s "Invalid option: ~A" (option-error-option c)))))

(define-condition argument-error (simple-error)
  ;; ARGUMENTS shoud be list of string
  ;; REASON shoud be string
  ((arguments :initarg :arguments :reader argument-error-arguments)
   (reason :initarg :reason :reader argument-error-reason))
  (:report (lambda (c s)
            (format s "Invalid arguments: ~S, reason: ~A"
                    (argument-error-arguments c) (argument-error-reason c)))))

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

(defmethod help ((arg argument))
  (let ((name (get-name arg))
        (help (slot-value arg 'help)))
    (cons (format nil "<~A>" (string-upcase name))
          help)))

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

