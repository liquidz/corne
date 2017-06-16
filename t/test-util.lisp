(in-package :cl-user)
(defpackage corne-test.test-util
  (:use :cl))
(in-package :corne-test.test-util)

(defmacro binding (binds &rest body)
  (let* ((syms (loop for _ in binds collect (gensym)))
         (lets (mapcar (lambda (b s) (list s (first b))) binds syms))
         (before-setfs (mapcar (lambda (b) (cons 'setf b)) binds))
         (after-setfs (mapcar (lambda (b s) (list 'setf (first b) s)) binds syms)))
    `(let ,lets
       ,@before-setfs
       ,@body
       ,@after-setfs)))
