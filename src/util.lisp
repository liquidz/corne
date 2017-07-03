(in-package :cl-user)
(defpackage corne.util
  (:use :cl)
  (:export
    :join
    :repeat
    :align-cons))

(in-package :corne.util)

(defun join (coll &optional (delm ""))
  (and coll
       (reduce (lambda (res s)
                 (format nil "~A~A~A" res delm s))
               coll)))

(defun repeat (x n)
  (loop for _ from 0 below n collect x))

(defun align-cons (coll &key (space 4) (delm " "))
  (let* ((car-max-len (apply #'max (loop for x in coll collect (length (car x)))))
         (delm-len    (+ space car-max-len))
         (gen-delm    (lambda (x) (join (repeat delm (- delm-len (length x)))))))
    (mapcar (lambda (x)
              (format nil "~A~A~A" (car x) (funcall gen-delm (car x)) (cdr x))) coll)))
