(in-package :cl-user)
(defpackage cola.util
  (:use :cl)
  (:export
    :asetf
    :it))
(in-package :cola.util)

(defmacro asetf (key value)
  `(let ((,'it ,key))
     (setf ,key ,value)))
