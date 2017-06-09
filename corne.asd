#|
  This file is a part of corne project.
|#

(in-package :cl-user)
(defpackage corne-asd
  (:use :cl :asdf))
(in-package :corne-asd)

(defsystem corne
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "corne" :depends-on ("command"))
                 (:file "command" :depends-on ("argument" "option"))
                 (:file "argument")
                 (:file "option"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op corne-test))))
