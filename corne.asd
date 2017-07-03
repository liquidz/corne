#|
  This file is a part of corne project.
|#

(in-package :cl-user)
(defpackage corne-asd
  (:use :cl :asdf))
(in-package :corne-asd)

(defsystem corne
  :version "0.1"
  :author "Masashi Iizuka <liquidz.uo@gmail.com>"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "corne"   :depends-on ("command" "result"))
                 (:file "command" :depends-on ("argument" "option" "result" "util"))
                 (:file "result"  :depends-on ("argument" "option" "util"))
                 (:file "argument")
                 (:file "option")
                 (:file "util"))))
  :description ""
  :long-description
     #.(uiop:read-file-string
         (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op corne-test))))
