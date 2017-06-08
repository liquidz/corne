#|
  This file is a part of corne project.
|#

(in-package :cl-user)
(defpackage corne-test-asd
  (:use :cl :asdf))
(in-package :corne-test-asd)

(defsystem corne-test
  :author ""
  :license ""
  :depends-on (:corne
               :prove
               :alexandria)
  :components ((:module "t"
                :components
                ((:test-file "corne")
                 (:test-file "argument")
                 (:test-file "option")
                )))
  :description "Test system for corne"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
