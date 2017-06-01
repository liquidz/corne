#|
  This file is a part of cola project.
|#

(in-package :cl-user)
(defpackage cola-test-asd
  (:use :cl :asdf))
(in-package :cola-test-asd)

(defsystem cola-test
  :author ""
  :license ""
  :depends-on (:cola
               :prove
               :alexandria)
  :components ((:module "t"
                :components
                ((:test-file "cola"))))
  :description "Test system for cola"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
