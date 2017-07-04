(defsystem "corne"
  :class :package-inferred-system
  :version "0.1"
  :author "Masashi Iizuka <liquidz.uo@gmail.com>"
  :license "MIT"
  :depends-on ("corne/src/core")
  :description "Commandline argument parser for Common Lisp"
  :in-order-to ((test-op (test-op "corne/test"))))

(defsystem "corne/test"
  :class :package-inferred-system
  :depends-on ("rove"
               "cl-ppcre"
               "corne/test/argument"
               "corne/test/command"
               "corne/test/core"
               "corne/test/option"
               "corne/test/result"
               "corne/test/util")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

;;; vim: set ft=lisp lisp:
