# Corne

Commandline argument parser for Common Lisp

## Usage

```lisp
(in-package :cl-user)
(defpackage foo.bar
  (:use :cl :corne))
(in-package :foo.bar)

(defvar *cli*
  (cmd "hello"
       :help "corne example command"
       :version "1.0"
       :options (list (opt "verbose" :short "v" :long "verbose" :help ""))
       :arguments (list (arg "msg" :help "message"))))

(defun main (&rest argv)
  (let* ((res (parse *cli* argv)))
    (format t "hello ~A~%" (get-arg res "msg"))))
```

More examples are [here](example).

## Features

### Auto help generation
```sh
$ ./example/hello.ros -h
hello 1.0
corne example command

USAGE: hello [OPTIONS] <MSG>

OPTIONS:
    -v, --verbose
    -h, --help       Prints help information

ARGUMENTS:
    <MSG>    message
```
### Subcommand support
```
(defvar *cli*
  (cmd "subcommand"
       :help "corne example command"
       :version "1.0"
       :subcommands (list (cmd "foo" :help "foooo")
                          (cmd "bar" :help "baaar"
                                     :subcommands (list (cmd "baz" :help "baaaz"))))))
```

## Installation

* Corne is not registered [Quicklisp](https://www.quicklisp.org/beta/) yet.
* clone to your local-projects
```sh
# ex
git clone https://github.com/liquidz/corne ~/.roswell/local-projects
```
* register local project
```lisp
(ql:register-local-projects)
```
