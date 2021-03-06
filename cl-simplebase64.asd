(defpackage :b64-system (:use :cl :asdf))
(in-package :b64-system)

(defsystem :cl-simplebase64
    :description "cl-simplebase64: simple utilities of Base64 encoding on lisp"
    :version "0.3"
    :author "h_chiro <peanut2626@gmail.com>"
    :licence "Public Domain"
    :components ((:file "package")
		 (:file "util" :depends-on ("package"))
		 (:file "variables" :depends-on ("package"))
		 (:file "bit-op" :depends-on ("util" "variables"))
		 (:file "encode" :depends-on ("bit-op"))
		 (:file "decode" :depends-on ("bit-op"))
		 (:file "test" :depends-on ("encode" "decode"))))
