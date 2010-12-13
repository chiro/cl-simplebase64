(defpackage :b64-test-system (:use :cl :asdf))
(in-package :b64-test-system)

(defsystem :cl-simplebase64-test
  :components ((:file "package")
               (:file "test-lift" :depends-on ("package")))
  :depends-on (:lift :cl-simplebase64))

