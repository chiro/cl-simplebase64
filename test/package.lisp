(in-package #:common-lisp-user)

(defpackage #:cl-simplebase64-test
  (:use #:cl #:lift #:cl-simplebase64)
  (:import-from #:lift
                #:failures
                #:errors)
  (:export
   #:run-alltest))

