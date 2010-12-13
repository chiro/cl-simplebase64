;;;;This package provides base64 encoding utils

(defpackage #:cl-simplebase64
  (:use #:cl)
  (:nicknames #:base64)
  (:export
   #:encode
   #:decode
   #:vector=
   #:vector-list=))

