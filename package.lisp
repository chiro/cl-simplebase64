;;;;This package provides base64 encoding utils

(cl:defpackage #:cl-simplebase64
  (:use #:cl)
  (:nicknames #:base64)
  (:export
   ;;encode
   #:encode
   ;;decode
   #:decode))

