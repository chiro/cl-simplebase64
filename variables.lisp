;;;;This file contains variables in common

(in-package :cl-simplebase64)

(deftype octets () '(vector (unsigned-byte 8)))
(defvar *base64-alphabet* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")