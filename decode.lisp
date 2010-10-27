(in-package :cl-simplebase64)

;;;decode Base64
(defun decode (str)
  (base64::octets2string
   (base64::make-octetvector
    (base64::divide-bits
     (base64::make-bits
      (base64::encoded2int str))
     8))))

(defun make-bits (vec)
  (conc-seq-list vector vec))

(defun encoded2int (str)
    (map 'vector #'(lambda (x) (padding (make-bits (encodedchar2int x)) 6 0))
	 (remove #\= str)))

(defun octets2string (str)
  #+sbcl (sb-ext:octets-to-string str)
  #-(or sbcl allegro clisp) (error "run this on sbcl"))

(defun encodedchar2int (char)
  (position char base64::*base64-alphabet*))

(defun make-octetvector (lst)
  (coerce
   (map 'vector #'base64::bits2integer
	(remove-if #'(lambda (x) (< (length x) 8)) lst)) 'octets))
