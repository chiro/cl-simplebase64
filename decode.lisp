(in-package :cl-simplebase64)

(defun encoded-char2int (char)
  (position char *base64-alphabet*))

(defun octets2string (str)
  #+sbcl (sb-ext:octets-to-string str)
  #-sbcl (error "run this on sbcl"))

(defun make-octets (lst)
  (coerce
   (map 'vector #'bits2integer
	(remove-if #'(lambda (x) (< (length x) 8)) lst)) 'octets))

(defun make-bits (vec)
  (conc-seq-list vector vec))

(defun encoded-string2int (str)
    (map 'vector #'(lambda (x) (padding (make-bits (encoded-char2int x)) 6 0))
	 (remove #\= str)))

(defun decode (str)
  (octets2string
   (make-octets
    (divide-bits (make-bits (encoded-string2int str)) 8))))

