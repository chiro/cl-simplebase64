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

(defun encoded-string2int (str)
    (map 'vector #'(lambda (x) (padding (make-bits (encoded-char2int x)) 6 0 nil 'bit))
	 (remove #\= str)))

(defgeneric decode (input))

(defmethod decode ((str string))
  (octets2string
   (make-octets
    (divide-bits (conc-seq-list vector (encoded-string2int str)) 8))))

(defmethod decode ((stm stream))
  (cond ((not (open-stream-p stm)) (error "stream is not opened"))
	((not (input-stream-p stm)) (error "stream cannot provide input"))
	(t (decode (read-line stm)))))
