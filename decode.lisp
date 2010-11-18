(in-package :cl-simplebase64)

(defun encoded-char2int (char &optional safe-p)
  (if safe-p
      (position char *base64-alphabet-url-and-filename-safe*)
      (position char *base64-alphabet*)))

(defun octets2string (str)
  #+sbcl (sb-ext:octets-to-string str)
  #-sbcl (error "run this on sbcl"))

(defun make-octets (lst)
  (coerce
   (map 'vector #'bits2integer
	(remove-if #'(lambda (x) (< (length x) 8)) lst)) 'octets))

(defun encoded-string2int (str &optional safe-p)
    (map 'vector #'(lambda (x) (padding (make-bits (encoded-char2int x safe-p)) 6 0 nil 'bit))
	 (remove #\= str)))

(defgeneric decode (input &optional url-and-filename-safe-p))

(defmethod decode ((str string) &optional url-and-filename-safe-p)
  (octets2string
   (make-octets
    (divide-bits
     (conc-seq-list vector
                    (encoded-string2int str url-and-filename-safe-p)) 8))))

(defmethod decode ((stm stream) &optional url-and-filename-safe-p)
  (cond ((not (open-stream-p stm)) (error "stream is not opened"))
	((not (input-stream-p stm)) (error "stream cannot provide input"))
	(t (mapcar
        (base64::curry-l #'decode url-and-filename-safe-p)
        (base64::stream2list stm)))))

