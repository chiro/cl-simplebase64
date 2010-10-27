(in-package :cl-simplebase64)

;;環境依存
(defun string2octets (str)
  #+sbcl (sb-ext:string-to-octets str)
  #+clisp (ext:convert-string-to-bytes)
  #-(or sbcl clisp) (error "run this on sbcl or clisp"))

(defun padding-4char (str)
  (padding str (+ (length str) (rem (- 4 (rem (length str) 4)) 4)) #\= t))

(defun string2bits (str)
  (conc-seq-list vector (loop for x across (string2octets str)
			   collect (padding (make-bits x) 8 0))))

(defun bits2string (bits)
  (elt *base64-alphabet* (bits2integer bits)))

(defun bits-list2string (bv)
  (conc-seq-list string (loop for x in bv collect (string (bits2string x)))))

(defun encode (str)
  (padding-4char
   (bits-list2string
    (mapcar
     (lambda (x) (padding x 6 0 t))
     (divide-bits (string2bits str) 6)))))