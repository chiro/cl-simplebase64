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
			   collect (base64::padding (base64::make-bits x) 8 0))))

(defun bits2string (bits &optional (safep nil))
  (if safep
      (elt base64::*base64-alphabet-url-and-filename-safe* (bits2integer bits))
      (elt base64::*base64-alphabet* (bits2integer bits))))

(defun bits-list2string (bv &optional safe-p)
  (conc-seq-list string (loop for x in bv collect (string (bits2string x safe-p)))))

;(defgeneric encode (input &optional url-and-filename-safe-p)

(defmethod encode ((str string) &optional url-and-filename-safe-p)
  (padding-4char
   (bits-list2string
    (mapcar
     (lambda (x) (padding x 6 0 t))
     (divide-bits (string2bits str) 6))
    url-and-filename-safe-p)))

(defmethod encode ((stm stream) &optional url-and-filename-safe-p)
    (cond ((not (open-stream-p stm)) (error "stream is not opened"))
          ((not (input-stream-p stm)) (error "stream cannot provide input"))
          (t (mapcar (curry-l #'encode url-and-filename-safe-p)
                     (base64::stream2list stm)))))
