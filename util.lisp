(in-package :cl-simplebase64)

(defun curry (fn &rest args)
  (lambda (&rest rest-args)
    (apply fn (append args rest-args))))

(defun curry-l (fn &rest args)
  (lambda (&rest rest-args)
    (apply fn (append rest-args args))))

(defmacro conc-seq-list (type data)
  `(reduce (curry #'concatenate ',type) ,data))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun vector= (vec1 vec2)
  (if (/= (length vec1) (length vec2)) nil
      (progn (loop for x from 0 below (length vec1)
		do (when (not (equal (aref vec1 x) (aref vec2 x)))
		     (return-from vector= nil)))
	     t)))

(defun vector-list= (vecl1 vecl2)
  (if (/= (length vecl1) (length vecl2)) nil
      (progn (loop for x from 0 below (length vecl1)
		  do (when (not (vector= (elt vecl1 x) (elt vecl2 x)))
		       (return-from vector-list= nil)))
	     t)))

(defun stream2list (stream)
  (loop for str = (read-line stream nil 'eof) until (eql str 'eof) collect str))
