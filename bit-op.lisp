(in-package :cl-simplebase64)

(defgeneric padding (seq len pad &optional rev))

(defmacro defpadding (type)
  `(defmethod padding ((vec ,type) len pad &optional rev)
     (cond ((>= (length vec) len) vec)
	   (rev (concatenate ',type
			     vec
			     (make-array (- len (length vec))
					 :initial-element pad)))
	   (t (concatenate ',type
			   (make-array (- len (length vec))
				       :initial-element pad)
			   vec)))))

(defpadding string)
(defpadding vector)

(defun make-bits (integer)
  (let ((acc (make-array 0 :fill-pointer 0 :adjustable t)))
    (do ((var integer (floor (/ var 2))))
	((<= var 1)
	 (progn (vector-push-extend (rem var 2) acc)
		(nreverse acc)))
      (vector-push-extend (rem var 2) acc))))

(defun bits2integer (vec)
  (reduce (lambda (x y) (+ (* x 2) y)) vec))

(defun divide-bits (vec block-size)
  (cond ((<= block-size 0) '());(error "zero length error")
	((= (length vec) 0) '(#())); vec = #()
	(t (loop for idx from 0 below (length vec) by block-size
	      collect (if (>= (+ idx block-size) (length vec))
			  (subseq vec idx)
			  (subseq vec idx (+ idx block-size)))))))
