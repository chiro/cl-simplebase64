;;;;This file contains bit-operation utilities
(in-package :cl-simplebase64)

;;;if length of vec is below len, padding pad to vec
(defgeneric padding (seq len pad &optional rev))

(defmacro defpaddingrule (type)
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

(defpaddingrule string)
(defpaddingrule vector)

;;;make vector of bit from integer
;;;ex. 4 -> #(1 0 0)
(defun make-bits (integer)
  (let ((acc (make-array 0 :fill-pointer 0 :adjustable t)))
    (do ((var integer (floor (/ var 2))))
	((<= var 1)
	 (progn (vector-push-extend (rem var 2) acc)
		(nreverse acc)))
      (vector-push-extend (rem var 2) acc))))

;;;return integer from vector of bit
;;;ex. #(1 0 1) -> 5
(defun bits2integer (vec)
  (reduce (lambda (x y) (+ (* x 2) y)) vec))

;;;separate each block-size bit from vector of bit
(defun divide-bits (vec block-size)
  (loop for idx from 0 below (length vec) by block-size
     collect (if (>= (+ idx block-size) (length vec))
		 (subseq vec idx)
		 (subseq vec idx (+ idx block-size)))))
