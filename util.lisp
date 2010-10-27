(in-package :cl-simplebase64)

(defun curry (fn &rest args)
  (lambda (&rest rest-args)
    (apply fn (append args rest-args))))

(defmacro conc-seq-list (type data)
  `(reduce (curry #'concatenate ',type) ,data))
