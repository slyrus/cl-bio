
(in-package :cl-bio)

;; utility functions

(defun char-lookup-array-length (char-list)
  (1+ (apply #'max
             (mapcar #'char-code
                     (mapcan #'(lambda (x)
                                 (list (char-upcase x)
                                       (char-downcase x)))
                             char-list)))))

(defun flexichain-to-list (fc)
  (loop for i below (flexichain:nb-elements fc)
       collect (flexichain:element* fc i)))

(defun general-flexichain-to-string (fc)
  (coerce (loop for i below (flexichain:nb-elements fc)
             append (let ((el (flexichain:element* fc i)))
                      (cond ((characterp el)
                             (list el))
                            ((stringp el)
                             (coerce el 'list)))))
          'string))

(defun vector-flexichain-to-string (fc)
  (coerce (loop for i below (flexichain:nb-elements fc)
             collect (flexichain:element* fc i))
          'string))
