
(in-package :bio)

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

(defun find-matches (seq1 seq2)
  (let ((l))
    (loop with i = 0 while i
       do
         (let ((pos (setf i (search seq1 seq2 :start2 i))))
           (when pos (push pos l))
           (when i (incf i))))
    (reverse l)))
