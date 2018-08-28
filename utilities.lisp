
(in-package :bio)

;; utility functions

(defun split-string-into-lines-list (string &key (max-line-length 70))
  (let ((line-buffer (make-string max-line-length)))
    (with-input-from-string (stream string)
      (loop for count = (read-sequence line-buffer stream)
           while (plusp count)
         collect (subseq line-buffer 0 count)))))

(defgeneric split-string-into-lines (string &key stream max-line-length))

(defmethod split-string-into-lines (string &key stream max-line-length)
  (format stream
          "~{~A~^~&~}"
          (apply #'split-string-into-lines-list string
                 (when max-line-length `(:max-line-length ,max-line-length)))))

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
  "Returns a list of the occurences of seq1 in seq2. Note that the
instances of seq1 in seq2 can overlap such that (find-matches \"AA\"
  \"AAA\" returns (0 1)."
  (butlast
   (loop with i = 0 while i
      collect (setf i (search seq1 seq2 :start2 i))
      when i do (incf i))))
