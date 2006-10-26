
(in-package :cl-bio)

(defgeneric seq-code-to-char (seq code))
(defgeneric char-to-seq-code (seq char))

(defclass sequence-encoding () ())

(defclass 2-bit-dna-sequence-encoding (sequence-encoding) ())

(defparameter *simple-dna-sequence-char-map* #(#\A #\C #\G #\T))

(defmethod seq-code-to-char ((seq 2-bit-dna-sequence-encoding) code)
  (aref *simple-dna-sequence-char-map* code))

(let ((char-list (coerce *simple-dna-sequence-char-map* 'list)))
  (let ((simple-dna-sequence-int-array
         (make-array (char-lookup-array-length char-list))))
    (loop for c in char-list
       for i from 0
       do
         (setf (aref simple-dna-sequence-int-array (char-code (char-upcase c))) i)
         (setf (aref simple-dna-sequence-int-array (char-code (char-downcase c))) i))
    
    (defmethod char-to-seq-code ((seq simple-dna-sequence) char)
      (aref simple-dna-sequence-int-array (char-code char)))))

(defclass 2-bit-rna-sequence-encoding (sequence-encoding) ())

(defparameter *simple-rna-sequence-char-map* #(#\A #\C #\G #\U))

(defmethod seq-code-to-char ((seq 2-bit-rna-sequence-encoding) code)
  (aref *simple-rna-sequence-char-map* code))

(let ((char-list (coerce *simple-rna-sequence-char-map* 'list)))
  (let ((simple-rna-sequence-int-array
         (make-array (char-lookup-array-length char-list))))
    (loop for c in char-list
       for i from 0
       do
         (setf (aref simple-rna-sequence-int-array (char-code (char-upcase c))) i)
         (setf (aref simple-rna-sequence-int-array (char-code (char-downcase c))) i))
    
    (defmethod char-to-seq-code ((seq 2-bit-rna-sequence-encoding) char)
      (aref simple-rna-sequence-int-array (char-code char)))))


(defclass aa-sequence-encoding (sequence-encoding) ())

(defparameter *simple-aa-sequence-char-map* #(#\A #\C #\D #\E #\F
                                              #\G #\H #\I #\K #\L
                                              #\M #\N #\P #\Q #\R
                                              #\S #\T #\V #\W #\Y))

(defmethod seq-code-to-char ((seq aa-sequence-encoding) code)
  (aref *simple-aa-sequence-char-map* code))

(let ((char-list (coerce *simple-aa-sequence-char-map* 'list)))
  (let ((simple-aa-sequence-int-array
         (make-array (char-lookup-array-length char-list))))
    (loop for c in char-list
       for i from 0
       do
       (setf (aref simple-aa-sequence-int-array (char-code (char-upcase c))) i)
       (setf (aref simple-aa-sequence-int-array (char-code (char-downcase c))) i))

    (defmethod char-to-seq-code ((seq aa-sequence-encoding) char)
      (aref simple-aa-sequence-int-array (char-code char)))))

