
(in-package :cl-bio)

;; sequences

(defclass bio-sequence () ())

(defgeneric seq-length (seq))
(defgeneric residues-string (seq))

(defclass sequence-with-residues ()
  ((residues :accessor residues)))

(defmethod seq-length ((seq sequence-with-residues))
  (length (residues seq)))

(defmethod residues-string ((seq sequence-with-residues))
  (let ((str (make-string (seq-length seq))))
    (loop for i from 0 below (seq-length seq)
       do (setf (elt str i) (residue seq i)))
    str))

(defmethod (setf residues-string) (residues (seq sequence-with-residues))
  (loop for res across residues
       for i from 0
       do (setf (residue seq i) (elt residues i)))
  residues)

;;; TODO: add (setf residues-string) and use that down in the make-foo-seqeuence-from-string methods

;;; TODO: rework make-random-foo-sequence methods to use (setf residue)

(defgeneric residue (seq i))
(defgeneric (setf residue) (val seq i))
(defgeneric residue-code (seq i))
(defgeneric (setf residue-code) (val seq i))

(defclass simple-sequence (sequence-with-residues) ())

(defmethod initialize-instance :after ((seq simple-sequence) &rest initargs
                                       &key length initial-contents)
  (declare (ignore initargs))
  (let ((element-type (slot-value seq 'element-type))
        (length (or (and initial-contents
                         (length initial-contents))
                    length)))
    (setf (residues seq)
          (apply #'make-array length
                 (when element-type `(:element-type ,element-type)))))
  (when initial-contents
    (setf (residues-string seq) initial-contents)))

(defmethod residue ((seq simple-sequence) i)
  (seq-code-to-char seq (aref (residues seq) i)))

(defmethod (setf residue) (val (seq simple-sequence) i)
  (setf (aref (residues seq) i)
        (char-to-seq-code seq val)))

(defmethod residue-code ((seq simple-sequence) i)
  (aref (residues seq) i))

(defmethod (setf residue-code) (val (seq simple-sequence) i)
  (setf (aref (residues seq) i) val))

(defclass na-sequence (bio-sequence) ())

(defclass dna-sequence (na-sequence) ())

(defclass 2-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 2) :allocation :class)))

(defclass rna-sequence (na-sequence) ())

(defclass simple-dna-sequence (dna-sequence simple-sequence 2-bit-sequence 2-bit-dna-sequence-encoding)  ())

(defun make-simple-dna-sequence (length)
  (make-instance 'simple-dna-sequence
                 :length length))

(defun make-dna-sequence-from-string (residues)
  (let* ((seq (make-simple-dna-sequence (length residues))))
    (setf (residues-string seq) residues)
    seq))

(defun make-random-dna-sequence (length)
  (let* ((dna (make-simple-dna-sequence length)))
    (let ((k (length *simple-dna-sequence-char-map*)))
      (loop for i below length
         do (setf (residue-code dna i) (random k))))
    dna))

(defclass simple-rna-sequence (rna-sequence simple-sequence 2-bit-rna-sequence-encoding) ())

(defun make-simple-rna-sequence (length)
  (let* ((storage (make-array length :element-type '(unsigned-byte 2)))
         (rna (make-instance 'simple-rna-sequence)))
    (declare (type (simple-array (unsigned-byte 2))
                   storage))
    (setf (residues rna) storage)
    rna))

(defun make-rna-sequence-from-string (residues)
  (let* ((seq (make-simple-rna-sequence (length residues))))
    (setf (residues-string seq) residues)
    seq))

(defun make-random-rna-sequence (length)
  (let* ((rna (make-simple-rna-sequence length))
         (storage (residues rna)))
    (declare (type (simple-array (unsigned-byte 2))
                   storage))
    (setf (residues rna) storage)
    (let ((k (length *simple-rna-sequence-char-map*)))
      (loop for i below length
         do (setf (aref storage i)
                  (random k))))
    rna))

(defclass aa-sequence (bio-sequence) ())

(defclass simple-aa-sequence (aa-sequence simple-sequence aa-sequence-encoding) ())

(defun make-simple-aa-sequence (length)
  (let* ((storage (make-array length :element-type '(unsigned-byte 5)))
         (aa (make-instance 'simple-aa-sequence)))
    (declare (type (simple-array (unsigned-byte 5))
                   storage))
    (setf (residues aa) storage)
    aa))

(defun make-aa-sequence-from-string (residues)
  (let* ((aa (make-simple-aa-sequence (length residues))))
    (loop for res across residues
       for i from 0
       do (setf (residue aa i) (elt residues i)))
    aa))

(defun make-random-aa-sequence (length)
  (let* ((aa (make-simple-aa-sequence length))
         (storage (residues aa)))
    (declare (type (simple-array (unsigned-byte 5))
                   storage))
    (let ((k (length *simple-aa-sequence-char-map*)))
      (loop for i below length
         do (setf (aref storage i) (random k))))
    aa))




;;;; OPTIMIZATION HACKS

;;;
;;; if we ever need a faster version of the make-random-foo-sequence functions,
;;; we can use the following template:
;;;

#+bio-sequence-optimization-hacks
(defun %fast-make-random-dna-sequence (length)
  (let* ((dna (make-simple-dna-sequence length))
         (storage (residues dna)))
    (declare (type (simple-array (unsigned-byte 2))
                   storage))
    (setf (residues dna) storage)
    (let ((k (length *simple-dna-sequence-char-map*)))
      (loop for i below length
         do (setf (aref storage i)
                  (random k))))
    dna))


