
(in-package :cl-bio)

;; sequences

(defclass bio-sequence () ())

(defgeneric seq-length (seq))
(defgeneric residues-string (seq))
(defgeneric (setf residues-string) (val seq))
(defgeneric residues-string-range (seq range))
(defgeneric residue (seq i))
(defgeneric (setf residue) (val seq i))

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

(defmethod residues-string-range ((seq sequence-with-residues) (range range))
  (let ((str (make-string (range-length range))))
    (loop
       for i from (range-start range) below (range-end range)
       for j from 0
       do (setf (elt str j) (residue seq i)))
    str))

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
    (when element-type
      (setf (slot-value seq 'element-type)
            (upgraded-array-element-type element-type)))
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


(defgeneric insert-residue (seq pos res))
(defgeneric insert-residues (seq pos str))
(defgeneric insert-residue-codes (seq pos vec))
(defgeneric append-residues (seq res))
(defgeneric append-residues (seq str))
(defgeneric append-residue-codes (seq vec))
(defgeneric delete-residue (seq pos))

(defclass flexichain-sequence (sequence-with-residues) ())

(defmethod initialize-instance :after ((seq flexichain-sequence) &rest initargs
                                       &key length initial-contents)
  (declare (ignore initargs))
  (let ((element-type (slot-value seq 'element-type))
        (length (or (and initial-contents
                         (length initial-contents))
                    length)))
    (setf (residues seq)
          (apply #'make-instance 'flexichain:standard-flexichain
                 (append (when length `(:min-size ,length))
                         (when element-type `(:element-type ,element-type))))))
  (when initial-contents
    (let ((chain (residues seq)))
      (loop for res across initial-contents
         do (flexichain:push-end chain (char-to-seq-code seq res))))))

(defmethod seq-length ((seq flexichain-sequence))
  (flexichain:nb-elements (residues seq)))

(defmethod residue ((seq flexichain-sequence) i)
  (seq-code-to-char seq (flexichain:element* (residues seq) i)))

(defmethod (setf residue) (val (seq flexichain-sequence) i)
  (setf (flexichain:element* (residues seq) i)
        (char-to-seq-code seq val)))

(defmethod residue-code ((seq flexichain-sequence) i)
  (flexichain:element* (residues seq) i))

(defmethod (setf residue-code) (val (seq flexichain-sequence) i)
  (setf (flexichain:element* (residues seq) i) val))

(defmethod insert-residue ((seq flexichain-sequence) pos res)
  (flexichain:insert* (residues seq)
                      pos
                      (char-to-seq-code seq res)))

(defmethod insert-residues ((seq flexichain-sequence) pos str)
  (flexichain:insert-vector* (residues seq)
                             pos
                             (map (if (slot-value seq 'element-type)
                                      `(vector ,(slot-value seq 'element-type))
                                      'vector)
                                  #'(lambda (x)
                                      (char-to-seq-code seq x))
                                  str)))

(defmethod insert-residue-codes ((seq flexichain-sequence) pos vec)
  (flexichain:insert-vector* (residues seq) pos vec))


(defmethod append-residue ((seq flexichain-sequence) res)
  (flexichain:push-end (residues seq)
                       (char-to-seq-code seq res)))

(defmethod append-residues ((seq flexichain-sequence) str)
  (flexichain:insert-vector* (residues seq)
                             (seq-length seq)
                             (map (if (slot-value seq 'element-type)
                                      `(vector ,(slot-value seq 'element-type))
                                      'vector)
                                  #'(lambda (x)
                                      (char-to-seq-code seq x))
                                  str)))

(defmethod append-residue-codes ((seq flexichain-sequence) vec)
  (flexichain:insert-vector* (residues seq) (seq-length seq) vec))

(defmethod delete-residue ((seq flexichain-sequence) pos)
  (flexichain:delete* (residues seq) pos))

(defmethod delete-residues ((seq flexichain-sequence) pos count)
  (loop for i below count
     do (flexichain:delete* (residues seq) pos)))


(defclass 2-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 2) :allocation :class)))

(defclass 5-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 5) :allocation :class)))

;;; nucleic acid sequences
(defclass na-sequence (bio-sequence) ())

;;; DNA sequences
(defclass dna-sequence (na-sequence) ())
(defclass 2-bit-dna-sequence (dna-sequence 2-bit-sequence 2-bit-dna-sequence-encoding) ())
(defclass simple-dna-sequence (2-bit-dna-sequence simple-sequence) ())
(defclass flexichain-dna-sequence (2-bit-dna-sequence flexichain-sequence) ())

(defun make-simple-dna-sequence (length)
  (make-instance 'simple-dna-sequence :length length))

(defun make-dna-sequence-from-string (residues)
  (make-instance 'simple-dna-sequence :initial-contents residues))

(defun make-random-dna-sequence (length)
  (let* ((dna (make-simple-dna-sequence length)))
    (let ((k (length *simple-dna-sequence-char-map*)))
      (loop for i below length
         do (setf (residue-code dna i) (random k))))
    dna))

;;; RNA sequences
(defclass rna-sequence (na-sequence) ())
(defclass 2-bit-rna-sequence (rna-sequence 2-bit-sequence 2-bit-rna-sequence-encoding) ())
(defclass simple-rna-sequence (2-bit-rna-sequence simple-sequence) ())
(defclass flexichain-rna-sequence (2-bit-rna-sequence flexichain-sequence) ())

(defun make-simple-rna-sequence (length)
  (make-instance 'simple-rna-sequence :length length))

(defun make-rna-sequence-from-string (residues)
  (make-instance 'simple-rna-sequence :initial-contents residues))

(defun make-random-rna-sequence (length)
  (let* ((rna (make-simple-rna-sequence length)))
    (let ((k (length *simple-rna-sequence-char-map*)))
      (loop for i below length
         do (setf (residue-code rna i) (random k))))
    rna))

;;; amino acid sequences
(defclass aa-sequence (bio-sequence) ())
(defclass 5-bit-aa-sequence (aa-sequence 5-bit-sequence aa-sequence-encoding) ())
(defclass simple-aa-sequence (5-bit-aa-sequence simple-sequence) ())
(defclass flexichain-aa-sequence (5-bit-aa-sequence flexichain-sequence) ())

(defun make-simple-aa-sequence (length)
  (make-instance 'simple-aa-sequence :length length))

(defun make-aa-sequence-from-string (residues)
  (make-instance 'simple-aa-sequence :initial-contents residues))

(defun make-random-aa-sequence (length)
  (let* ((aa (make-simple-aa-sequence length)))
    (let ((k (length *simple-aa-sequence-char-map*)))
      (loop for i below length
         do (setf (residue-code aa i) (random k))))
    aa))

