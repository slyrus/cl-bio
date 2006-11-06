;;; Bio-sequence
;;; Classes, generic functions, methods and functions for working
;;; with biological sequences
;;;
;;; Copyright (c) 2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :cl-bio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biological sequences, such as nucleic acid and
;;; protein sequences.

;;; biological sequence protocol class
(defclass bio-sequence () ())

(defgeneric seq-length (seq)
  (:documentation "Returns the length of a bio-sequence. Subclasses of
bio-sequence are free to use arbitrary units for the length, although
it is expected that sequences with residues will return the number
of residues as the length."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sequence with residues. Anticipating the need for sequences
;;; without residues, we explicitly define a sequence-with-reisidues
;;; class and define generic methods that are intended to be
;;; associated with these classes, as well as simple reference
;;; implementation of these functions.

;;; Protocol class for biological sequences having residues
(defclass sequence-with-residues ()
  ((residues :accessor residues)))

(defgeneric residues-string (seq)
  (:documentation "Returns all of the residues of the sequence as a
single string."))

(defgeneric (setf residues-string) (val seq)
  (:documentation "Sets the residues of the sequence to be the residues
contained in the string. The sequence may or may not modify the
string or use the provided string as the storage for the reisdues."))1

(defgeneric residues-string-range (seq range)
  (:documentation "Returns the residues of the sequence in a particular
range."))

(defgeneric residue (seq i)
  (:documentation "Returns the residue of seq at position i. Note
that for residue-coded sequences this is the external character
representation of the residues, not the internal integer
representation."))

(defgeneric (setf residue) (val seq i)
  (:documentation "Sets the ith residue of seq to val."))

;;; some reference implementations for
;;; sequence-with-residues. subclasses are free to override these as
;;; they see fit
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Annotated sequences. Sequences that can have annotations attached
;;; to them.

(defclass annotated-sequence (bio-sequence described-object) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Residue codes. Rather than just using characters to represent the
;;; residues of sequences, we allow for an encoding of a residue as an
;;; integer. The protocol class sequence-with-residue-codes is to be a
;;; superclass of classes that use residue codes and support the
;;; residue-code method and its friends.

;;; seuqence with residue codes protocol class
(defclass sequence-with-residue-codes (sequence-with-residues) ())

(defgeneric residue-code (seq i)
  (:documentation "Returns the residue cod value (the internal
integer representation) of the ith residue of seq."))

(defgeneric (setf residue-code) (val seq i)
  (:documentation "Sets the internal interger representation of the
ith residue of seq to the residue code val."))

(defgeneric seq-reverse (seq)
  (:documentation "Returns a new instance of the same class as seq
whose residues have been reversed (AACCGT -> TGCCAA)"))

(defmethod seq-reverse ((forward sequence-with-residue-codes))
  (let* ((length (seq-length forward))
         (rev (make-instance (class-of forward) :length length)))
    (loop for i below length
       for j from (1- length) downto 0
       do (setf (residue-code rev i) (residue-code forward j)))
    rev)
  
  ;; the following is a slightly less efficient, if simpler way of
  ;; doing the same thing:
  #+nil (make-instance (class-of forward)
                       :initial-contents (reverse (residues-string forward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple sequences. A simple sequence is a sequence that extends
;;; sequence-with-residue-codes (and therefore sequence-with-residues
;;; and bio-sequence) that stores its residues using an array. Simple
;;; operations such as getting and setting residue values are
;;; supported, but more involved operations such as inesrtion and
;;; deletion are not.

;;; simple sequence protocol class
(defclass simple-sequence (sequence-with-residue-codes annotated-sequence) ())

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adjustable sequences. An adjustable sequence is a sequence that
;;; supports operations for insertion, deletion and appening of
;;; residues

;;; adjustable sequence protocol class
(defclass adjustable-sequence (sequence-with-residue-codes) ())

(defgeneric insert-residue (seq pos res))
(defgeneric insert-residues (seq pos str))
(defgeneric insert-residue-codes (seq pos vec))
(defgeneric append-residue (seq res))
(defgeneric append-residues (seq str))
(defgeneric append-residue-codes (seq vec))
(defgeneric delete-residue (seq pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flexichain Sequences. Flexichain sequences are adjustable
;;; sequences that ues a flexichain as the storage for the residues.

;;; flexichain sequences class
(defclass flexichain-sequence (adjustable-sequence annotated-sequence)
  ((initial-element :initform 0)))

(defmethod initialize-instance :after ((seq flexichain-sequence) &rest initargs
                                       &key
                                       length
                                       initial-contents
                                       initial-residue-codes
                                       initial-element)
  (declare (ignore initargs))
  (let ((element-type (slot-value seq 'element-type))
        (length (or (and initial-contents
                         (length initial-contents))
                    length)))
    (setf (residues seq)
          (apply #'make-instance 'flexichain:standard-flexichain
                 :initial-element (or initial-element (slot-value seq 'initial-element))
                 (append (when length `(:initial-nb-elements ,length))
                         (if initial-residue-codes
                             `(:initial-contents ,initial-residue-codes)
                             (when initial-contents `(:initial-contents
                                                      ,(map 'vector
                                                            #'(lambda (c)
                                                                (char-to-seq-code seq c))
                                                            initial-contents))))
                         (when length `(:initial-nb-elements ,length))
                         (when element-type `(:element-type ,element-type)))))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences for handling specific size residues

;;; 2-bit sequence protocol class
(defclass 2-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 2) :allocation :class)))

(defclass 4-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 4) :allocation :class)))

;;; 5-bit sequence protocol class
(defclass 5-bit-sequence (bio-sequence)
  ((element-type :initform '(unsigned-byte 5) :allocation :class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences for handling specific types of biomolecules, such as
;;; nucleic acid, DNA, RNA, or amino acids (proteins).

;;; nucleic acid sequence protocol class
(defclass na-sequence (bio-sequence) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for DNA sequences

;;; DNA sequence protocol class
(defclass dna-sequence (na-sequence) ())

;;; 2-bit DNA sequence protocol class
(defclass 2-bit-dna-sequence (dna-sequence 2-bit-sequence 2-bit-dna-sequence-encoding) ())

;;; 2-bit DNA sequence protocol class
(defclass 4-bit-dna-sequence (dna-sequence 4-bit-sequence 4-bit-dna-sequence-encoding) ())

(defgeneric reverse-complement (seq)
  (:documentation "Returns a new sequence that is the reverse
complement of seq."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for DNA sequences

;;; simple DNA sequence class
(defclass simple-dna-sequence (2-bit-dna-sequence simple-sequence) ())

;;; adjustable DNA sequence class
(defclass adjustable-dna-sequence (2-bit-dna-sequence flexichain-sequence) ())

(defclass 4-bit-adjustable-dna-sequence (4-bit-dna-sequence
                                        flexichain-sequence) ())

(defun make-simple-dna-sequence (length)
  (make-instance 'simple-dna-sequence :length length))

(defun make-adjustable-dna-sequence (length)
  (make-instance 'adjustable-dna-sequence :length length))

(defun make-dna-sequence-from-string (residues)
  (make-instance 'simple-dna-sequence :initial-contents residues))

(defun make-random-dna-sequence (length)
  (let* ((dna (make-simple-dna-sequence length)))
    (let ((k 4))
      (loop for i below length
         do (setf (residue-code dna i) (random k))))
    dna))

(defmethod reverse-complement ((forward 2-bit-dna-sequence))
  (let* ((length (seq-length forward))
         (revcomp (make-instance (class-of forward) :length length)))
    (loop for i below length
       for j from (1- length) downto 0
       do (setf (residue-code revcomp i) (- 3 (residue-code forward j))))
    revcomp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for RNA sequences

;;; RNA sequence protocol class
(defclass rna-sequence (na-sequence) ())

;;; 2-bit RNA sequence protocol class
(defclass 2-bit-rna-sequence (rna-sequence 2-bit-sequence 2-bit-rna-sequence-encoding) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for RNA sequences

;;; simple RNA sequence class
(defclass simple-rna-sequence (2-bit-rna-sequence simple-sequence) ())

;;; adjustable RNA sequence class
(defclass adjustable-rna-sequence (2-bit-rna-sequence flexichain-sequence) ())

(defun make-simple-rna-sequence (length)
  (make-instance 'simple-rna-sequence :length length))

(defun make-adjustable-rna-sequence (length)
  (make-instance 'adjustable-rna-sequence :length length))

(defun make-rna-sequence-from-string (residues)
  (make-instance 'simple-rna-sequence :initial-contents residues))

(defun make-random-rna-sequence (length)
  (let* ((rna (make-simple-rna-sequence length)))
    (let ((k 4))
      (loop for i below length
         do (setf (residue-code rna i) (random k))))
    rna))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for amino acid sequences

;;; amino acid sequence protocol class
(defclass aa-sequence (bio-sequence) ())

;;; 5-bit amino acid sequence protocol class
(defclass 5-bit-aa-sequence (aa-sequence 5-bit-sequence aa-sequence-encoding) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for amino acid sequences

;;; simple amino acid sequence class
(defclass simple-aa-sequence (5-bit-aa-sequence simple-sequence) ())

;;; adjustable amino acid sequence class
(defclass adjustable-aa-sequence (5-bit-aa-sequence flexichain-sequence) ())

(defun make-simple-aa-sequence (length)
  (make-instance 'simple-aa-sequence :length length))

(defun make-aa-sequence-from-string (residues)
  (make-instance 'simple-aa-sequence :initial-contents residues))

(defun make-random-aa-sequence (length)
  (let* ((aa (make-simple-aa-sequence length)))
    (let ((k 20))
      (loop for i below length
         do (setf (residue-code aa i) (random k))))
    aa))

