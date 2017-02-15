;;; Encoding
;;; Classes, generic functions, methods and functions for encoding the
;;; residues of biological sequences
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

(in-package :bio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sequence encodings. Sequence encodings map between characters and
;;; character codes that represent resides in biological
;;; sequences. For instance, we may use a 2-bit representation of DNA
;;; sequences in which the residues denoted by the characters A, C, G
;;; and T would be represented by 0, 1, 2, or 3, respectively, in the
;;; underlying storage. Clearly, we would like to be able to express
;;; these sequences in the traditional ACGT manner, so we use sequence
;;; to map between the external (character) and internal (integer, so
;;; far) representations.

;;; sequence encoding protocol class
(defclass sequence-encoding () ())

(define-condition sequence-encoding-error (simple-error) ())

(defun sequence-encoding-error (message &rest args)
  (error 'sequence-encoding-error
         :format-control message
         :format-arguments args))

(defgeneric seq-code-to-char (seq code)
  (:documentation "Returns the character associated with sequence
code code in sequence seq."))

(defgeneric char-to-seq-code (seq char)
  (:documentation "Returns the sequence code for the character char in
in sequence seq."))

;;; dna sequence encoding protocol class

(defclass dna-sequence-encoding (sequence-encoding) ())

;;; 2-bit dna sequence encoding
(defparameter *2-bit-dna-sequence-char-map* #(#\A #\C #\G #\T))
(defparameter *2-bit-dna-char-list* (coerce *2-bit-dna-sequence-char-map* 'list))
    
(defclass 2-bit-dna-sequence-encoding (dna-sequence-encoding)
  ((char-map :reader char-map :allocation :class :initform *2-bit-dna-sequence-char-map*)
   (int-array :accessor int-array
              :allocation :class
              :initform (let ((arr (make-array (char-lookup-array-length *2-bit-dna-char-list*)
                                               :initial-element nil)))
                          (loop for c in *2-bit-dna-char-list*
                             for i from 0
                             do
                             (setf (aref arr (char-code (char-upcase c))) i)
                             (setf (aref arr (char-code (char-downcase c))) i))
                          arr))))
    
(defmethod seq-code-to-char ((seq 2-bit-dna-sequence-encoding) code)
  (aref (char-map seq) code))
    
(defmethod char-to-seq-code ((seq 2-bit-dna-sequence-encoding) char)
  (let ((seq-code (aref (int-array seq) (char-code char))))
    (if seq-code
        seq-code
        (sequence-encoding-error "No encoding found for ~S" char))))

;;; 4-bit dna sequence encoding
;;; 4-bit extended IUPAC nucleic sequence
;;;  0  A - Adenosine
;;;  1  C - Cytosine
;;;  2  G - Guanine
;;;  3  T - Thymine
;;;  4  B - (or C G T) (not A)
;;;  5  D - (or A G T) (not C)
;;;  6  H - (or A C T) (not G)
;;;  7  V - (or A C G) (not T)
;;;  8  S - (or C G)
;;;  9  W - (or A T)
;;;  10 K - (or G T) (Keto)
;;;  11 M - (or A C) (Amino)
;;;  12 R - (or A G) (Purine)
;;;  13 Y - (or C T) (Pyrimidine)
;;;  14 N - (or A C G T)
;;;  15 . - (not (or A C G T))
(defparameter *4-bit-dna-sequence-char-map* #(#\A #\C #\G #\T
                                     #\B #\D #\H #\V
                                     #\S #\W #\K #\M
                                     #\R #\Y #\N #\.))
(defparameter *4-bit-dna-char-list* (coerce *4-bit-dna-sequence-char-map* 'list))
    
(defclass 4-bit-dna-sequence-encoding (dna-sequence-encoding)
  ((char-map :reader char-map :allocation :class :initform *4-bit-dna-sequence-char-map*)
   (int-array :accessor int-array
              :allocation :class
              :initform (let ((arr (make-array (char-lookup-array-length *4-bit-dna-char-list*)
                                               :initial-element nil)))
                          (loop for c in *4-bit-dna-char-list*
                             for i from 0
                             do
                             (setf (aref arr (char-code (char-upcase c))) i)
                             (setf (aref arr (char-code (char-downcase c))) i))
                          arr))))
    
(defmethod seq-code-to-char ((seq 4-bit-dna-sequence-encoding) code)
  (aref (char-map seq) code))
    
(defmethod char-to-seq-code ((seq 4-bit-dna-sequence-encoding) char)
  (let ((seq-code (aref (int-array seq) (char-code char))))
    (if seq-code
        seq-code
        (error 'sequence-encoding-error "No encoding found for ~S" char))))

;;; dna sequence encoding protocol class

(defclass rna-sequence-encoding (sequence-encoding) ())

;;; 2-bit rna sequence encoding

(defparameter *2-bit-rna-sequence-char-map* #(#\A #\C #\G #\U))
(defparameter *2-bit-rna-char-list* (coerce *2-bit-rna-sequence-char-map* 'list))
    
(defclass 2-bit-rna-sequence-encoding (rna-sequence-encoding)
  ((char-map :reader char-map :allocation :class :initform *2-bit-rna-sequence-char-map*)
   (int-array :accessor int-array
              :allocation :class
              :initform (let ((arr (make-array (char-lookup-array-length *2-bit-rna-char-list*)
                                               :initial-element nil)))
                          (loop for c in *2-bit-rna-char-list*
                             for i from 0
                             do
                             (setf (aref arr (char-code (char-upcase c))) i)
                             (setf (aref arr (char-code (char-downcase c))) i))
                          arr))))
    
(defmethod seq-code-to-char ((seq 2-bit-rna-sequence-encoding) code)
  (aref (char-map seq) code))
    
(defmethod char-to-seq-code ((seq 2-bit-rna-sequence-encoding) char)
  (when (char-equal char #\t)
    (setf char #\u))
  (let ((seq-code (aref (int-array seq) (char-code char))))
    (if seq-code
        seq-code
        (error 'sequence-encoding-error "No encoding found for ~S" char))))

(defparameter *acgt-2-bit-rna-sequence-char-map* #(#\A #\C #\G #\T))
(defparameter *acgt-2-bit-rna-char-list* (coerce *acgt-2-bit-rna-sequence-char-map* 'list))
    
(defclass acgt-2-bit-rna-sequence-encoding (rna-sequence-encoding)
  ((char-map :reader char-map :allocation :class :initform *acgt-2-bit-rna-sequence-char-map*)
   (int-array :accessor int-array
              :allocation :class
              :initform (let ((arr (make-array (char-lookup-array-length *acgt-2-bit-rna-char-list*)
                                               :initial-element nil)))
                          (loop for c in *acgt-2-bit-rna-char-list*
                             for i from 0
                             do
                             (setf (aref arr (char-code (char-upcase c))) i)
                             (setf (aref arr (char-code (char-downcase c))) i))
                          arr))))
    
(defmethod seq-code-to-char ((seq acgt-2-bit-rna-sequence-encoding) code)
  (aref (char-map seq) code))
    
(defmethod char-to-seq-code ((seq acgt-2-bit-rna-sequence-encoding) char)
  (when (char-equal char #\u)
    (setf char #\t))
  (let ((seq-code (aref (int-array seq) (char-code char))))
    (if seq-code
        seq-code
        (error 'sequence-encoding-error "No encoding found for ~S" char))))


;;; amino acid sequence encoding protocol class

(defclass aa-sequence-encoding (sequence-encoding) ())

;;; 5-bit amino acide sequence encoding

(defparameter *5-bit-aa-sequence-char-map* #(#\A #\C #\D #\E #\F
                                             #\G #\H #\I #\K #\L
                                             #\M #\N #\P #\Q #\R
                                             #\S #\T #\V #\W #\Y #\*))
(defparameter *5-bit-aa-char-list* (coerce *5-bit-aa-sequence-char-map* 'list))
    
(defclass 5-bit-aa-sequence-encoding (aa-sequence-encoding)
  ((char-map :reader char-map :allocation :class :initform *5-bit-aa-sequence-char-map*)
   (int-array :accessor int-array
              :allocation :class
              :initform (let ((arr (make-array (char-lookup-array-length *5-bit-aa-char-list*)
                                               :initial-element nil)))
                          (loop for c in *5-bit-aa-char-list*
                             for i from 0
                             do
                               (setf (aref arr (char-code (char-upcase c))) i)
                               (setf (aref arr (char-code (char-downcase c))) i))
                          arr))))
    
(defmethod seq-code-to-char ((seq 5-bit-aa-sequence-encoding) code)
  (aref (char-map seq) code))

(defmethod char-to-seq-code ((seq 5-bit-aa-sequence-encoding) char)
  (let ((seq-code (aref (int-array seq) (char-code char))))
    (if seq-code
        seq-code
        (error 'sequence-encoding-error "No encoding found for ~S" char))))
