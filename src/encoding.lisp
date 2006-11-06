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

(in-package :cl-bio)

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

(defgeneric seq-code-to-char (seq code)
  (:documentation "Returns the character associated with sequence
code code in sequence seq."))

(defgeneric char-to-seq-code (seq char)
  (:documentation "Returns the sequence code for the character char in
in sequence seq."))

;;; 2-bit dna sequence encoding
(let ((2-bit-dna-sequence-char-map #(#\A #\C #\G #\T)))
  (let ((char-list (coerce 2-bit-dna-sequence-char-map 'list)))
    
    (defclass 2-bit-dna-sequence-encoding (sequence-encoding)
      ((char-map :reader char-map :allocation :class :initform 2-bit-dna-sequence-char-map)
       (int-array :accessor int-array
                  :allocation :class
                  :initform (let ((arr (make-array (char-lookup-array-length char-list)
                                                   :initial-element nil)))
                              (loop for c in char-list
                                 for i from 0
                                 do
                                   (setf (aref arr (char-code (char-upcase c))) i)
                                   (setf (aref arr (char-code (char-downcase c))) i))
                              arr))))
    
    (defmethod seq-code-to-char ((seq 2-bit-dna-sequence-encoding) code)
      (aref (char-map seq) code))
    
    (defmethod char-to-seq-code ((seq 2-bit-dna-sequence-encoding) char)
      (aref (int-array seq) (char-code char)))))

;;; 4-bit dna sequence encoding
;;; 4-bit extended IUPAC nucleic sequence
;;;  0  A - Adenosine
;;;  1  C - Cytosine
;;;  2  G - Guanine
;;;  3  T - Thymine
;;;  4  B - NOT A (C, G or T)
;;;  5  D - NOT C
;;;  6  H - NOT G
;;;  7  V - NOT T
;;;  8  S - A or T
;;;  9 W - C or G
;;;  10 K - Keto
;;;  11 M - Amino
;;;  12 R - Purine
;;;  13 Y - Pyrimidine
;;;  14 N - Any

(let ((4-bit-dna-sequence-char-map #(#\A #\C #\G #\T
                                      #\B #\D #\H #\V
                                      #\S #\W #\K #\M
                                      #\R #\Y #\N #\Space)))
  (let ((char-list (coerce 4-bit-dna-sequence-char-map 'list)))
    
    (defclass 4-bit-dna-sequence-encoding (sequence-encoding)
      ((char-map :reader char-map :allocation :class :initform 4-bit-dna-sequence-char-map)
       (int-array :accessor int-array
                  :allocation :class
                  :initform (let ((arr (make-array (char-lookup-array-length char-list)
                                                   :initial-element nil)))
                              (loop for c in char-list
                                 for i from 0
                                 do
                                 (setf (aref arr (char-code (char-upcase c))) i)
                                 (setf (aref arr (char-code (char-downcase c))) i))
                              arr))))
    
    (defmethod seq-code-to-char ((seq 4-bit-dna-sequence-encoding) code)
      (aref (char-map seq) code))
    
    (defmethod char-to-seq-code ((seq 4-bit-dna-sequence-encoding) char)
      (aref (int-array seq) (char-code char)))))

;;; 2-bit rna sequence encoding

(let ((2-bit-rna-sequence-char-map #(#\A #\C #\G #\U)))
  (let ((char-list (coerce 2-bit-rna-sequence-char-map 'list)))
    
    (defclass 2-bit-rna-sequence-encoding (sequence-encoding)
      ((char-map :reader char-map :allocation :class :initform 2-bit-rna-sequence-char-map)
       (int-array :accessor int-array
                  :allocation :class
                  :initform (let ((arr (make-array (char-lookup-array-length char-list)
                                                   :initial-element nil)))
                              (loop for c in char-list
                                 for i from 0
                                 do
                                   (setf (aref arr (char-code (char-upcase c))) i)
                                   (setf (aref arr (char-code (char-downcase c))) i))
                              arr))))
    
    (defmethod seq-code-to-char ((seq 2-bit-rna-sequence-encoding) code)
      (aref (char-map seq) code))
    
    (defmethod char-to-seq-code ((seq 2-bit-rna-sequence-encoding) char)
      (aref (int-array seq) (char-code char)))))


;;; 5-bit amino acide sequence encoding


(let ((5-bit-aa-sequence-char-map #(#\A #\C #\D #\E #\F
                                      #\G #\H #\I #\K #\L
                                      #\M #\N #\P #\Q #\R
                                      #\S #\T #\V #\W #\Y)))
  (let ((char-list (coerce 5-bit-aa-sequence-char-map 'list)))
    
    (defclass 5-bit-aa-sequence-encoding (sequence-encoding)
      ((char-map :reader char-map :allocation :class :initform 5-bit-aa-sequence-char-map)
       (int-array :accessor int-array
                  :allocation :class
                  :initform (let ((arr (make-array (char-lookup-array-length char-list)
                                                   :initial-element nil)))
                              (loop for c in char-list
                                 for i from 0
                                 do
                                   (setf (aref arr (char-code (char-upcase c))) i)
                                   (setf (aref arr (char-code (char-downcase c))) i))
                              arr))))
    
    (defmethod seq-code-to-char ((seq 5-bit-aa-sequence-encoding) code)
      (aref (char-map seq) code))
    
    (defmethod char-to-seq-code ((seq 5-bit-aa-sequence-encoding) char)
      (aref (int-array seq) (char-code char)))))
