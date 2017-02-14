
;; bio-sequence1 is a set of routines for working with both 1-based
;; indices, rather than 0-based indices. We use 0-based indices for
;; bio-sequence, but it is often desirable to work with 1-based
;; sequences, particularly when working with human-generated
;; annotations of genes and proteins, e.g., features identified in the
;; literature.
;;

(defpackage #:bio-sequence1
  (:use #:cl #:bio)
  (:nicknames #:seq1)
  (:shadow #:elt
           #:subseq
           #:translate)
  (:export #:elt
           #:subseq
           #:substring
           #:translate))

(in-package :seq1)

(defun elt (sequence index)
  (residue sequence (1- index)))

(defun (setf elt) (val sequence index)
  (setf (residue sequence (1- index)) val))

(defun subseq (sequence start &optional (end (1+ (seq-length sequence))))
  (copy-sequence-range sequence (range (1- start) (1- end))))

(defun substring (sequence start &optional (end (1+ (seq-length sequence))))
  (residues-string-range sequence (range (1- start) (1- end))))

(defun (setf substring) (str sequence start &optional (end (1+ (seq-length sequence))))
  (setf (residues-string-range sequence (range (1- start) (1- end))) str))

(defun translate (sequence &key (start 1) (end (1+ (seq-length sequence))))
  (bio:translate sequence :range (range (1- start) (1- end))))

