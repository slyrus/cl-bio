
;; bio-sequence0 is a set of routines for working with 0-based
;; indices. The routines provided here are analogous to those in the
;; bio-sequence1 package, but use 0-based indexing and are generally
;; just wrappers for routines the bio-sequence package.
;;

(defpackage #:bio-sequence0
  (:use #:cl #:bio)
  (:nicknames #:seq0)
  (:shadow #:elt
           #:subseq
           #:translate)
  (:export #:elt
           #:subseq
           #:substring
           #:translate))

(in-package :seq0)

(defun elt (sequence index)
  (residue sequence index))

(defun (setf elt) (val sequence index)
  (setf (residue sequence index) val))

(defun subseq (sequence start &optional (end (seq-length sequence)))
  (copy-sequence-range sequence (range start end)))

(defun substring (sequence start &optional (end (seq-length sequence)))
  (residues-string-range sequence (range start end)))

(defun (setf substring) (str sequence start &optional (end (seq-length sequence)))
  (setf (residues-string-range sequence (range start end)) str))

(defun translate (sequence &key (start 0) (end (seq-length sequence)))
  (bio:translate sequence :range (range start end)))


