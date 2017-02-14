
(defpackage #:bio-sequence-util
  (:use #:cl #:bio)
  (:nicknames #:seq)
  (:shadow #:string
           #:length)
  (:export #:string
           #:length))

(in-package :seq)

(defun string (sequence)
  (residues-string sequence))

(defun length (sequence)
  (seq-length sequence))


