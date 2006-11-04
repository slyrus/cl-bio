;;; FASTA sequence parser
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

(defun read-fasta-header-line (stream)
  (let ((header (read-line stream)))
    (print header)))

(defun check-end-of-sequence (stream)
  (let ((char (peek-char t stream nil :eof)))
    (when (or (equal char #\>)
              (equal char :eof))
      :eof)))

(defun check-end-of-file (stream)
  (equal (peek-char t stream nil :eof) :eof))

(defun read-fasta-residues (stream &key (sequence-type :dna))
  ;; peek a character and if it's, EOF, newline or >, then we're done, otherwise,
  ;; read the line and add it to the sequence
  (flet ((read-fasta-sequence-line ()
             (or (check-end-of-sequence stream)
                 (read-line stream nil :eof))))
    (cond ((eql sequence-type :dna)
           (let ((seq (make-adjustable-dna-sequence 0)))
             (do ((line (read-fasta-sequence-line) (read-fasta-sequence-line)))
                 ((eq line :eof))
               (append-residues seq line))
             seq)))))

(defun read-fasta-sequence (stream &key (sequence-type :dna sequence-type-supplied-p))
  (read-fasta-header-line stream)
  (apply #'read-fasta-residues
         stream
         (when sequence-type-supplied-p
           `(:sequence-type ,sequence-type))))

(defun read-fasta-sequences (stream &key (sequence-type :dna sequence-type-supplied-p))
  "Returns a list whose elements are bio-sequences, populated from the stream."
  (loop while (not (check-end-of-file stream))
     collect 
     (apply #'read-fasta-sequence stream
            (when sequence-type-supplied-p
              `(:sequence-type ,sequence-type)))))

(defun read-fasta-file (file)
  (with-open-file (stream file)
    (read-fasta-sequences stream)))

