;;; FASTA sequence parser
;;;
;;; Copyright (c) 2006-2008 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(in-package :bio-io)

(defun read-fasta-header-line (stream seq)
  ;; read until the first #\>
  (read-until-char stream #\>)
  ;; skip over the #\>
  (read-char stream)
  (let ((header (read-line stream)))
    (let ((terms (cl-ppcre:split #\| header)))
      (do ((term (pop terms) (pop terms)))
          ((null terms))
        (cond ((equal term "gi")
               (let ((gi-id (pop terms)))
                 (add-descriptor
                  seq
                  (make-instance 'ncbi-gi :id gi-id))))
              ((equal term "ref")
               (let ((refseq-id (pop terms)))
                 (add-descriptor
                  seq
                  (make-instance 'refseq-id :id refseq-id)))))))
    header))

(defun read-fasta-residues (stream seq &key (sequence-type :dna))
  (declare (ignore sequence-type))
  ;; peek a character and if it's, EOF, newline or >, then we're done, otherwise,
  ;; read the line and add it to the sequence
  (flet ((read-fasta-sequence-line ()
           (or (check-end-of-sequence stream)
               (read-line stream nil :eof))))
    (do ((line (read-fasta-sequence-line) (read-fasta-sequence-line)))
        ((eq line :eof))
      (append-residues seq line))))

(defun read-fasta-sequence (stream &key (sequence-type :dna sequence-type-supplied-p))
  
  (let ((seq (cond ((eql sequence-type :dna)
                    (make-instance 'bio::4-bit-adjustable-dna-sequence :length 0)))))
    (when seq
      (read-fasta-header-line stream seq)
      (apply #'read-fasta-residues
             stream
             seq
             (when sequence-type-supplied-p
               `(:sequence-type ,sequence-type)))
      seq)))

(defun read-fasta-sequences (stream &key (sequence-type :dna sequence-type-supplied-p))
  "Returns a list whose elements are bio-sequences, populated from the stream."
  (loop while (not (check-end-of-file stream))
     collect 
     (apply #'read-fasta-sequence stream
            (when sequence-type-supplied-p
              `(:sequence-type ,sequence-type)))))

(defun read-fasta-file (filespec)
  (with-open-file (stream filespec)
    (read-fasta-sequences stream)))

(defun write-fasta-file (sequence filespec &key header)
  (with-open-file (stream filespec :direction :output :if-exists :supersede)
    (unless header
      (setf header (format nil "~{~A~^|~}" (mapcar (lambda (x)
                                                     (typecase x
                                                       (identifier (id x))))
                                                   (get-descriptors sequence)))))
    (format stream "~&>~A~%" header)
    (split-string-into-lines (residues-string sequence)
                             :stream stream)
    (terpri stream)))
