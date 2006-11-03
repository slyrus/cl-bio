
(in-package :cl-bio)

(defun read-fasta-header-line (stream)
  (let ((header (read-line stream)))
    (print header)))

(defun read-fasta-sequence (stream &key (sequence-type :dna))
  ;; peek a character and if it's, EOF, newline or >, then we're done, otherwise,
  ;; read the line and add it to the sequence
  (flet ((read-fasta-sequence-line ()
           (read-line stream)))
    (cond ((eql sequence-type :dna)
           (let ((seq (make-adjustable-dna-sequence 0)))
             (let ((line (read-fasta-sequence-line)))
               (print line)))))))

(defun read-fasta-stream (stream)
  (read-fasta-header-line stream)
  (read-fasta-sequence stream))

