
(in-package #:cl-user)

(defpackage #:cl-bio (:use #:cl)

            (:export

             ;; ranges
             #:range
             #:ds-range
             
             #:range-equal
             #:range-contains
             
             #:+plus-strand+
             #:+unknown-strand+
             #:+minus-strand+
             #:+both-strands+

             ;; bio-sequence
             #:bio-sequence

             #:residues-string
             #:seq-length
             
             #:na-sequence
             #:dna-sequence
             #:rna-sequence
             #:aa-sequence

             #:make-dna-sequence-from-string
             #:make-random-dna-sequence

             #:make-rna-sequence-from-string
             #:make-random-rna-sequence

             #:make-aa-sequence-from-string
             #:make-random-aa-sequence

             ))
