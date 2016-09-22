
;;;
;;; CLH: what does this comment mean? FIXME!
;;; this gives the wrong results!!!

(in-package #:cl-user)

(defpackage #:align-test
  (:use #:cl #:bio #:align)
  (:import-from :prove
                :plan
                :ok
                :is
                :finalize))

(in-package :align-test)

(plan nil)

(ok
 (alignment-results (global-align-na "AATTC" "GAT")))

(ok
 (alignment-results
  (global-align-na 
   (make-dna-sequence-from-string "gcgcgtgcgcggaaggagccaaggtgaagttgtagcagtgtgtcagaagaggtgcgtggcaccatgctgtcccccgaggcggagcgggtgctgcggtacctggtcgaagtagaggagttg")
   (make-dna-sequence-from-string "gacttgtggaacctacttcctgaaaataaccttctgtcctccgagctctccgcacccgtggatgacctgctcccgtacacagatgttgccacctggctggatgaatgtccgaatgaagcg")
   :match 5
   :mismatch -4
   :gap -10
   :gap-extend -1
   :terminal-gap -10)))

(ok
 (alignment-results
  (global-align-na 
   (make-dna-sequence-from-string "gcgcgtgcgcggaaggagccaaggtgaagttgtagcagtgtgtcagaagaggtgcgtggcaccatgctgtcccccgaggcggagcgggtgctgcggtacctggtcgaagtagaggagttg")
   (make-dna-sequence-from-string "gacttgtggaacctacttcctgaaaataaccttctgtcctccgagctctccgcacccgtggatgacctgctcccgtacacagatgttgccacctggctggatgaatgtccgaatgaagcg")
   :match 2
   :mismatch -1
   :gap -2)))

(ok
 (alignment-results
  (global-align-na 
   (make-dna-sequence-from-string "aattc")
   (make-dna-sequence-from-string "gat")
   :match 2
   :mismatch -1
   :gap -2)))


(ok
 (global-align-na 
  (make-dna-sequence-from-string "a")
  (make-dna-sequence-from-string "g")
  :match 2
  :mismatch -1
  :gap -2))

(ok
 (let* ((a "AATTC") (b "GAT"))
   (let ((z (global-align-na a b :match 2 :mismatch -1 :gap -2)))
     (align::emit (align::alignment-dp-matrix z)
                  (align::alignment-dp-traceback z)
                  (length a)
                  (length b)
                  a b))))

(defparameter *q*
  (global-align-na-affine-gaps
   "ATTTTAAGCGCATACCGC" "TCGCAAATATAC"
   :terminal-gap 0
   :match 4
   :mismatch -4
   :transition -2
   :gap -8
   :gap-extend -1))

(ok
 (alignment-results
  (global-align-na-affine-gaps
   (make-dna-sequence-from-string "gcgcgtgcgcggaaggagccaaggtgaagttgtagcagtgtgtcagaagaggtgcgtggcaccatgctgtcccccgaggcggagcgggtgctgcggtacctggtcgaagtagaggagttg")
   (make-dna-sequence-from-string "gacttgtggaacctacttcctgaaaataaccttctgtcctccgagctctccgcacccgtggatgacctgctcccgtacacagatgttgccacctggctggatgaatgtccgaatgaagcg")
   :terminal-gap -10
   :terminal-gap-extend -10
   :match 5
   :mismatch -4
   :transition -4
   :gap -10
   :gap-extend -1)))

(finalize)
