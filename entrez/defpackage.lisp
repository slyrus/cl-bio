
(in-package #:cl-user)

(defpackage #:entrez
  (:use #:cl)
  (:export #:element-type
           #:children
           #:attributes
           #:attribute

           #:entrez-fetch
           #:entrez-search

           #:gb-set-get-gb-seqs
           #:gb-seg-get-sequence
           #:gb-seq-sequence-get-residues))

