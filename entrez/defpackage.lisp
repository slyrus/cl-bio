
(in-package #:cl-user)

(defpackage #:bio-entrez
  (:nicknames #:entrez)
  (:use #:cl)
  (:export #:*entrez-dictionary*
           #:*entrez-xml-dictionary*

           #:entrez-fetch
           #:entrez-search

           #:gb-set-get-gb-seqs
           #:gb-seg-get-sequence
           #:gb-seq-sequence-get-residues
           
           #:generif
           #:generif-text))

(defpackage #:entrez-user (:use #:cl #:bio #:entrez))
