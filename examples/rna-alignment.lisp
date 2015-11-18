;;;
;;; The following example loads the mRNA sequences for ER-alpha (ESR1)
;;; and ER-beta (ESR2) and performs global and local alignments
;;; between the two sequences.
;;;

(asdf:oos 'asdf:load-op :cl-bio)
(asdf:oos 'asdf:load-op :cl-bio-entrez)
(asdf:oos 'asdf:load-op :cl-bio-align)

(defpackage #:alignment-example
  (:use :cl :bio :bio-entrez :bio-align))

(in-package #:alignment-example)

(defparameter *esr1-gene-search*
  (bio:lookup "ESR1[GENE] human[Organism]" entrez:*entrez-dictionary* :database "gene"))

;;; get the id first hit from the search set returned above, and load
;;; the corresponding gene from the entrez "gene" database.
(defparameter *esr1-gene*
  (car (bio:members
        (bio:fetch (bio:id (first (bio:members *esr1-gene-search*)))
                   entrez:*entrez-dictionary*
                   :database "gene"))))

(defparameter *esr1-nucleotide*
  (car (bio:members
        (bio:fetch
         (bio:id
          (car (bio:get-genbank-accessions
                (car (bio:gene-products *esr1-gene*)))))
         entrez:*entrez-dictionary*))))

(defparameter *esr2-gene-search*
  (bio:lookup "ESR2[GENE] human[Organism]" entrez:*entrez-dictionary* :database "gene"))

;;; get the id first hit from the search set returned above, and load
;;; the corresponding gene from the entrez "gene" database.
(defparameter *esr2-gene*
  (car (bio:members
        (bio:fetch (bio:id (first (bio:members *esr2-gene-search*)))
                   entrez:*entrez-dictionary*
                   :database "gene"))))

(defparameter *esr2-nucleotide*
  (car (bio:members
        (bio:fetch
         (bio:id
          (car (bio:get-genbank-accessions
                (car (bio:gene-products *esr2-gene*)))))
         entrez:*entrez-dictionary*))))

(bio:residues-string *esr1-nucleotide*)
(bio:residues-string *esr2-nucleotide*)

(defparameter *global-align-esr1-esr2-rna*
  (align:global-align-na *esr1-nucleotide* *esr2-nucleotide*))
(align:alignment-results *global-align-esr1-esr2-rna*)

(defparameter *global-align-affine-esr1-esr2-rna*
  (align:global-align-na-affine-gaps *esr1-nucleotide* *esr2-nucleotide*))
(align:alignment-results *global-align-affine-esr1-esr2-rna*)

(defparameter *local-align-esr1-esr2-rna*
  (align:local-align-na *esr1-nucleotide* *esr2-nucleotide*))
(align:alignment-results *local-align-esr1-esr2-rna*)

