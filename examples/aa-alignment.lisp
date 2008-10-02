;;;
;;; The following example loads the protein sequences for ER-alpha (ESR1)
;;; and ER-beta (ESR2) and performs a local alignment between the two
;;; sequences.
;;;

(asdf:oos 'asdf:load-op :cl-bio)
(asdf:oos 'asdf:load-op :cl-bio-entrez)
(asdf:oos 'asdf:load-op :cl-bio-align)

(defpackage #:alignment-example
  (:use :cl :bio :bio-entrez :bio-align))

(in-package #:alignment-example)

(defparameter *esr1-protein-search*
  (bio:lookup "estrogen receptor alpha isoform 1"
              *entrez-dictionary*
              :database "protein"))

(defparameter *esr1-protein*
  (car (bio:members
        (bio:fetch (bio:id (car (bio:members *esr1-protein-search*)))
                   *entrez-dictionary*
                   :database "protein"))))

(defparameter *esr2-protein-search*
  (bio:lookup "estrogen receptor beta human"
              *entrez-dictionary*
              :database "protein"))

(defparameter *esr2-protein*
  (car (bio:members
        (bio:fetch (bio:id (car (bio:members *esr2-protein-search*)))
                   *entrez-dictionary*
                   :database "protein"))))

(bio:residues-string *esr1-protein*)
(bio:residues-string *esr2-protein*)

(defparameter *global-align-esr1-esr2-aa*
  (bio-align::global-align-aa *esr1-protein* *esr2-protein*))
(bio-align::print-alignment *global-align-esr1-esr2-aa*)

(defparameter *local-align-esr1-esr2-aa*
  (bio-align::local-align-aa *esr1-protein* *esr2-protein*))
(bio-align::print-alignment *local-align-esr1-esr2-aa*)

