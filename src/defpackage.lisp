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


(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'bio)
    (defpackage #:bio)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:bio-io (:use #:cl #:bio)
              (:export
               .
               #1=(
                   #:read-fast-sequences
                   #:read-fasta-file)))

  (defpackage #:bio (:use #:cl #:bio-io)

              (:export

               ;; objects
               #:bio-object
               #:bio-object-set
               #:members
               
               ;; ranges
               #:range
               #:ds-range
             
               #:range-equal
               #:range-contains
               #:range-start
               #:range-end
               #:range-min
               #:range-max
             
               #:+plus-strand+
               #:+unknown-strand+
               #:+minus-strand+
               #:+both-strands+

               ;; descritptors
               #:descriptor
               #:get-descriptors
               #:add-descriptor
               #:remove-descriptor
               
               ;; identifiers
               #:identifier
               #:identifiers
               #:id
               #:type
               #:version
               #:authority

               #:ncbi-gi
               #:get-ncbi-gis

               #:refseq-id
               #:get-refseq-ids
               #:genbank-accession
               #:affymetrix-probe-set-id
               #:flybase-identifier
               #:flybase-gene-identifier
               
               ;; bio-sequences
               #:bio-sequence
               #:residue
               #:residues-string
               #:residues-string-range
               #:seq-length

               ;; sequences with residues
               #:sequence-with-residues
               #:seq-reverse

               ;; simple sequences
               #:simple-sequence

               ;; adjustable sequences
               #:adjustable-sequence
               #:insert-residue
               #:insert-residues
               #:append-residue
               #:append-residues
               #:delete-residue

               ;; nucleic acid sequences
               #:na-sequence
               #:na-sequence-with-residues

               ;; DNA sequences
               #:dna-sequence
               #:simple-dna-sequence
               #:adjustable-dna-sequence
               #:dna-sequence-with-residues
               #:reverse-complement
               #:make-simple-dna-sequence
               #:make-adjustable-dna-sequence
               #:make-dna-sequence-from-string
               #:make-random-dna-sequence

               ;; RNA sequences
               #:rna-sequence
               #:simple-rna-sequence
               #:adjustable-rna-sequence
               #:rna-sequence-with-residues
               #:make-simple-rna-sequence
               #:make-adjustable-rna-sequence
               #:make-rna-sequence-from-string
               #:make-random-rna-sequence

               ;; amino acid sequences
               #:aa-sequence
               #:simple-aa-sequence
               #:adjustable-aa-sequence
               #:aa-sequence-with-residues
               #:make-simple-aa-sequence
               #:make-adjustable-aa-sequence
               #:make-aa-sequence-from-string
               #:make-random-aa-sequence

               ;; genes
               #:gene
               #:gene-set
               #:genes
               #:gene-type
               #:gene-source
               
               #:annotation
               #:annotations
               #:exon

               #:simple-pairwise-alignment

               ;; utilities
               #:split-string-into-lines-list
               #:split-string-into-lines-string

               ;; dictionary
               #:lookup
               #:fetch
               
               . #1#)))

(defpackage #:bio-user (:use #:cl #:bio))

