;;; Entrez
;;; Routines for working with NCBI's entrez database
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(in-package :entrez)

(defparameter *efetch-url-base* "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi")
(defparameter *esearch-url-base* "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi")

(defparameter *entrez-dtd-list*
  '(
    ;; Pubmed

    (:public-id "-//NLM//DTD PubMedArticle, 1st January 2010//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/corehtml/query/DTD/pubmed_100101.dtd"
     :local-file "dtd/DTD_PubmedArticle.dtd")
    
    (:public-id ("-//NLM//DTD Medline, 01 Jan 2009//EN"
                 "-//NLM//DTD Medline//EN"
                 "-//NLM//DTD NLM Medline//EN")
     :system-url "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/nlmmedline_090101.dtd"
     :local-file "dtd/DTD_Medline.dtd")
    
    (:public-id ("-//NLM//DTD Medline, 01 Jan 2010//EN"
                  "-//NLM//DTD MedlineCitation//EN")
     :system-url "http://www.ncbi.nlm.nih.gov/corehtml/query/DTD/nlmmedlinecitationset_100101.dtd" 
     :local-file "dtd/DTD_MedlineCitation.dtd")

    (:public-id ("-//NLM//DTD SharedCatCit, 1st January 2009//EN"
                 "-//NLM//DTD SharedCatCit//EN")
     :system-url "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/nlmsharedcatcit_090101.dtd"
     :local-file "dtd/DTD_SharedCatCit.dtd")

    (:public-id ("-//NLM//DTD Common, 1st January 2009//EN"
                 "-//NLM//DTD Common//EN")
     :system-url "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/nlmcommon_090101.dtd"
     :local-file "dtd/DTD_Common.dtd")
    

    ;; GBSeq
    (:public-id "-//NCBI//NCBI GBSeq/EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_GBSeq.dtd"
     :local-file "dtd/NCBI_GBSeq.dtd")
    (:public-id "-//NCBI//NCBI Entity Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Entity.mod.dtd"
     :local-file "dtd/NCBI_Entity.mod.dtd")
    (:public-id "-//NCBI//NCBI GBSeq Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_GBSeq.mod.dtd"
     :local-file "dtd/NCBI_GBSeq.mod.dtd")
    (:public-id "-//NLM//DTD eSearchResult, 11 May 2002//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/entrez/query/DTD/eSearch_020511.dtd"
     :local-file "dtd/DTD_eSearchResult.dtd")
    (:public-id "-//NLM//DTD esearch 20060628//EN"
     :system-url "https://eutils.ncbi.nlm.nih.gov/eutils/dtd/20060628/esearch.dtd"
     :local-file "dtd/DTD_eSearch.dtd")
    (:public-id "-//NLM//DTD NCBI-Entrezgene, 21st January 2005//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Entrezgene.dtd"
     :local-file "dtd/NCBI_Entrezgene.dtd")
    ;;; Entrezgene entities
    (:public-id "-//NCBI//NCBI Entity Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Entity.mod.dtd"
     :local-file "dtd/NCBI_Entity.mod.dtd")
    (:public-id "-//NCBI//EMBL General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/EMBL_General.mod.dtd"
     :local-file "dtd/EMBL_General.mod.dtd")
    (:public-id "-//NCBI//GenBank General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/GenBank_General.mod.dtd"
     :local-file "dtd/GenBank_General.mod.dtd")
    (:public-id "-//NCBI//NCBI Biblio Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Biblio.mod.dtd"
     :local-file "dtd/NCBI_Biblio.mod.dtd")
    (:public-id "-//NCBI//NCBI BioSource Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_BioSource.mod.dtd"
     :local-file "dtd/NCBI_BioSource.mod.dtd")
    (:public-id "-//NCBI//NCBI Entrezgene Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Entrezgene.mod.dtd"
     :local-file "dtd/NCBI_Entrezgene.mod.dtd")
    (:public-id "-//NCBI//NCBI Gene Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Gene.mod.dtd"
     :local-file "dtd/NCBI_Gene.mod.dtd")
    (:public-id "-//NCBI//NCBI General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_General.mod.dtd"
     :local-file "dtd/NCBI_General.mod.dtd")
    (:public-id "-//NCBI//NCBI Medline Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Medline.mod.dtd"
     :local-file "dtd/NCBI_Medline.mod.dtd")
    (:public-id "-//NCBI//NCBI Organism Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Organism.mod.dtd"
     :local-file "dtd/NCBI_Organism.mod.dtd")
    (:public-id "-//NCBI//NCBI Protein Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Protein.mod.dtd"
     :local-file "dtd/NCBI_Protein.mod.dtd")
    (:public-id "-//NCBI//NCBI Pub Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Pub.mod.dtd"
     :local-file "dtd/NCBI_Pub.mod.dtd")
    (:public-id "-//NCBI//NCBI RNA Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_RNA.mod.dtd"
     :local-file "dtd/NCBI_RNA.mod.dtd")
    (:public-id "-//NCBI//NCBI Rsite Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Rsite.mod.dtd"
     :local-file "dtd/NCBI_Rsite.mod.dtd")
    (:public-id "-//NCBI//NCBI SeqTable Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_SeqTable.mod.dtd"
     :local-file "dtd/NCBI_SeqTable.mod.dtd")
    (:public-id "-//NCBI//NCBI Seqalign Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Seqalign.mod.dtd"
     :local-file "dtd/NCBI_Seqalign.mod.dtd")
    (:public-id "-//NCBI//NCBI Seqfeat Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Seqfeat.mod.dtd"
     :local-file "dtd/NCBI_Seqfeat.mod.dtd")
    (:public-id "-//NCBI//NCBI Seqloc Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Seqloc.mod.dtd"
     :local-file "dtd/NCBI_Seqloc.mod.dtd")
    (:public-id "-//NCBI//NCBI Seqres Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Seqres.mod.dtd"
     :local-file "dtd/NCBI_Seqres.mod.dtd")
    (:public-id "-//NCBI//NCBI Sequence Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_Sequence.mod.dtd"
     :local-file "dtd/NCBI_Sequence.mod.dtd")
    (:public-id "-//NCBI//NCBI TxInit Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/NCBI_TxInit.mod.dtd"
     :local-file "dtd/NCBI_TxInit.mod.dtd")
    (:public-id "-//NCBI//PDB General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/PDB_General.mod.dtd"
     :local-file "dtd/PDB_General.mod.dtd")
    (:public-id "-//NCBI//PIR General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/PIR_General.mod.dtd"
     :local-file "dtd/PIR_General.mod.dtd")
    (:public-id "-//NCBI//PRF General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/PRF_General.mod.dtd"
     :local-file "dtd/PRF_General.mod.dtd")
    (:public-id "-//NCBI//SP General Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/dtd/SP_General.mod.dtd"
     :local-file "dtd/SP_General.mod.dtd")
    (:public-id "-//NCBI//NCBI Variation Module//EN"
     :system-url "http://www.ncbi.nlm.nih.gov/data_specs/dtd/NCBI_Variation.mod.dtd"
     :local-file "dtd/NCBI_Variation.mod.dtd")))

(defvar *local-dtd-hash-table* (make-hash-table :test 'equal))

(map nil (lambda (x)
           (let ((public-id-atom-or-list (getf x :public-id))
                 (local-file (getf x :local-file)))
             (map nil (lambda (id)
                        (setf (gethash id *local-dtd-hash-table*)
                              local-file))
                  (if (listp public-id-atom-or-list)
                      public-id-atom-or-list
                      (list public-id-atom-or-list)))))
     *entrez-dtd-list*)

(defun entrez-directory ()
  (asdf:component-pathname
   (reduce #'asdf:find-component
           (list nil "cl-bio-entrez" "entrez"))))

;;; call this to cache the DTDs
(defun http-get-entrez-dtds ()
  (mapcar (lambda (x)
            (let ((url (getf x :system-url))
                  (file (merge-pathnames
                            (getf x :local-file)
                            (entrez-directory))))
              (http-get-file url file)
              (cons url file)))
          *entrez-dtd-list*))

(defun dtd-resolver (pubid sysid)
  (let ((local-dtd (gethash pubid *local-dtd-hash-table*)))
    (let ((local-dtd-pathname (when local-dtd
                                (merge-pathnames local-dtd
                                                 (entrez-directory)))))
      (if (and pubid local-dtd (probe-file local-dtd-pathname))
          (open local-dtd-pathname :element-type :default)
          (when (eq (puri:uri-scheme sysid) :http)
            (drakma:http-request sysid :want-stream t))))))

;;; fetching xml sequences from NCBI
(defun get-entrez-stream (id
                          &key
                          (database "nucleotide")
                          (retmode "xml")
                          copy-to-file
                          if-exists
                          if-does-not-exist)
  (let ((query-params `(("db" ,database)
                        ("id" ,id)
                        ("retmode" ,retmode))))
    (let* ((entrez-url (puri:merge-uris (make-query-uri query-params)
                                        *efetch-url-base*))
           (rendered-url (puri:render-uri entrez-url nil)))
      (let ((drakma:*body-format-function* (constantly :latin1)))
        (if copy-to-file
            (progn
              (apply #'http-get-file rendered-url copy-to-file
                     (append
                      (when if-exists `(:if-exists ,if-exists))
                      (when if-does-not-exist `(:if-does-not-exist ,if-does-not-exist))))
              (open copy-to-file :direction :input :element-type :default))
            (drakma:http-request rendered-url :want-stream t))))))

(defun parse-entrez-xml-stream (stream
                                &key
                                (builder (cxml-stp:make-builder)))
  (cxml:parse-stream stream
                     (cxml:make-whitespace-normalizer builder)
                     :entity-resolver #'dtd-resolver
                     :validate t))

(defun entrez-fetch (id
                     &key
                     (database "nucleotide")
                     (retmode "xml")
                     copy-to-file
                     builder)
  (let ((stream (apply #'get-entrez-stream
                       id
                       (append
                        (when database `(:database ,database))
                        (when retmode `(:retmode ,retmode))
                        (when copy-to-file `(:copy-to-file ,copy-to-file))))))
    (apply #'parse-entrez-xml-stream stream
           (when builder `(:builder ,builder)))))

(defun parse-entrez-xml-file (pathname &key builder)
  (with-open-file (stream pathname :element-type :default)
    (apply #'parse-entrez-xml-stream
           stream
           (when builder
             `(:builder ,builder)))))

