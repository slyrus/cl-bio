;;; Entrez searching routines
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

(defun get-entrez-search-stream (term
                                 &key
                                 (database "nucleotide")
                                 (retmode "xml")
                                 (retstart 0)
                                 (retmax 20)
                                 copy-to-file
                                 if-exists
                                 if-does-not-exist)
  (let ((query-params `(("db" ,database)
                        ("term" ,term)
                        ("retmode" ,retmode)
                        ("retstart" ,retstart)
                        ("retmax" ,retmax))))
    (let* ((entrez-url (puri:merge-uris (make-query-uri query-params)
                                        *esearch-url-base*))
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

(defun entrez-search (term
                      &key
                      (database "nucleotide")
                      (retstart 0)
                      (retmax 20)
                      builder
                      copy-to-file
                      if-exists
                      if-does-not-exist)
  (let ((stream (apply #'get-entrez-search-stream
                       term
                       (append
                        (when database `(:database ,database))
                        (when retstart `(:retstart ,retstart))
                        (when retmax `(:retmax ,retmax))
                        (when copy-to-file `(:copy-to-file ,copy-to-file))
                        (when if-exists `(:if-exists ,if-exists))
                        (when if-does-not-exist `(:if-does-not-exist ,if-does-not-exist))))))
    (apply #'parse-entrez-xml-stream stream
           (when builder `(:builder ,builder)))))

(defun get-search-result-ids (search-result &key (id-class 'bio:ncbi-gi))
  (xpath:with-namespaces ()
    (let ((idset (make-instance 'bio::identifier-set)))
      (mapcar
       (lambda (node)
         (let ((id (xpath-protocol:node-text node)))
           (push (make-instance id-class :id (or (parse-integer id :junk-allowed t)
                                                 id))
                 (bio:members idset))))
       (xpath:all-nodes
        (xpath:evaluate
         "eSearchResult/IdList/Id/text()" search-result)))
      (setf (bio:members idset)
            (nreverse (bio:members idset)))
      idset)))
