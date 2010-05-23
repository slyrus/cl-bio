;;; pubmed.lisp
;;; Routines for working with the pubmed section of NCBI's entrez database
;;;
;;; Copyright (c) 2009 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defun parse-author-node (node)
  (let ((last-name (xpath:string-value
                    (xpath:evaluate "LastName" node)))
        (forenames (or (string-if
                        (format nil "~A~@[ ~A~]"
                                (xpath:string-value (xpath:evaluate "FirstName" node))
                                (string-value-if (xpath:evaluate "MiddleName" node))))
                       (string-value-if
                        (xpath:evaluate "ForeName" node))))
        (initials (string-value-if
                   (xpath:evaluate "Initials" node))))
    (apply #'make-instance 'bio:author
           :last-name last-name
           (append
            (when forenames `(:forenames ,forenames))
            (when initials `(:initials ,initials))))))

(defun fix-pages (medline-pages)
  (let ((pages-split (ppcre:split "-" medline-pages)))
    (if (and pages-split 
             (> (length pages-split) 1)
             (> (length (car pages-split))
                (length (cadr pages-split))))
        (destructuring-bind (start partial-end)
            pages-split
          (let ((end (copy-seq start)))
            (let ((pos (- (length start) (length partial-end))))
              (format nil "~A-~A" start (replace end partial-end :start1 pos)))))
        medline-pages)))

(defun parse-article (node)
  (xpath:with-namespaces ()
    (let ((citation (xpath:first-node (xpath:evaluate "MedlineCitation" node)))
          (doi (string-value-if
                (xpath:first-node
                 (xpath:evaluate "PubmedData/ArticleIdList/ArticleId[attribute::IdType=\"doi\"]"
                                 node)))))
      (let ((pmid
             (xpath:number-value
              (xpath:evaluate "PMID" citation)))
            (article (xpath:first-node (xpath:evaluate "Article" citation)))
            (mesh-headings
             (xpath:map-node-set->list
              #'string-value-if
              (xpath:evaluate "MeshHeadingList/MeshHeading/DescriptorName" citation)))
            
            (short-journal-title
             (xpath:string-value (xpath:evaluate "MedlineJournalInfo/MedlineTA" citation))))
        (let ((article-title
               (xpath:string-value
                (xpath:evaluate "ArticleTitle" article)))
              (pages
               (string-value-if
                (xpath:evaluate "Pagination/MedlinePgn" article)))
              (authors
               (mapcar #'parse-author-node
                       (xpath:all-nodes (xpath:evaluate "AuthorList/Author" article))))
              (abstract (string-value-if
                         (xpath:evaluate "Abstract/AbstractText" article)))
              (affiliation (string-value-if
                            (xpath:evaluate "Affiliation" article)))
              (journal-node
               (xpath:first-node (xpath:evaluate "Journal" article))))
          (let ((journal-title
                 (xpath:string-value
                  (xpath:evaluate "Title" journal-node)))
                (journal-issue-node (xpath:first-node (xpath:evaluate "JournalIssue" journal-node))))
            (let ((pub-date-node (xpath:first-node
                                  (xpath:evaluate "PubDate" journal-issue-node)))
                  (volume (string-value-if
                           (xpath:evaluate "Volume" journal-issue-node)))
                  (issue (string-value-if
                          (xpath:evaluate "Issue" journal-issue-node))))
              (let ((year (number-value-if
                           (xpath:evaluate "Year" pub-date-node)))
                    (month (string-value-if
                            (xpath:evaluate "Month" pub-date-node)))
                    (day (number-value-if
                          (xpath:evaluate "Day" pub-date-node))))
                (unless year
                  (let ((medline-date (string-value-if
                                       (xpath:evaluate "MedlineDate" pub-date-node))))
                    (if medline-date
                        (let ((maybe-year (parse-integer
                                           (ppcre:scan-to-strings "(\\d+)" medline-date))))
                          (when maybe-year
                            (setf year maybe-year))))))
                (let ((obj (make-instance 'bio:article
                                          :pmid pmid
                                          :title article-title
                                          :journal journal-title
                                          :short-journal short-journal-title
                                          :authors authors
                                          :abstract abstract
                                          :affiliation affiliation
                                          :pages (when pages (fix-pages pages))
                                          :volume volume
                                          :issue issue
                                          :date (list year month day)
                                          :mesh-headings mesh-headings
                                          :doi doi)))
                  obj)))))))))

(defun parse-pubmed-article-set (node)
  (xpath:with-namespaces ()
    (let ((set (make-instance 'bio:article-set)))
      (let ((articles
             (xpath:all-nodes
              (xpath:evaluate "PubmedArticleSet/PubmedArticle" node))))
        (setf (bio:article-set-articles set)
              (mapcar #'parse-article articles)))
      set)))

