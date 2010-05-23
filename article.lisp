;;; Article
;;; Classes, generic functions, methods and functions for working
;;; with articles
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

(in-package :bio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Article
;;;
;;; Note that the article and article-set-classes were originally
;;; "borrowed" from Kevin Rosenberg's cl-pubmed package.

;;; author protocol class
(defclass author (bio-object)
  ((last-name :type string :accessor author-last-name :initarg :last-name)
   (forenames :type (or nil string) :accessor author-forenames :initarg :forenames :initform nil)
   (initials :type (or nil string) :accessor author-initials :initarg :initials :initform nil)))

;;; article protocol class
(defclass article (bio-object)
  ((pmid :type integer :accessor article-pmid :initarg :pmid)
   (title :type string :accessor article-title :initarg :title)
   (authors :type list :accessor article-authors :initarg :authors :initform nil)
   (journal :type string :accessor article-journal :initarg :journal)
   (short-journal :type string :accessor article-short-journal :initarg :short-journal)
   (date :type list :accessor article-date :initarg :date)
   (volume :type (or string null) :accessor article-volume :initarg :volume)
   (issue :type (or string null) :accessor article-issue :initarg :issue)
   (pages :type (or string null) :accessor article-pages :initarg :pages)
   (abstract :type (or string null) :accessor article-abstract :initarg :abstract)
   (mesh-headings :type list :accessor article-mesh-headings :initarg :mesh-headings :initform nil)
   (doi :type (or string null) :accessor article-doi :initarg :doi)
   (affiliation :type (or string null) :accessor article-affiliation :initarg :affiliation)))

;;; article-set protocol class
(defclass article-set (bio-set)
  ((members :initarg :articles :accessor article-set-articles :initform nil)
   (query :type (or string null) :initarg :query :accessor article-set-query)
   (total :type fixnum :initarg :total :accessor article-set-total)
   (count :type fixnum :initarg :count :accessor article-set-count)
   (start :type fixnum :initarg :start :accessor article-set-start)))

