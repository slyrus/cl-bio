
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:cl-bio-doc-system (:use #:cl #:asdf #:ch-asdf #:smarkup))
(in-package #:cl-bio-doc-system)

(defsystem :cl-bio-doc
  :name "cl-bio-doc"
  :author "Cyrus Harmon" 
  :version "0.2.7"
  :depends-on (ch-asdf puri smarkup cl-bio cl-bio-taxonomy cl-bio-entrez)
  :components
  ((:module
    "doc"
    :components
    ((:smarkup-object-from-file :cl-bio-doc-sexp
                                :pathname #p"cl-bio-doc.sexp")

     (:filtered-object :cl-bio-doc-filtered-sexp
                       :filters (:lisp :smarkup-metadata :html-metadata)
                       :depends-on (:cl-bio-doc-sexp)
                       :input-object :cl-bio-doc-sexp)

     (:object-xhtml-file :cl-bio-doc-xhtml
                         :pathname #p"cl-bio-doc.xhtml"
                         :depends-on (:cl-bio-doc-filtered-sexp)
                         :input-object :cl-bio-doc-filtered-sexp)

     (:object-cl-pdf-file :cl-bio-doc-pdf
                          :pathname #p"cl-bio-doc.pdf"
                          :depends-on (:cl-bio-doc-filtered-sexp)
                          :input-object :cl-bio-doc-filtered-sexp)))))

