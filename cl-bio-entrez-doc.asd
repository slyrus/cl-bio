
(asdf:operate 'asdf:load-op :ch-asdf)
(asdf:operate 'asdf:load-op :smarkup)

(defpackage #:cl-bio-entrez-doc-system (:use #:cl #:asdf #:ch-asdf #:smarkup))
(in-package #:cl-bio-entrez-doc-system)

#.(smarkup::enable-quote-reader-macro)

(defsystem :cl-bio-entrez-doc
  :name "cl-bio-entrez-doc"
  :author "Cyrus Harmon" 
  :version "0.2.7"
  :depends-on (ch-asdf ch-bib ch-util puri smarkup cl-graph)
  :components
  ((:module "entrez"
            :components
            ((:module
              "doc"
              :components
              ((:object-from-file :cl-bio-entrez-doc-sexp
                                  :pathname #p"cl-bio-entrez-doc.sexp")
               
               (:filtered-object :cl-bio-entrez-doc-filtered-sexp
                                 :filters (:lisp :smarkup-metadata :html-metadata)
                                 :depends-on (:cl-bio-entrez-doc-sexp)
                                 :input-object :cl-bio-entrez-doc-sexp)
               
               (:filtered-object :cl-bio-entrez-doc-html-filtered-sexp
                                 :filters (:html-metadata)
                                 :depends-on (:cl-bio-entrez-doc-filtered-sexp)
                                 :input-object :cl-bio-entrez-doc-filtered-sexp)
               
               (:object-xhtml-file :cl-bio-entrez-doc-xhtml
                                   :pathname #p"cl-bio-entrez-doc.xhtml"
                                   :depends-on (:cl-bio-entrez-doc-filtered-sexp)
                                   :input-object :cl-bio-entrez-doc-filtered-sexp)
               
               (:object-cl-pdf-file :cl-bio-entrez-doc-pdf
                                    :pathname #p"cl-bio-entrez-doc.pdf"
                                    :depends-on (:cl-bio-entrez-doc-filtered-sexp)
                                    :input-object :cl-bio-entrez-doc-filtered-sexp)))))))

