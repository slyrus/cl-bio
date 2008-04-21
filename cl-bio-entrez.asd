
(asdf:defsystem #:cl-bio-entrez
  :name "cl-bio-entrez"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "A library for accessing the NCBI entrez web services"
  :depends-on (puri drakma cxml cxml-stp xpath cl-bio ch-asdf)
  :components
  ((:module :entrez
            :components 
            ((:cl-source-file "defpackage")
             (:cl-source-file "classes" :depends-on ("defpackage"))
             (:cl-source-file "utilities" :depends-on ("defpackage"))
             (:cl-source-file "xml" :depends-on ("defpackage"))
             (:cl-source-file "entrez" :depends-on ("defpackage" "classes" "utilities" "xml"))
             (:cl-source-file "search"
                              :depends-on ("defpackage" "classes" "utilities" "xml" "entrez"))
             (:cl-source-file "entrez-gene"
                              :depends-on ("defpackage" "classes" "utilities" "xml" "entrez"))
             (:cl-source-file "gbseq" :depends-on ("defpackage" "classes" "utilities" "xml" "entrez"))
             (:cl-source-file "entrez-dictionary"
                              :depends-on ("defpackage" "classes" "utilities" "xml" "entrez" "search"))
             (:static-file "README")))
   (:module "cache"
            :components
            ((:module "data")
             (:module "search")))))
