
(asdf:defsystem #:cl-bio-entrez
  :name "cl-bio-entrez"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :version "0.2.6"
  :description "A library for accessing the NCBI entrez web services"
  :depends-on (cl-bio puri drakma cxml cxml-stp xpath)
  :components
  ((:module :entrez
            :components 
            ((:cl-source-file "defpackage")
             (:cl-source-file "classes" :depends-on ("defpackage"))
             (:cl-source-file "utilities" :depends-on ("defpackage"))
             (:cl-source-file "entrez" :depends-on ("defpackage" "classes" "utilities"))
             (:cl-source-file "search"
                              :depends-on ("defpackage" "classes" "utilities" "entrez"))
             (:cl-source-file "entrez-gene"
                              :depends-on ("defpackage" "classes" "utilities" "entrez"))
             (:cl-source-file "gbseq" :depends-on ("defpackage" "classes" "utilities" "entrez"))
             (:cl-source-file "pubmed" :depends-on ("defpackage" "classes" "utilities" "entrez"))
             (:cl-source-file "entrez-dictionary"
                              :depends-on ("defpackage" "classes" "utilities" "entrez" "search"))
             (:static-file "README")))
   (:module "cache"
            :components
            ((:module "data")
             (:module "search")))))

