
(defpackage #:cl-bio-system (:use #:asdf #:cl))
(in-package #:cl-bio-system)

(defsystem #:cl-bio
  :name "cl-bio"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :description "A library for representing various biological objects"
  :depends-on (ch-asdf alexandria cl-ppcre flexichain rucksack)
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module :src
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "cl-bio" :depends-on ("defpackage"))
             (:cl-source-file "bio-object" :depends-on ("defpackage"))
             (:cl-source-file "rucksack" :depends-on ("defpackage" "bio-object"))
             (:cl-source-file "gene" :depends-on ("defpackage" "bio-object"))
             (:cl-source-file "range" :depends-on ("defpackage"))
             (:cl-source-file "utilities" :depends-on ("defpackage"))
             (:cl-source-file "encoding" :depends-on ("defpackage" "utilities"))
             (:cl-source-file "descriptor" :depends-on ("defpackage" "utilities"))
             (:cl-source-file "identifier" :depends-on ("defpackage" "utilities" "descriptor"))
             (:cl-source-file "bio-sequence" :depends-on ("defpackage"
                                                          "encoding"
                                                          "range"
                                                          "descriptor"
                                                          "bio-object"))
             (:cl-source-file "taxon" :depends-on ("defpackage"
                                                   "bio-object"
                                                   "rucksack"))
             (:cl-source-file "dictionary" :depends-on ("defpackage" "bio-object"))
             (:module :io
                      :components
                      ((:cl-source-file "utilities")
                       (:cl-source-file "fasta" :depends-on ("utilities")))
                      :depends-on ("defpackage" "encoding" "range" "bio-sequence"))))
   (:module :align
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "align" :depends-on ("defpackage"))
             (:module :matrix
                      :components
                      ((:static-file "blosum62" :pathname #p"blosum62.bla"))))
            :depends-on (:src))
   (:module :rucksack)
   (:static-file "bootstrap" :pathname #p"bootstrap.lisp")
   (:static-file "COPYRIGHT")
   (:static-file "README")
   (:static-file "make-dist" :pathname #.(make-pathname :name "make-dist" :type "sh"))))

