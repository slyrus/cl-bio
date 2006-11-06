
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
  :depends-on (flexichain split-sequence)
  :components
  ((:static-file "version" :pathname #p"version.lisp-expr")
   (:module :src
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "cl-bio" :depends-on ("defpackage"))
             (:cl-source-file "range" :depends-on ("defpackage"))
             (:cl-source-file "utilities" :depends-on ("defpackage"))
             (:cl-source-file "encoding" :depends-on ("defpackage" "utilities"))
             (:cl-source-file "descriptor" :depends-on ("defpackage" "utilities"))
             (:cl-source-file "identifier" :depends-on ("defpackage" "utilities" "descriptor"))
             (:cl-source-file "bio-sequence" :depends-on ("defpackage" "encoding" "range" "descriptor"))
             (:module :io
                      :components
                      ((:cl-source-file "fasta"))
                      :depends-on ("defpackage" "encoding" "range" "bio-sequence"))))
   (:static-file "bootstrap" :pathname #p"bootstrap.cl")
   (:static-file "COPYRIGHT")
   (:static-file "README")
   (:static-file "make-dist" :pathname #.(make-pathname :name "make-dist" :type "sh"))))

