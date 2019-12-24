
(asdf:defsystem #:cl-bio
  :name "cl-bio"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.7"
  :licence "BSD"
  :description "A library for representing various biological objects"
  :depends-on (cl-ppcre flexichain parse-number named-readtables alexandria prove)
  :serial t
  :components
  ((:cl-source-file "defpackage")
   (:cl-source-file "utilities")
   (:cl-source-file "cl-bio")
   (:cl-source-file "descriptor")
   (:cl-source-file "bio-object")
   (:cl-source-file "article")
   (:cl-source-file "gene")
   (:cl-source-file "range")
   (:cl-source-file "encoding")
   (:cl-source-file "identifier")
   (:cl-source-file "bio-sequence")
   (:cl-source-file "bio-sequence-util")
   (:cl-source-file "bio-sequence0")
   (:cl-source-file "bio-sequence1")
   (:cl-source-file "annotation")
   (:cl-source-file "dictionary")
   (:cl-source-file "reader-macros")
   (:module :io
            :components
            ((:cl-source-file "utilities")
             (:cl-source-file "fasta" :depends-on ("utilities"))
             (:cl-source-file "pdb" :depends-on ("utilities")))))
  :in-order-to ((test-op (test-op :cl-bio/test))))

(asdf:defsystem :cl-bio/test
  :serial t
  :depends-on (:cl-bio :prove :prove-asdf)
  :components ((:module :test
                        :serial t
                        :components ((:file "defpackage")
                                     (:file "cl-bio-test")
                                     (:file "sequence-test")))
               (:module :data
                        :components ((:static-file "dpp-fasta" :pathname #p"dpp.fasta"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))

