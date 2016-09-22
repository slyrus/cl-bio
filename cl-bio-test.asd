
(cl:defpackage cl-bio-test-asd
  (:use :cl :asdf))

(in-package :cl-bio-test-asd)

(asdf:defsystem #:cl-bio-test
  :name "cl-bio-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.7"
  :licence "BSD"
  :description "Tests for cl-bio"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-bio :prove)
  :components
  ((:module :test
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "cl-bio-test" :depends-on ("defpackage"))))
   (:module :data
	    :components
	    ((:static-file "dpp-fasta" :pathname #p"dpp.fasta"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
