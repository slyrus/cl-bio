
(defpackage #:cl-bio-test-system (:use #:asdf #:cl))
(in-package #:cl-bio-test-system)

(defsystem #:cl-bio-test
  :name "cl-bio-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.6"
  :licence "BSD"
  :description "Tests for cl-bio"
  :depends-on (cl-bio)
  :components
  ((:module :test
	    :components
	    ((:cl-source-file "defpackage")
	     (:cl-source-file "cl-bio-test" :depends-on ("defpackage"))))
   (:module :data
	    :components
	    ((:static-file "dpp-fasta" :pathname #p"dpp.fasta")))))

