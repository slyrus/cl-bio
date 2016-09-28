(cl:defpackage cl-bio-align-test-asd
  (:use :cl :asdf))

(in-package :cl-bio-align-test-asd)

(asdf:defsystem #:cl-bio-align-test
  :name "cl-bio-align-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.7"
  :licence "BSD"
  :description "Tests for cl-bio-align"
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-bio-align :prove)
  :components
  ((:module :test
	    :components
	    ((:test-file "cl-bio-align-test"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)
                    (asdf:clear-system c)))

