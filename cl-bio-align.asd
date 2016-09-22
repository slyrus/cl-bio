(cl:defpackage cl-bio-align-asd
  (:use :cl :asdf))

(in-package :cl-bio-align-asd)

(asdf:defsystem #:cl-bio-align
  :name "cl-bio-align"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.7"
  :licence "BSD"
  :depends-on (:cl-bio :alexandria)
  :serial t
  :components
  ((:module :align
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "align")
             (:module :matrix
                      :components
                      ((:static-file "blosum62" :pathname #p"blosum62.bla"))))))
  :in-order-to ((test-op (test-op cl-bio-align-test))))


