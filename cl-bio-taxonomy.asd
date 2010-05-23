
(asdf:defsystem #:cl-bio-taxonomy
  :name "cl-bio-taxonomy"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.6"
  :licence "BSD"
  :depends-on (cl-bio cl-bio-rucksack)
  :components
  ((:module :taxonomy
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "taxon" :depends-on ("defpackage"))))))

