
(asdf:defsystem #:cl-bio-rucksack
  :name "cl-bio-rucksack"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.2.7"
  :licence "BSD"
  :depends-on (cl-bio rucksack)
  :components
  ((:module :rucksack
            :components
            ((:module :rucksack-data)
             (:cl-source-file "defpackage")
             (:cl-source-file "rucksack" :depends-on ("defpackage"))))))
