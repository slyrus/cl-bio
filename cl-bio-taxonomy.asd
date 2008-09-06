
(asdf:oos 'asdf:load-op :ch-asdf)

(let ((asdf:*central-registry* asdf:*central-registry*))
  (pushnew (ch-asdf:asdf-lookup-path "asdf:/cl-bio") asdf:*central-registry*)
  (asdf:oos 'asdf:load-op :cl-bio)
  (asdf:oos 'asdf:load-op :cl-bio-rucksack))

(asdf:defsystem #:cl-bio-taxonomy
  :name "cl-bio-taxonomy"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :licence "BSD"
  :depends-on (cl-bio cl-bio-rucksack)
  :components
  ((:module :taxonomy
            :components
            ((:cl-source-file "defpackage")
             (:cl-source-file "taxon" :depends-on ("defpackage"))))))

