

(in-package :cl-bio)

(defparameter *drosophila-2-annot-csv-file*
  "/Users/sly/projects/wing/expression/drodev/affy-annot/Drosophila_2_annot.csv")

(with-open-file (s *drosophila-2-annot-csv-file*)
  (print (fare-csv::read-csv-line s))
  (print (fare-csv::read-csv-line s)))
