
(in-package :bio-test)

(plan 10)

;; DNA Sequences

(is 
 (let ((test-seq (make-instance 'adjustable-dna-sequence :initial-contents "GAATTC")))
   (residues-string test-seq))
 "GAATTC")

(is-error
 (let ((test-seq (make-instance 'adjustable-dna-sequence :initial-contents "U")))
   (residues-string test-seq))
 sequence-encoding-error)

(is 
 (let ((test-seq (make-instance 'adjustable-dna-sequence)))
   (insert-residues test-seq 0 "GAATTC")
   (residues-string test-seq))
 "GAATTC")

(is-error
 (let ((test-seq (make-instance 'adjustable-dna-sequence)))
   (insert-residues test-seq 0 "U")
   (residues-string test-seq))
 sequence-encoding-error)

(is 
 (let ((test-seq (make-instance 'adjustable-dna-sequence)))
   (insert-residues test-seq 0 "GAATTC")
   (setf (residue test-seq 0) #\A)
   (setf (residue test-seq 1) #\T)
   (residues-string test-seq))
 "ATATTC")


;; RNA Sequences

(is 
 (let ((test-seq (make-instance 'adjustable-rna-sequence :initial-contents "GAAUUC")))
   (residues-string test-seq))
 "GAAUUC")

(is 
 (let ((test-seq (make-instance 'adjustable-rna-sequence)))
   (insert-residues test-seq 0 "GAAUUC")
   (residues-string test-seq))
 "GAAUUC")

(is 
 (let ((test-seq (make-instance 'adjustable-rna-sequence)))
   (insert-residues test-seq 0 "GAAUUC")
   (setf (residue test-seq 0) #\A)
   (setf (residue test-seq 1) #\U)
   (residues-string test-seq))
 "AUAUUC")

;; Amino Acid Sequences

(is
 (let ((test-seq (make-instance 'adjustable-aa-sequence :initial-contents "MVNPTVFFDIAVDGEPLGRV")))
   (residues-string test-seq))
 "MVNPTVFFDIAVDGEPLGRV")

(is
 (let ((test-seq (make-instance 'adjustable-aa-sequence)))
   (insert-residues test-seq 0 "MVN")
   (residues-string test-seq))
 "MVN")

(finalize)
