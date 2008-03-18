
(in-package :bio-user)

(defparameter *d* (make-random-dna-sequence 100))
(residues-string *d*)

(defparameter *dna-seq-1* (make-instance 'adjustable-dna-sequence))
(insert-residues *dna-seq-1* 0 "GAATTC")
(residues-string *dna-seq-1*)

(defparameter *df* (make-instance 'adjustable-dna-sequence
                                :initial-contents (residues-string (make-random-dna-sequence 1000))))
(defparameter *rf* (make-instance 'adjustable-rna-sequence
                                :initial-contents (residues-string (make-random-rna-sequence 100))))
(defparameter *af* (make-instance 'adjustable-aa-sequence
                                :initial-contents (residues-string (make-random-aa-sequence 100))))

(insert-residues *df* 0 "TTTT")
(insert-residues *rf* 0 "UUUU")
(insert-residues *af* 0 "HHHH")

(residues-string *df*)
(residues-string *rf*)
(residues-string *af*)
(seq-length *df*)

(bio::residue *df* 0)

(append-residues *df* "TTTT")
(append-residues *rf* "CUCU")
(append-residues *af* "YYYY")

(insert-residues *df* 1 "AAAA")

#+nil
(array-element-type (map '(vector (unsigned-byte 2))
                         #'(lambda (x)
                             (char-to-seq-code df x))
                         "TACGT"))

(flexichain:insert-vector*
 (bio::residues df) 0 (map '(vector (unsigned-byte 2))
                      #'(lambda (x)
                          (bio::char-to-seq-code df x))
                      "AAAAA"))


(array-element-type (make-array 5 :initial-contents '(0 0 0 0 0) :element-type '(unsigned-byte 2)))
(slot-value (bio::residues df) 'flexichain::element-type)

(residues-string df)

(let ((r (make-instance 'range :start 10 :end 15)))
  (residues-string-range df r))


(entrez::gb-set-get-gb-seqs esr1)

(bio::find-matches "t" (entrez::gb-seq-sequence-get-residues (entrez::gb-seq-get-sequence (car (entrez::gb-set-get-gb-seqs esr1)))))


(entrez::gb-seq-get-sequence (car (entrez::gb-set-get-gb-seqs esr1)))

(defparameter esrseq (car (entrez::gb-set-get-gb-seqs esr1)))

(cdr esrseq)
(cadr (assoc :|GBSeq_moltype| (cdr esrseq)))

(bio-io::convert-entrez-seq-to-bio-seq esrseq)

(defparameter df2 (make-instance 'adjustable-dna-sequence))
(setf (residues-string df2) "AACCGG")
(residues-string df2)

