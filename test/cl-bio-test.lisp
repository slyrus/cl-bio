
(in-package :bio-test)

(plan 11)

;;; ranges

;; both ranges in forward direction
(ok
 (range-contains
  (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
  (make-instance 'ds-range :start 2 :end 99 :strand +plus-strand+)))

;; range1 is reversed
(ok
 (range-contains
  (make-instance 'ds-range :start 100 :end 1 :strand +plus-strand+)
  (make-instance 'ds-range :start 2 :end 99 :strand +plus-strand+)))

;; range2 is reversed
(ok
 (range-contains
  (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
  (make-instance 'ds-range :start 99 :end 2 :strand +plus-strand+)))

;; both ranges are reversed
(ok
 (range-contains
  (make-instance 'ds-range :start 100 :end 1 :strand +plus-strand+)
  (make-instance 'ds-range :start 99 :end 2 :strand +plus-strand+)))

;; left overlap fails
(ok
 (not (range-contains
       (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 0 :end 99 :strand +plus-strand+))))


;; right overlap fails
(ok
 (not (range-contains
       (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 2 :end 101 :strand +plus-strand+))))

;; both ends overlap fails
(ok
 (not (range-contains
       (make-instance 'ds-range :start 2 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 1 :end 101 :strand +plus-strand+))))

;; both ends overlap fails, range 1 is reversed
(ok
 (not (range-contains
       (make-instance 'ds-range :start 100 :end 2 :strand +plus-strand+)
       (make-instance 'ds-range :start 1 :end 101 :strand +plus-strand+))))


;; both ends overlap fails, range 2 is reversed
(ok
 (not (range-contains
       (make-instance 'ds-range :start 2 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 101 :end 1 :strand +plus-strand+))))

;; both ends overlap fails, range 1 and range 2 are reversed
(ok
 (not (range-contains
       (make-instance 'ds-range :start 100 :end 2 :strand +plus-strand+)
       (make-instance 'ds-range :start 101 :end 1 :strand +plus-strand+))))


;; file IO

(is
 (let ((dpp (car (read-fasta-file
                  (asdf:component-pathname
                   (reduce #'asdf:find-component
                           (list nil "cl-bio-test" "data" "dpp-fasta")))))))
   (seq-length dpp))
 3427)

#+nil
(ok
 (let ((2l (car (read-fasta-file "data/2L_genomic_dmel_RELEASE3-1.FASTA"))))
      2l))

#+nil
(ok
 (defun test-load-trunc-2l ()
   (let ((2l (car (read-fasta-file "data/trunc-2l.fasta"))))
     2l)))

(finalize)
