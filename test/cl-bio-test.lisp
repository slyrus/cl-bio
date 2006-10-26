
(in-package :cl-bio-test)

;;; ranges

;; both ranges in forward direction
(assert
 (range-contains
  (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
  (make-instance 'ds-range :start 2 :end 99 :strand +plus-strand+)))

;; range1 is reversed
(assert
 (range-contains
  (make-instance 'ds-range :start 100 :end 1 :strand +plus-strand+)
  (make-instance 'ds-range :start 2 :end 99 :strand +plus-strand+)))

;; range2 is reversed
(assert
 (range-contains
  (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
  (make-instance 'ds-range :start 99 :end 2 :strand +plus-strand+)))

;; both ranges are reversed
(assert
 (range-contains
  (make-instance 'ds-range :start 100 :end 1 :strand +plus-strand+)
  (make-instance 'ds-range :start 99 :end 2 :strand +plus-strand+)))

;; left overlap fails
(assert
 (not (range-contains
       (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 0 :end 99 :strand +plus-strand+))))


;; right overlap fails
(assert
 (not (range-contains
       (make-instance 'ds-range :start 1 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 2 :end 101 :strand +plus-strand+))))

;; both ends overlap fails
(assert
 (not (range-contains
       (make-instance 'ds-range :start 2 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 1 :end 101 :strand +plus-strand+))))

;; both ends overlap fails, range 1 is reversed
(assert
 (not (range-contains
       (make-instance 'ds-range :start 100 :end 2 :strand +plus-strand+)
       (make-instance 'ds-range :start 1 :end 101 :strand +plus-strand+))))


;; both ends overlap fails, range 2 is reversed
(assert
 (not (range-contains
       (make-instance 'ds-range :start 2 :end 100 :strand +plus-strand+)
       (make-instance 'ds-range :start 101 :end 1 :strand +plus-strand+))))

;; both ends overlap fails, range 1 and range 2 are reversed
(assert
 (not (range-contains
       (make-instance 'ds-range :start 100 :end 2 :strand +plus-strand+)
       (make-instance 'ds-range :start 101 :end 1 :strand +plus-strand+))))

