
(in-package :cl-bio)

(defgeneric range-equal (range1 range2))
(defgeneric range-contains (range1 range2))
(defgeneric range-min (range1))
(defgeneric range-max (range1))

(defclass range ()
  ((start :accessor range-start :initarg :start)
   (end :accessor range-end :initarg :end)))

(defmethod range-equal ((range1 range)
                         (range2 range))
  (and (equal (range-start range1)
              (range-start range2))
       (equal (range-end range1)
              (range-end range2))))

(defmethod range-contains ((range1 range)
                           (range2 range))
  (let ((min2 (min (range-start range2)
                   (range-end range2)))
        (max2 (max (range-start range2)
                   (range-end range2))))
    (if (<= (range-start range1)
            (range-end range1))
        (and (<= (range-start range1) min2)
             (>= (range-end range1) max2))
        (and (<= (range-end range1) min2)
             (>= (range-start range1) max2)))))

(defmethod range-min ((range range))
  (min (range-start range)
       (range-end range)))

(defmethod range-max ((range range))
  (max (range-start range)
       (range-end range)))

(defconstant +plus-strand+ 1)
(defconstant +unknown-strand+ 0)
(defconstant +minus-strand+ -1)
(defconstant +both-strands+ 2)

(defclass ds-range (range)
  ((strand :accessor strand :initarg :strand)))

(defmethod range-equal ((range1 ds-range)
                         (range2 ds-range))
  (and (call-next-method)
       (equal (strand range1)
              (strand range2))))
