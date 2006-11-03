;;; Range
;;; Linear ranges, for use with biological sequences
;;;
;;; Copyright (c) 2006 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :cl-bio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Ranges are spatial extents on biological sequences, having a start
;;; and an end. The ranges are directed, in that a range with end
;;; greater than start are considered to be in opposite orientation
;;; from ranges whose end is greater than or equal to there start. A
;;; range whose end is equal to its start are considered to be points.

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

(defmethod range-length ((range range))
  (- (range-end range)
     (range-start range)))

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

