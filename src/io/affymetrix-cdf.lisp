;;; Affymetrix CDF File Parser
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


(in-package :cl-bio-io)

(defun read-cdf-line (stream)
  (let ((line-type (read-until-char stream #\=))
        (line (progn (read-char stream)
                     (read-dos-line stream))))
    (values line-type line)))

(defgeneric parse-cdf-section (chip stream section section-line))

(defmethod parse-cdf-section (chip stream (section (eql :cdf)) section-line)
  (multiple-value-bind (line-type line)
      (read-cdf-line stream)
    (print (cons line-type line))))

(defmethod parse-cdf-section (chip stream (section (eql :chip)) section-line)
  (loop while (not (check-end-of-line stream))
     do (multiple-value-bind (line-type line)
            (read-cdf-line stream)
          (cond ((equal line-type "Name")
                 (setf (cl-bio::name chip)
                       line))
                ((equal line-type "NumberOfUnits")
                 (setf (cl-bio::unit-count chip)
                       (parse-integer line))))
          (print (cons line-type line)))))

(defmethod parse-cdf-section (chip stream (section (eql :qc)) section-line))

(defun parse-tag-value-line (line)
  (coerce (nth-value
           1
           (cl-ppcre:scan-to-strings "^(\\w+)=(.+)$" line))
          'list))

(defmethod parse-cdf-section (chip stream (section (eql :unit-block)) section-line)
  (multiple-value-bind (matched term-vec)
      (cl-ppcre:scan-to-strings "\\[([a-zA-Z]+)(\\d+)_Block(\\d+)\\]" section-line)
    (declare (ignore matched))
    (when term-vec
      (let ((unit (elt term-vec 0))
            (block (elt term-vec 1)))
        (let (name block-number num-atoms num-cells start-position end-position cell-header)
          (do ((line (print (read-dos-line stream)) (read-dos-line stream)
                     ))
              ((equal line ""))
            (destructuring-bind (tag value)
                (parse-tag-value-line line)
              (print tag)
              (cond ((equal "Name" tag)
                     (print (cons :name value))
                     (setf name value))
                    ((equal "BlockNumber" tag)
                     (print (cons :block-number value))
                     (setf block-number value))
                    ((equal "NumAtoms" tag)
                     (print (cons :num-atoms value))
                     (setf num-atoms value))
                    ((equal "NumCells" tag)
                     (print (cons :num-cells value))
                     (setf num-cells value))
                    ((equal "StartPosition" tag)
                     (print (cons :start-position value))
                     (setf start-position value))
                    ((equal "StopPosition" tag)
                     (print (cons :stop-position value))
                     (setf end-position value))
                    ((equal "CellHeader" tag)
                     (print (cons :cell-header value))
                     (setf cell-header value))
                    (t
                     (multiple-value-bind (matched cell-num)
                         (cl-ppcre:scan-to-strings "Cell(\\d+)" tag)
                       (declare (ignore matched))
                       (when cell-num
                         (print (cons :cell-num (elt cell-num 0)))))))))          
          (break))
        ))))

(defmethod parse-cdf-section (chip stream (section (eql :unit)) section-line)
  (if (cl-ppcre:scan-to-strings "\\[([a-zA-Z]+)(\\d+)_Block(\\d+)\\]" section-line)
      (parse-cdf-section chip stream :unit-block section-line)
      (multiple-value-bind (matched term-vec)
          (cl-ppcre:scan-to-strings "\\[([a-zA-Z]+)(\\d+)\\]" section-line)
        (declare (ignore matched))
        (if term-vec
            (progn
              #+nil (progn do the right thing here))
            ))))



(defun parse-qc-sections (stream))

(defun parse-units-section (stream))

(defun parse-affymetrix-cdf (chip stream)
  (let ((section (read-dos-line stream)))
    (when section
      (let ((parsed-section
             (nth-value 1
                        (cl-ppcre:scan-to-strings "\\[([a-zA-Z]+)\\d*\\w*\\]" section))))
        (when parsed-section
          (let ((section-key (intern (string-upcase (elt parsed-section 0)) :keyword)))
            (print section-key)
            (parse-cdf-section chip stream section-key section)))))))

(defun parse-affymetrix-cdf-file (file)
  (let ((chip (make-instance 'cl-bio::chip)))
    (with-open-file (stream file)
      (loop while (not (check-end-of-file stream))
         do 
         (parse-affymetrix-cdf chip stream)))
    chip))

