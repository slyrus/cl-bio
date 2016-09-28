;;; PDB (Protein Data Bank) File Parser
;;;
;;; Copyright (c) 2007 Cyrus Harmon (ch-lisp@bobobeach.com)
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

;;; PDB Files consist of a number of "records" which are, in turn, one
;;; or more 80-column lines of ASCII text.
;;;
;;; The reference for the file format can be found at:
;;;   http://www.wwpdb.org/documentation/format23/v2.3.html
;;;
;;; There are a number classes of records:
;;;
;;;  * single (non-continuable) records. Records of these types may
;;;    only appear once (per type) and are not "continuable" (see below).
;;;
;;;  * single continuable records. Records of these types may only
;;;    appear once (per type), but are continuable across mutiple lines.
;;;
;;;  * multiple records. multiple records (per type) are permitted and
;;;    these are continuable across multiple lines.
;;;
;;; * extended line multiple records
;;;
;;; * grouping records
;;; 

(in-package :bio)

;;; since we need to look ahead to see if we are continuing lines or
;;; not, establish a special variable to hold the value of the current
;;; line so that we only need to read it once.
(defparameter *current-line* nil)
(defparameter *current-entry* nil)

(defun read-pdb-record-name (line)
  "returns the first word of a given line."
  (let ((strings (nth-value 1 (ppcre:scan-to-strings "^(\\w+)\\s*" (subseq line 0 6)))))
    (when strings (elt strings 0))))

(defclass pdb-entry ()
  ((classification :initarg :classification :accessor classification :initform nil)
   (dep-date :initarg :dep-date :accessor dep-date :initform nil)
   (id-code :initarg :id-code :accessor id-code :initform nil)
   (obsolete :initarg :obsolete :accessor obsolete :initform nil)
   (title :initarg :title :accessor title :initform nil)
   (molecules :initarg :molecules :accessor molecules :initform nil)
   (chains :initarg :chains :accessor chains :initform nil)
   (atom-hash :initarg :atom-hash :accessor atom-hash :initform (make-hash-table))))

(defclass pdb-molecule ()
  ((id :initarg :id :accessor :id)
   (name :initarg :name :accessor name)
   (chains :initarg :chains :accessor chains)
   (fragments :initarg :fragments :accessor fragments)
   (synonym :initarg :synonym :accessor synonym)
   (ec :initarg :ec :accessor ec)
   (engineered :initarg :engineered :accessor engineered)
   (mutation :initarg :mutation :accessor mutation)
   (other-details :initarg :other-details :accessor other-details)))

(defclass pdb-chain ()
  ((name :initarg :name :accessor name)
   (sequence :initarg :sequence :accessor chain-sequence)))

(defclass pdb-record ()
  ((record-name :reader record-name :allocation class)
   (continuable :initarg :continuable :accessor continuable
                :initform nil))
  (:documentation "class for holding information about pdb-records
  while parsing PDB files."))

(defclass continuable-pdb-record (pdb-record)
  ((continuable :initarg :continuable :accessor continuable
                :initform t)
   (continuation-columns :initarg :continuation-columns
                         :accessor continuation-columns
                         :initform '(8 10))
   (field-columns :initarg :field-columns
                  :accessor field-columns
                  :initform '(10 70))
   (lines :initarg :lines :accessor lines :initform nil)
   (data :initarg :data :accessor data :initform nil))
  (:documentation "subclass of pdb-record for holding information
  about contiuable pdb-records for use while parsing PDB files."))

(defgeneric start-pdb-record (record-name line &key entry)
  (:documentation "Reads the first line of a PDB record"))

(defgeneric continue-pdb-record (record line cont &key entry))

(defgeneric finish-pdb-record (record &key entry))

(defgeneric read-continuation (record line)
  (:documentation "generic function for reading the second and later
  lines of a continued record of a PDB file."))

(defmethod read-continuation ((record continuable-pdb-record) line)
  (destructuring-bind (start end)
      (continuation-columns record)
    (parse-integer (subseq line start end))))

(defmethod continue-pdb-record ((record continuable-pdb-record) line cont
                                &key (entry *current-entry*))
  (declare (ignore entry))
  (destructuring-bind (start end)
      (field-columns record)
    (let ((continuation-lines (subseq line (1+ start) end)))
      (push continuation-lines (lines record)))))

(defmethod finish-pdb-record (record &key (entry *current-entry*))
  (declare (ignore entry))
  record)

(defmethod finish-pdb-record ((record continuable-pdb-record)
                              &key (entry *current-entry*))
  (declare (ignore entry))
  (setf (data record)
        (apply #'concatenate 'string
               (mapcar #'remove-trailing-spaces (nreverse (lines record)))))
  record)

(defmethod start-pdb-record (record-name line &key (entry *current-entry*))
  (declare (ignore entry))
  (progn
    (format t "Ignoring ~A~%" line)))

(defmethod start-pdb-record ((record-name (eql :header)) line
                           &key (entry *current-entry*))
  (let ((classification (subseq line 10 50))
        (dep-date (subseq line 50 59))
        (id-code (subseq line 62 66)))
    (setf (classification entry)
          (remove-trailing-spaces classification))
    (setf (dep-date entry)
          (remove-trailing-spaces dep-date))
    (setf (id-code entry)
          (remove-trailing-spaces id-code)))
  nil)

(defmethod start-pdb-record ((record-name (eql :obslte)) line
                           &key (entry *current-entry*))
  (let ((record-name (subseq line 0 6))
        (continuation (subseq line 8 10))
        (date (subseq line 11 20))
        (id-code (subseq line 21 25))
        (rid-code-1 (subseq line 31 35))
        (rid-code-2 (subseq line 36 40))
        (rid-code-3 (subseq line 41 45))
        (rid-code-4 (subseq line 46 50))
        (rid-code-5 (subseq line 51 55))
        (rid-code-6 (subseq line 56 60))
        (rid-code-7 (subseq line 61 65))
        (rid-code-8 (subseq line 66 70)))))

(defclass pdb-title (continuable-pdb-record)
  ((record-name :initform "TITLE" :allocation :class)))

(defmethod start-pdb-record ((record-name (eql :title)) line
                          &key (entry *current-entry*))
  (declare (ignore entry))
  (let ((record (make-instance 'pdb-title)))
    (setf (lines record) (list (apply #'subseq line (field-columns record))))
    record))

(defmethod finish-pdb-record ((record pdb-title) &key (entry *current-entry*))
  (call-next-method)
  (setf (title entry)
        (data record)))

(defclass pdb-atom (pdb-record)
  ((record-name :initform "ATOM" :initarg :record-name :accessor record-name)
   (atom-number :initarg :atom-number :accessor atom-number)
   (atom-name :initarg :atom-name :accessor atom-name)
   (alt-loc :initarg :alt-loc :accessor alt-loc)
   (residue-name :initarg :residue-name :accessor residue-name)
   (chain-id :initarg :chain-id :accessor chain-id)
   (residue-seq-number :initarg :residue-seq-number :accessor residue-seq-number)
   (insertion-code :initarg :insertion-code :accessor insertion-code)
   (x-coord :initarg :x-coord :accessor x-coord)
   (y-coord :initarg :y-coord :accessor y-coord)
   (z-coord :initarg :z-coord :accessor z-coord)
   (occupancy :initarg :occupancy :accessor occupancy)
   (temp-factor :initarg :temp-factor :accessor temp-factor)
   (element-symbol :initarg :element-symbol :accessor element-symbol)
   (charge :initarg :charge :accessor charge)))

(defclass pdb-hetero-atom (pdb-atom)
  ((record-name :initform "HETATM" :initarg :record-name :accessor record-name)))

(defclass pdb-compound (continuable-pdb-record)
  ((record-name :initform "COMPND" :allocation :class)))

(defmethod start-pdb-record ((record-name (eql :compnd)) line
                          &key (entry *current-entry*))
  (declare (ignore entry))
  (let ((record (make-instance 'pdb-compound)))
    (setf (lines record) (list (apply #'subseq line (field-columns record))))
    record))

(defun parse-pdb-atom-record (record-class line entry)
  (let ((record-name (remove-trailing-spaces (subseq line 0 6)))
        (atom-number (parse-integer (subseq line 6 11)))
        (atom-name (remove-trailing-spaces (subseq line 13 16)))
        (alt-loc (subseq line 16 17))
        (residue-name (subseq line 17 20))
        (chain-id (remove-initial-spaces (subseq line 20 22)))
        (residue-seq-number (parse-integer (subseq line 22 26)))
        (insertion-code (subseq line 26 27))
        (x-coord (parse-number:parse-real-number (subseq line 30 38)))
        (y-coord (parse-number:parse-real-number (subseq line 38 46)))
        (z-coord (parse-number:parse-real-number (subseq line 46 54)))
        (occupancy (parse-number:parse-real-number (subseq line 54 60)))
        (temp-factor (parse-number:parse-real-number (subseq line 60 66)))
        (element-symbol (remove-initial-spaces (subseq line 76 78)))
        (charge (let ((charge (remove-initial-spaces (subseq line 78 80))))
                  (if (zerop (length charge))
                      0
                      (parse-integer charge)))))
    (let ((atom (make-instance record-class
                               :record-name record-name 
                               :atom-number atom-number 
                               :atom-name atom-name
                               :alt-loc alt-loc 
                               :residue-name residue-name 
                               :chain-id chain-id
                               :residue-seq-number residue-seq-number 
                               :insertion-code insertion-code
                               :x-coord x-coord 
                               :y-coord y-coord 
                               :z-coord z-coord
                               :occupancy occupancy 
                               :temp-factor temp-factor
                               :element-symbol element-symbol 
                               :charge charge)))
      (setf (gethash atom-number (atom-hash entry))
            atom))))

(defmethod start-pdb-record ((record-name (eql :atom)) line
                          &key (entry *current-entry*))
  (parse-pdb-atom-record 'pdb-atom line entry))

(defmethod start-pdb-record ((record-name (eql :hetatm)) line
                             &key (entry *current-entry*))
  (parse-pdb-atom-record 'pdb-hetero-atom line entry))

(defun find-first-char (char string &key (start 0) (escape-char #\\))
  (let ((pos (position char string :start start)))
    (when pos (if (and (plusp pos)
                       (char= (char string (1- pos)) escape-char))
                  (find-first-char char string :start (1+ pos) :escape-char escape-char)
                  pos))))

(defun parse-specification (string &optional start)
  (let ((colon-pos (find-first-char #\: string :start start)))
    (when colon-pos
      (let ((semicolon-pos (find-first-char #\; string
                                            :start colon-pos)))
        (list (remove-initial-spaces (subseq string start colon-pos))
              (remove-initial-spaces (subseq string (1+ colon-pos)
                                             (or semicolon-pos
                                                 (length string))))
              (if semicolon-pos (1+ semicolon-pos)
                  (length string)))))))

(defun parse-specification-list (string)
  (loop with next = 0
     for (token value start) = (parse-specification string next)
     then (parse-specification string start)
     while token
     collect
       (cons token value)))

(defmethod finish-pdb-record ((record pdb-compound) &key (entry *current-entry*))
  (declare (ignore entry))
  (call-next-method)
  (print (parse-specification-list (print (data record)))))

(defgeneric write-pdb-record (record-type data stream))

(defun write-pdb-header (entry stream)
  (format stream "~A~10,0T~A~50,0T~A~62,0T~A~80,0T~%"
          "HEADER"
          (classification entry)
          (dep-date entry)
          (id-code entry)))

(defun write-continuable-pdb-record (record-type data stream
                                     &key
                                     (continuation-start 8)
                                     (field-start 10)
                                     (line-length 60))
  (loop with i = 0
     for line from 1
     while (< i (length data))
     do (let ((end (min (length data)
                        (+ i line-length))))
          (format stream "~?"
                  (format nil "~~A~~~D,0T~~@[~~2D ~~]~~~D,0T~~A~~80,0T~~%"
                          continuation-start
                          field-start)
                  (list (typecase record-type
                          (string (string-upcase record-type))
                          (symbol (symbol-name record-type)))
                        (when (> line 1) line)
                        (subseq data i end))))
     (incf i (if (zerop i)
                 line-length
                 (1- 59)))))

(defmethod write-pdb-record ((record-type (eql :title)) title stream)
  (write-continuable-pdb-record record-type title stream))

(defparameter *continuable-records*
  '(:author :caveat :compnd :expdata :keywds :obslte :source :sprsde
    :title :anisou :atom :cispep :conect :dbref :helix :het :hetsyn
    :link :modres :mtrix1 :mtrix2 :mtrix3 :revdat :eqadv :seqres
    :sheet :sigatm :siguij :site :ssbond :tvect :formul :hetatm :hetnam))

(defun read-pdb-record (stream)
  (let ((record (start-pdb-record
                 (intern (read-pdb-record-name *current-line*) :keyword)
                 *current-line*)))
    (if record
        (progn
          (if (continuable record)
              (loop for str = (setf *current-line* (read-line stream nil nil))
                 while str
                 do
                 (if (equal (record-name record)
                            (read-pdb-record-name str))
                     (let ((cont (read-continuation record str)))
                       (if cont
                           (continue-pdb-record record str cont)
                           (return)))
                     (return)))
              (setf *current-line* (read-line stream nil nil)))
          (finish-pdb-record record))
      (setf *current-line* (read-line stream nil nil)))))

(defun read-pdb-stream (stream)
  ;; bind *current-line* per thread to make this thread-safe
  (let ((*current-line* (read-line stream nil nil))
        (*current-entry* (make-instance 'pdb-entry)))
    (loop while *current-line*
       do (read-pdb-record stream))
    *current-entry*))

(defun read-pdb-file (file)
  (with-open-file (stream file)
    (read-pdb-stream stream)))

(defun write-pdb-stream (stream entry)
  (write-pdb-header entry stream)
  (let ((title (title entry)))
    (when title (write-pdb-record :title title stream))))
