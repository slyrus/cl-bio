;;; align.lisp
;;; Classes, generic functions, methods and functions for aligning
;;; biological sequences
;;;
;;; Copyright (c) 2002-2008 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(in-package :bio-align)

(defclass score-matrix ()
  ((list :accessor score-matrix-list :initarg :list)
   (hash :accessor score-matrix-hash :initarg :hash)
   (scores :accessor score-matrix-scores :initarg :scores)))

(defun make-score-matrix (&key list hash scores)
  (apply #'make-instance 'score-matrix
         (append
          (when list `(:list ,list))
          (when hash `(:hash ,hash))
          (when scores `(:scores ,scores)))))

(defclass alignment ()
  ((score :accessor alignment-score :initarg :score)
   (seq1 :accessor alignment-seq1 :initarg :seq1)
   (seq2 :accessor alignment-seq2 :initarg :seq2)
   (dp-matrix :accessor alignment-dp-matrix :initarg :dp-matrix)
   (dp-down-matrix :accessor alignment-dp-down-matrix :initarg :dp-down-matrix)
   (dp-right-matrix :accessor alignment-dp-right-matrix :initarg :dp-right-matrix)
   (dp-traceback :accessor alignment-dp-traceback :initarg :dp-traceback)))

(defun make-alignment (&key score seq1 seq2 dp-matrix dp-down-matrix dp-right-matrix dp-traceback)
  (apply #'make-instance 'alignment
         (append
          (when score `(:score ,score))
          (when seq1 `(:seq1 ,seq1))
          (when seq2 `(:seq2 ,seq2))
          (when dp-matrix `(:dp-matrix ,dp-matrix))
          (when dp-down-matrix `(:dp-down-matrix ,dp-down-matrix))
          (when dp-right-matrix `(:dp-right-matrix ,dp-right-matrix))
          (when dp-traceback `(:dp-traceback ,dp-traceback)))))

(defun alignment-results (a)
  (values (alignment-score a)
          (alignment-seq1 a)
          (alignment-seq2 a)))

(defun print-alignment-matrices (a)
  (print (alignment-dp-matrix a))
  (unless (null (alignment-dp-down-matrix a))
    (print (alignment-dp-down-matrix a)))
  (unless (null (alignment-dp-right-matrix a))
    (print (alignment-dp-right-matrix a)))
  (print (alignment-dp-traceback a)))

(defun parse-matrix (m-in)
  (let ((m (make-score-matrix
            :hash (make-hash-table :test #'equal)
            :scores (make-hash-table :test #'equal))))
    (let ((aa-list (first m-in)) (i 0))
      (setf (score-matrix-list m) (make-array (list (length m-in)) :initial-element 0))
      (dolist (symbol aa-list)
        (let ((c (aref (string symbol) 0))) 
          (setf (aref (score-matrix-list m) i) c)
          (setf (gethash c (score-matrix-hash m)) i)
          (setf i (+ i 1)))))
    (let ((i 0) (j 0))
      (dolist (l (rest m-in))
        (dolist (c l)
          (setf (gethash (list i j) (score-matrix-scores m)) c)
          (setf j (+ j 1)))
        (setf j 0)
        (setf i (+ i 1))))
    m))

(defparameter *blosum-62*
  (parse-matrix (read (open (ch-asdf:asdf-lookup-path
                             "asdf:/cl-bio-align/align/matrix/blosum62")))))

(defun get-score (m k l)  
  (gethash (list (gethash k (score-matrix-hash m)) (gethash l (score-matrix-hash m)))
           (score-matrix-scores m)))

(defun aa-score (k l &key (scoring-matrix *blosum-62*))
  (get-score scoring-matrix k l))

(defparameter transition-table (make-hash-table :test #'equal))
(setf (gethash #\A transition-table) #\G
      (gethash #\C transition-table) #\T
      (gethash #\G transition-table) #\A
      (gethash #\T transition-table) #\C)

(defun transition (residue)
  (let ((c (gethash (char-upcase residue) transition-table)))
    (if (null c) #\N c)))

(defparameter +gap-char+ #\-)
(declaim (type base-char +gap-char+))

(defparameter *gap* -8)
(defparameter *gap-extend* -2)
(declaim (type (signed-byte 8) *gap* *gap-extend*))

(defun na-score (k l)
  (declare (type base-char k l)
           (optimize (speed 3) (safety 0)))
  (cond
    ((eql k l) 4)
    ((eql k +gap-char+) *gap*)
    ((eql l +gap-char+) *gap*)
    ((eql (transition k) l) -2)
    (t -4)))
(declaim (ftype (function (base-char base-char)
                          (signed-byte 8))
                na-score)
         (inline na-score))

(defun gap-score (g i j)
  (if (= 0 (aref g i j))
      *gap*
      *gap-extend*))

(defconstant +match+ 0)
(defconstant +up+ 1)
(defconstant +left+ 2)
(defconstant +terminate+ 3)

(defun emit (m n i j a b &optional s1 s2)
  (cond
    ((or (and (= i 0) (= j 0)) (= (aref n i j) +terminate+))
     (list s1 s2))
    ((or (= i 0) (= (aref n i j) +left+))
     (emit m n i (1- j) a b
           (cons #\- s1) (cons (aref b (1- j)) s2)))
    ((or (= j 0) (= (aref n i j) +up+))
     (emit m n (- i 1) j a b
           (cons (aref a (- i 1)) s1) (cons #\- s2)))
    (t
     (emit m n (- i 1) (- j 1) a b
           (cons (aref a (- i 1)) s1) (cons (aref b (- j 1)) s2)))))

#+nil
(defun emit2 (m n i j a b &optional s1 s2)
  (cond
    ((and (= i 0) (= j 0))
     (list s1 s2))
    ((= i 0)
     (emit2 m n i (- j 1) a b
            (cons #\- s1) (cons (aref b (- j 1)) s2)))
    ((= j 0)
     (emit2 m n (- i 1) j a b
            (cons (aref a (- i 1)) s1) (cons #\- s2)))
    (t
     (let ((k (aref a (- i 1))) (l (aref b (- j 1))))
       (let ((x (+ (aref m (- i 1) (- j 1)) (score k l)))
             (y (+ (aref m (- i 1) j) (score k +gap-char+)))
             (z (+ (aref m i (- j 1)) (score +gap-char+ l))))
         (cond
           ((and (>= x y) (>= x z))
            (emit2 m n (- i 1) (- j 1) a b
                   (cons (aref a (- i 1)) s1) (cons (aref b (- j 1)) s2)))
           ((>= y z)
            (emit2 m n (- i 1) j a b
                   (cons (aref a (- i 1)) s1) (cons #\- s2)))
           (t
            (emit2 m n i (- j 1) a b
                   (cons #\- s1) (cons (aref b (- j 1)) s2)))))))))

(defun global-align-score (m n i j k l score-fn)
  (cond
    ((and (> i 0) (= j 0))
     (let ((y (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+)))))
       (setf (aref m i j) y) (setf (aref n i j) +up+)))
    ((and (= i 0) (> j 0))       
     (let ((z (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l)))))
       (setf (aref m i j) z) (setf (aref n i j) +left+)))
    (t
     (let ((x (+ (aref m (- i 1) (- j 1)) (apply score-fn (list k l))))
           (y (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+))))
           (z (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l)))))
       (cond
         ((and (>= x y) (>= x z))
          (setf (aref m i j) x
                (aref n i j) +match+))
         ((>= y z)
          (setf (aref m i j) y
                (aref n i j) +up+))
         (t
          (setf (aref m i j) z
                (aref n i j) +left+)))))))

(defun global-align-score-affine-gaps (m n d r i j k l score-fn)
  (cond
    ((and (> i 0) (= j 0))
     (let ((y (max (+ (aref d (- i 1) j) (if (> i 1) *gap-extend* *gap*))
                   (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+))))))
       (setf (aref m i j) y)
       (setf (aref n i j) +up+)
       (setf (aref d i j) y)
       (setf (aref r i j)
             (+ (aref r (- i 1) j) *gap*))))
    ((and (= i 0) (> j 0))
     (let ((z (max (+ (aref r i (- j 1))  (if (> j 1) *gap-extend* *gap*))
                   (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l))))))
       (setf (aref m i j) z)
       (setf (aref n i j) +left+)
       (setf (aref r i j) z)
       (setf (aref d i j)
             (+ (aref d i (- j 1)) *gap*))))
    (t
     (let ((x (+ (aref m (- i 1) (- j 1)) (apply score-fn (list k l))))
           (y (max (+ (aref d (- i 1) j) *gap-extend*)
                   (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+)))))
           (z (max (+ (aref r i (- j 1)) *gap-extend*)
                   (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l))))))
       (setf (aref d i j) y)
       (setf (aref r i j) z)
       (cond
         ((and (>= x y) (>= x z))
          (setf (aref m i j) x) (setf (aref n i j) +match+))
         ((>= y z)
          (setf (aref m i j) y) (setf (aref n i j) +up+))
         (t
          (setf (aref m i j) z) (setf (aref n i j) +left+)))))))

(defun global-align (a b score-fn)
  (let ((m (make-array (list (+ (length a) 1) (+ (length b) 1))
                       :initial-element 0
                       :element-type '(signed-byte 31)))
        (n (make-array (list (+ (length a) 1) (+ (length b) 1))
                       :initial-element 0
                       :element-type '(signed-byte 31))))
    (declare (type (simple-array (signed-byte 31)
                                 (* *))
                   m n))
    (dotimes (i (+ (length a) 1))
      (if (> i 0)
          (global-align-score m n i 0 (aref a (- i 1)) +gap-char+ score-fn)
          (setf (aref m i 0) 0)))
    (dotimes (j (+ (length b) 1))
      (if (> j 0)
          (global-align-score m n 0 j +gap-char+ (aref b (- j 1)) score-fn)
          (setf (aref m 0 j) 0)))
    (do ((i 1 (+ i 1)))
        ((> i (length a)))
      (do ((j 1 (+ j 1)))
          ((> j (length b)))
          (global-align-score m n i j (aref a (- i 1)) (aref b (- j 1)) score-fn)))
    (let ((z (emit m n (+ (length a) 0) (+ (length b) 0) a b)))
      (make-alignment
       :score (aref m (length a) (length b))
       :seq1 (coerce (first z) 'string)
       :seq2 (coerce (second z) 'string)
       :dp-matrix m
       :dp-traceback n))))

(macrolet ((global-align-score-mac (m n i j k l score-fn)
             (alexandria:once-only
                 (m n i j k l)
               `(cond
                  ((and (> ,i 0) (= ,j 0))
                   (let ((y (+ (aref ,m (1- ,i) ,j) (,score-fn ,k +gap-char+))))
                     (setf (aref ,m ,i ,j) y
                           (aref ,n ,i ,j) +up+)))
                  ((and (= ,i 0) (> ,j 0))       
                   (let ((z (+ (aref ,m ,i (1- ,j)) (,score-fn +gap-char+ ,l))))
                     (setf (aref ,m ,i ,j) z
                           (aref ,n ,i ,j) +left+)))
                  (t
                   (let ((x (+ (aref ,m (1- ,i) (1- ,j)) (,score-fn ,k ,l)))
                         (y (+ (aref ,m (1- ,i) ,j) (,score-fn ,k +gap-char+)))
                         (z (+ (aref ,m ,i (1- ,j)) (,score-fn +gap-char+ ,l))))
                     (cond
                       ((and (>= x y) (>= x z))
                        (setf (aref ,m ,i ,j) x
                              (aref ,n ,i ,j) +match+))
                       ((>= y z)
                        (setf (aref ,m ,i ,j) y
                              (aref ,n ,i ,j) +up+))
                       (t
                        (setf (aref ,m ,i ,j) z
                              (aref ,n ,i ,j) +left+))))))))
           (def-global-align-fun (fun-name score-fn)
             `(defun ,fun-name (a b)
                (let ((m (make-array (list (+ (length a) 1) (+ (length b) 1))
                                     :initial-element 0))
                      (n (make-array (list (+ (length a) 1) (+ (length b) 1))
                                     :initial-element 0
                                     :element-type '(unsigned-byte 2))))
                  (declare (type (simple-array (unsigned-byte 2) (* *)) n))
                  (dotimes (i (+ (length a) 1))
                    (declare (type fixnum i))
                    (if (> i 0)
                        (global-align-score-mac m n i 0 (aref a (- i 1)) +gap-char+ ,score-fn)
                        (setf (aref m i 0) 0)))
                  (dotimes (j (+ (length b) 1))
                    (declare (type fixnum j))
                    (if (> j 0)
                        (global-align-score-mac m n 0 j +gap-char+ (aref b (- j 1)) ,score-fn)
                        (setf (aref m 0 j) 0)))
                  (let ((ilimit (length a))
                        (jlimit (length b)))
                    (do ((i 1 (+ i 1)))
                        ((> i ilimit))
                      (declare (type fixnum i))
                      (do ((j 1 (+ j 1)))
                          ((> j jlimit))
                        (declare (type fixnum j))
                        (global-align-score-mac m n i j (aref a (1- i)) (aref b (1- j)) ,score-fn))))
                  (let ((z (emit m n (+ (length a) 0) (+ (length b) 0) a b)))
                    (make-alignment
                     :score (aref m (length a) (length b))
                     :seq1 (coerce (first z) 'string)
                     :seq2 (coerce (second z) 'string)
                     :dp-matrix m
                     :dp-traceback n))))))
  
  (def-global-align-fun %global-align-aa aa-score)
  (def-global-align-fun %global-align-na na-score))

(defgeneric global-align-aa (seq1 seq2))
(defmethod global-align-aa ((seq1 aa-sequence-with-residues)
                            (seq2 aa-sequence-with-residues))
  (%global-align-aa (residues-string seq1)
                    (residues-string seq2)))

(defgeneric global-align-na (seq1 seq2))
(defmethod global-align-na ((seq1 na-sequence-with-residues)
                            (seq2 na-sequence-with-residues))
  (%global-align-na (residues-string seq1)
                     (residues-string seq2)))

(defgeneric global-align-affine-gaps (a b score-fn))

(defmethod global-align-affine-gaps ((a vector) (b vector) score-fn)
  (let ((m (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (n (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (d (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (r (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0)))
    (dotimes (i (+ (length a) 1))
      (if (> i 0)
          (global-align-score-affine-gaps m n d r i 0 (aref a (- i 1)) +gap-char+ score-fn)
          (setf (aref m i 0) 0)))
    (dotimes (j (+ (length b) 1))
      (if (> j 0)
          (global-align-score-affine-gaps m n d r 0 j +gap-char+ (aref b (- j 1)) score-fn)
          (setf (aref m 0 j) 0)))
    (do ((i 1 (+ i 1)))
        ((> i (length a)))
        (do ((j 1 (+ j 1)))
            ((> j (length b)))
          (global-align-score-affine-gaps m n d r i j
                                          (aref a (- i 1)) (aref b (- j 1)) score-fn)))
    (let ((z (emit m n (+ (length a) 0) (+ (length b) 0) a b)))
      (make-alignment
       :score (aref m (length a) (length b))
       :seq1 (coerce (first z) 'string)
       :seq2 (coerce (second z) 'string)
       :dp-matrix m
       :dp-down-matrix d
       :dp-right-matrix r
       :dp-traceback n))))

(defmethod global-align-affine-gaps ((seq1 aa-sequence-with-residues)
                                     (seq2 aa-sequence-with-residues)
                                     score-fn)
  (global-align-affine-gaps (residues-string seq1)
                            (residues-string seq2)
                            score-fn))

(defun %global-align-aa-affine-gaps (a b)
  (global-align-affine-gaps a b #'aa-score))

(defgeneric global-align-aa-affine-gaps (seq1 seq2))
(defmethod global-align-aa-affine-gaps ((seq1 aa-sequence-with-residues)
                                        (seq2 aa-sequence-with-residues))
  (%global-align-aa-affine-gaps (residues-string seq1)
                                (residues-string seq2)))

(defun %global-align-na-affine-gaps (a b)
  (global-align-affine-gaps a b #'na-score))

(defgeneric global-align-na-affine-gaps (seq1 seq2))
(defmethod global-align-na-affine-gaps ((seq1 na-sequence-with-residues)
                                        (seq2 na-sequence-with-residues))
  (%global-align-na-affine-gaps (residues-string seq1)
                                (residues-string seq2)))


(defun local-align-score-affine-gaps (m n d r i j k l score-fn)
  (cond
    ((and (> i 0) (= j 0))
     (let ((y (max (+ (aref d (- i 1) j) (if (> i 1) *gap-extend* *gap*))
                   (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+))))))
       (if (> 0 y)
           (progn (setf (aref m i j) 0) (setf (aref n i j) +terminate+) 0)
           (progn (setf (aref m i j) y) (setf (aref n i j) +up+)))
       (setf (aref d i j) y)))
    ((and (= i 0) (> j 0))       
     (let ((z (max (+ (aref r i (- j 1)) (if (> j 1) *gap-extend* *gap*))
                   (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l))))))
       (if (> 0 z)
           (progn (setf (aref m i j) 0) (setf (aref n i j) +terminate+) 0)
           (progn (setf (aref m i j) z) (setf (aref n i j) +left+)))
       (setf (aref r i j) z)))
    (t
     (let ((x (+ (aref m (- i 1) (- j 1)) (apply score-fn (list k l))))
           (y (max (+ (aref d (- i 1) j) *gap-extend*)
                   (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+)))))
           (z (max (+ (aref r i (- j 1)) *gap-extend*)
                   (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l))))))
       (setf (aref d i j) y)
       (setf (aref r i j) z)
       (cond
         ((and (> 0 x) (> 0 y) (> 0 z))
          (setf (aref m i j) 0) (setf (aref n i j) +terminate+) 0)
         ((and (>= x y) (>= x z))
          (setf (aref m i j) x) (setf (aref n i j) +match+) x)
         ((>= y z)
          (setf (aref m i j) y) (setf (aref n i j) +up+) y)
         (t
          (setf (aref m i j) z) (setf (aref n i j) +left+) z))))))

(defgeneric local-align-affine-gaps (a b score-fn))

(defmethod local-align-affine-gaps (a b score-fn)
  (let ((m (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (n (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (d (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (r (make-array (list (+ (length a) 1) (+ (length b) 1)) :initial-element 0))
        (maxscore 0)
        (maxi 0)
        (maxj 0))
    (dotimes (i (+ (length a) 1))
      (setf (aref m i 0) 0))
    (dotimes (j (+ (length b) 1))
      (setf (aref m 0 j) 0))
    (do ((i 1 (+ i 1)))
        ((> i (length a)))
      (do ((j 1 (+ j 1)))
          ((> j (length b)))
        (let ((s (local-align-score-affine-gaps m n d r i j
                                                (aref a (- i 1)) (aref b (- j 1)) score-fn)))
          (if (> s maxscore)
              (progn
                (setf maxscore s)
                (setf maxi i)
                (setf maxj j))))))
    (let ((z (emit m n maxi maxj a b)))
      (make-alignment
       :score (aref m maxi maxj)
       :seq1 (coerce (first z) 'string)
       :seq2 (coerce (second z) 'string)
       :dp-matrix m
       :dp-down-matrix d
       :dp-right-matrix r
       :dp-traceback n))))

(defun %local-align-aa-affine-gaps (a b)
  (local-align-affine-gaps a b #'aa-score))

(defgeneric local-align-aa-affine-gaps (seq1 seq2))
(defmethod local-align-aa-affine-gaps ((seq1 na-sequence-with-residues)
                                       (seq2 na-sequence-with-residues))
  (%local-align-aa-affine-gaps (residues-string seq1)
                               (residues-string seq2)))

(defun %local-align-na-affine-gaps (a b)
  (local-align-affine-gaps a b #'na-score))

(defgeneric local-align-na-affine-gaps (seq1 seq2))
(defmethod local-align-na-affine-gaps ((seq1 na-sequence-with-residues)
                                       (seq2 na-sequence-with-residues))
  (%local-align-na-affine-gaps (residues-string seq1)
                               (residues-string seq2)))

(defmethod local-align-affine-gaps ((seq1 aa-sequence-with-residues)
                                    (seq2 aa-sequence-with-residues)
                                    score-fn)
  (local-align-affine-gaps (residues-string seq1)
                           (residues-string seq2)
                           score-fn))

(defun local-align-score (m n i j k l score-fn)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum i j)
           (type base-char k l)
           (type (simple-array fixnum (* *)) n m))
  (cond
    ((and (> i 0) (= j 0))
     (let ((y (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+)))))
       (declare (type fixnum y))
       (if (> 0 y)
           (progn
             (setf (aref m i j) 0
                   (aref n i j) +terminate+)
             0)
           (setf (aref m i j) y
                 (aref n i j) +up+))))
    ((and (= i 0) (> j 0))       
     (let ((z (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l)))))
       (declare (type fixnum z))
       (if (> 0 z)
           (progn (setf (aref m i j) 0) (setf (aref n i j) +terminate+) 0)
           (progn (setf (aref m i j) z) (setf (aref n i j) +left+)))))
    (t
     (let ((x (+ (aref m (- i 1) (- j 1)) (apply score-fn (list k l))))
           (y (+ (aref m (- i 1) j) (apply score-fn (list k +gap-char+))))
           (z (+ (aref m i (- j 1)) (apply score-fn (list +gap-char+ l)))))
       (declare (type fixnum x y z))
       (cond
         ((and (> 0 x) (> 0 y) (> 0 z))
          (setf (aref m i j) 0) (setf (aref n i j) +terminate+) 0)
         ((and (>= x y) (>= x z))
          (setf (aref m i j) x) (setf (aref n i j) +match+) x)
         ((>= y z)
          (setf (aref m i j) y) (setf (aref n i j) +up+) y)
         (t
          (setf (aref m i j) z) (setf (aref n i j) +left+) z))))))

(defun local-align (a b score-fn)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array character *) a b))
  (let ((m (make-array (list (+ (length a) 1) (+ (length b) 1))
                       :initial-element 0 :element-type 'fixnum))
        (n (make-array (list (+ (length a) 1) (+ (length b) 1))
                       :initial-element 0 :element-type 'fixnum))
        (maxscore 0)
        (maxi 0)
        (maxj 0)
        (lena (length a))
        (lenb (length b)))
    (declare (type fixnum maxscore))
    (dotimes (i (+ lena 1))
      (setf (aref m i 0) 0))
    (dotimes (j (+ lenb 1))
      (setf (aref m 0 j) 0))
    (do ((i 1 (+ i 1)))
        ((> i lena))
      (declare (type fixnum i))
      (do ((j 1 (+ j 1)))
          ((> j lenb))
        (declare (type fixnum j))
        (let ((s (local-align-score m n i j
                                    (aref a (- i 1)) (aref b (- j 1)) score-fn)))
          (if (> s maxscore)
              (progn
                (setf maxscore s)
                (setf maxi i)
                (setf maxj j))))))
    (let ((z (emit m n maxi maxj a b)))
      (make-alignment
       :score (aref m maxi maxj)
       :seq1 (coerce (first z) 'string)
       :seq2 (coerce (second z) 'string)
       :dp-matrix m
       :dp-traceback n))))

(defun %local-align-aa (a b)
  (local-align a b #'aa-score))

(defun %local-align-na (a b)
  (local-align a b #'na-score))

(defgeneric local-align-aa (seq1 seq2))
(defmethod local-align-aa ((seq1 aa-sequence-with-residues)
                           (seq2 aa-sequence-with-residues))
  (%local-align-aa (residues-string seq1)
                   (residues-string seq2)))

(defgeneric local-align-na (seq1 seq2))
(defmethod local-align-na ((seq1 na-sequence-with-residues)
                           (seq2 na-sequence-with-residues))
  (%local-align-na (residues-string seq1)
                   (residues-string seq2)))

