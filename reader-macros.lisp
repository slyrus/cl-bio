
(cl:in-package :bio)

(defun %read-aa-sequence (stream char arg)
  (declare (ignore char arg))
  (let ((string (read stream t nil t)))
    (aa-sequence string)))

(defun %read-dna-sequence (stream char arg)
  (declare (ignore char arg))
  (let ((string (read stream t nil t)))
    (dna-sequence string)))

(defun %read-rna-sequence (stream char arg)
  (declare (ignore char arg))
  (let ((string (read stream t nil t)))
    (rna-sequence string)))

(named-readtables:defreadtable bio-reader
  (:merge :standard)
  (:macro-char #\@ :dispatch)
  (:dispatch-macro-char #\@ #\A #'%read-aa-sequence)
  (:dispatch-macro-char #\@ #\P #'%read-aa-sequence)
  (:dispatch-macro-char #\@ #\D #'%read-dna-sequence)
  (:dispatch-macro-char #\@ #\R #'%read-rna-sequence))

