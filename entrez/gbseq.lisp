 
(in-package :entrez)

(defparameter *feature-annotation-type-list*
  '(("exon" . bio:exon)
    ("cds" . bio::cds)
    ("STS" . bio::sts)
    ("repeat_region" . bio::repeat-region)))

(defparameter *feature-annotation-type-hash-table*
  (make-hash-table :test 'equalp))

(mapcar
 (lambda (x)
   (destructuring-bind (feature-type . class)
       x
     (setf (gethash feature-type
                    *feature-annotation-type-hash-table*)
           (find-class class))))
 *feature-annotation-type-list*)

(defun feature-annotation-type (type)
  (let ((class (gethash type *feature-annotation-type-hash-table*)))
    (if class
        class
        (warn "unknown feature type ~S" type))))

(defun get-gbseq-feature-nodes (node &key type)
  (xpath:with-namespaces ()
    (xpath:evaluate
     (format nil
             "GBSeq_feature-table/GBFeature[GBFeature_key~@[/text()=\"~A\"~]]"
             type)
     node)))

(defun get-gbseq-feature-type (node)
  (xpath:with-namespaces ()
    (xpath:string-value
     (xpath:evaluate "GBFeature_key/text()" node))))

(defun get-gbseq-feature-ranges (node)
  (xpath:with-namespaces ()
    (mapcar
     (lambda (interval)
       (let ((from (xpath::number-value (xpath:evaluate "GBInterval_from/text()" interval)))
             (to (xpath::number-value (xpath:evaluate "GBInterval_to/text()" interval))))
         (unless (or (eql from :nan)
                     (eql to :nan))
           (let ((from (1- from))
                 (to (1- to)))
             (let ((alpha-range (make-instance 'bio:range :start from :end to))
                   (beta-range (make-instance 'bio:range :start 0 :end (- to from))))
               (cons alpha-range beta-range))))))
     (xpath:all-nodes
      (xpath:evaluate "GBFeature_intervals/GBInterval" node)))))

(defun get-gbseq-feature-types (node)
  (xpath:with-namespaces ()
    (xpath:evaluate
     "GBSeq_feature-table/GBFeature/GBFeature_key/text()"
     node)))

(defun parse-gbseq (node)
  (xpath:with-namespaces ()
    (let ((moltype
           (xpath:string-value
            (xpath:evaluate "GBSeq_moltype/text()" node))))
      (let ((obj (make-instance
                  (cond
                    ((member moltype '("mRNA" "tRNA" "RNA") :test 'equal) 'bio:adjustable-rna-sequence)
                    ((equal moltype "DNA") 'bio:adjustable-dna-sequence)
                    ((equal moltype "AA") 'bio:adjustable-aa-sequence)
                    (t 'bio:simple-sequence)))))
        (let ((gbseq-locus
               (xpath:string-value
                (xpath:evaluate "GBSeq_locus/text()" node))))
          (when gbseq-locus
            (bio:add-descriptor
             obj
             (make-instance 'bio:identifier :id gbseq-locus :type "locus"))))
        (let ((gbseq-sequence
               (xpath:string-value
                (xpath:evaluate "GBSeq_sequence/text()" node))))
          (when gbseq-sequence
            (setf (bio:residues-string obj) gbseq-sequence)))
        (let ((feature-nodes (get-gbseq-feature-nodes node)))
          (xpath:map-node-set
           (lambda (feat)
             (let ((feature-class
                    (feature-annotation-type
                     (get-gbseq-feature-type feat)))
                   (feature-ranges (get-gbseq-feature-ranges feat)))
               (when (and feature-class feature-ranges)
                 (mapcar
                  (lambda (range-pair)
                    (destructuring-bind (alpha-range . beta-range)
                        range-pair
                      (let* ((annot (make-instance
                                     feature-class
                                     :length (bio::range-end beta-range)))
                             (align (make-instance
                                     'bio:simple-pairwise-alignment
                                     :alpha-sequence obj :alpha-range alpha-range
                                     :beta-sequence annot :beta-range beta-range)))
                        (push align (bio:annotations obj)))))
                  feature-ranges))))
           feature-nodes))
        obj))))

(defun parse-gbset (node)
  (xpath:with-namespaces ()
    (let ((set (make-instance 'bio:gene-set)))
      (let ((gb-seqs
             (xpath:all-nodes
              (xpath:evaluate "GBSet/GBSeq" node))))
        (setf (bio:genes set)
              (mapcar #'parse-gbseq gb-seqs)))
      set)))
