
(in-package :entrez)

(declaim (optimize (debug 3)))
;;; starting with an xmls representation in memory we want to be able
;;; to specify a path to particular elements/sets of elements

;;; generic xml stuff -- maybe there's a better way to do this?

(defun element-type (element)
  (car element))

(defun attributes (element)
  (cadr element))

(defun content (element)
  (let ((content (cddr element)))
    (if (listp (car content))
        content
        (car content))))

(defun children (element &key type)
  (when (listp element)
    (if type
        (mapcan #'(lambda (x)
                    (when (and (listp x)
                               (equal (element-type x) type))
                      (list x)))
                (children element))
        (content element))))


(defun child (element &key type)
  (car (apply #'children element
              (when type `(:type ,type)))))

(defun walk-xmls-element-old (xmls path)
  (if (null path)
      xmls
      (let ((element-or-set
             (let ((tag (car path)))
               (cond ((eql tag :name)
                      (element-type xmls))
                     ((eql tag :attributes)
                      (attributes xmls))
                     ((eql tag :content)
                      (content xmls))
                     ((eql tag :children)
                      (let ((content-or-children (cddr xmls)))
                        (when (listp (car content-or-children))
                          content-or-children)))
                     ((eql tag :child)
                      (let ((content-or-children (cddr xmls)))
                        (when (listp (car content-or-children))
                          (car content-or-children))))
                     ((eql tag :rest)
                      (cddr xmls))
                     ((stringp tag)
                      (when (listp (children xmls))
                        (children xmls :type tag)))))))
        (if (and (listp element-or-set)
                 (listp (car element-or-set)))
            (list (element-type xmls)
                  (attributes xmls)
                  (walk-xmls-set element-or-set (cdr path)))
            :moose))))

(defun walk-xmls-set (xmls path)
  (mapcar (lambda (x)
            (print path)
            (walk-xmls-element x path))
          xmls))

(defun walk-xmls-element (xmls path)
  (if (null path)
      xmls
      (cond ((or (eql (car path) :wild)
                 (equal (element-type xmls) (car path)))
             (if (listp (children xmls))
                 (let ((results (mapcan
                                 (lambda (element)
                                   (let ((child (walk-xmls-element element (cdr path))))
                                     (when child (list child))))
                                 (children xmls))))
                   (when (car results)
                     (list* (element-type xmls)
                            (attributes xmls)
                            results)))
                 xmls)))))

;;; rewrite this to use stp via filter-children, etc...
;;; !!!
#+nil
(defun walk-stp-element (stp path)
  (if (null path)
      stp
      (cond ((or (eql (car path) :wild)
                 (equal (stp:local-name stp) (car path)))
             (if (listp (children stp))
                 (let ((results (mapcan
                                 (lambda (element)
                                   (let ((child (walk-stp-element element (cdr path))))
                                     (when child (list child))))
                                 (children stp))))
                   (when (car results)
                     (list* (element-type stp)
                            (attributes stp)
                            results)))
                 stp)))))

(defclass stp-path-node (stp:element)
  ((stp-node :accessor stp-node :initarg :stp-node)))

(defmethod stp:local-name ((node stp-path-node))
  (stp:local-name (stp-node node)))

(defun walk-stp-path (node path)
  (declare (optimize (debug 3)))
  (when (typep node 'cxml-stp:document)
    (setf node (stp:document-element node)))
  (cond ((null path)
         (make-instance 'stp-path-node :stp-node node))
        ((equal (car path)
                (stp:local-name node))
         (let ((path-node (make-instance 'stp-path-node :stp-node node)))
           (stp:do-children (child node)
             (when (typep child 'stp:element)
               (let ((child-path-node (walk-stp-path child (cdr path))))
                 (when child-path-node (stp:append-child path-node child-path-node)))))
           (unless (and (cdr path)
                        (zerop (stp:count-children nil path-node :test (lambda (&rest args) (values args)))))
             path-node)))))

(defun leaves (tree &aux x)
  (stp:map-recursively
   (lambda (path-node)
     (if (zerop (stp:count-children nil path-node :test (lambda (&rest args) (values args))))
         (let ((node (stp-node path-node)))
           (push (stp:data (stp:first-child node)) x))))
   tree)
  (nreverse x))


