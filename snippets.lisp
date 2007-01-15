
(in-package :cl-bio)

;;; find all taxa that start with "Canis" and return a list containing their
;;; rank (genus, species, etc...) and a list of (name . name-class) pairs
(mapcar
 #'(lambda (x)
     (let* ((id (tax-id x)))
       (cons (rank (get-tax-node id))
             (mapcar #'(lambda (y)
                         (cons
                          (name y)
                          (name-class y)))
                     (get-tax-names id)))))
 (lookup-tax-name "Canis"))

;;; show the path from the root of the tree to Drosophila melanogaster
(mapcar (lambda (id)
          (let ((node (get-tax-node id)))
            (cons (rank node)
                  (get-preferred-tax-name id))))
        (reverse
         (get-tax-node-ancestors
          (tax-id (car (lookup-tax-name "Drosophila melanogaster"))))))

(defun tree-print (x)
  (if (atom x)
      (when x (print x))
      (progn (tree-print (car x))
             (tree-print (cdr x)))))

(mapcar #'(lambda (x)
            (mapcar #'(lambda (y)
                        (cond ((listp y)
                               (print y)
                               (mapcar #'tax-id y))
                              (t (tax-id y))))
                    x))
        (mapcar #'identity (mapcar #'cdr (get-tax-node-descendents 32346))))

