
(in-package :cl-bio)

;;; find all taxa that start with "Canis" and return a list containing their
;;; rank (genus, species, etc...) and a list of (name . name-class) pairs
(with-bio-rucksack (rucksack)
  (mapcar
   #'(lambda (x)
       (let* ((id (tax-id x)))
         (cons (rank (get-tax-node id :rucksack rucksack))
               (mapcar #'(lambda (y)
                           (cons
                            (name y)
                            (name-class y)))
                       (get-tax-names id :rucksack rucksack)))))
   (lookup-tax-name "Canis" :rucksack rucksack)))

;;; show the path from the root of the tree to Drosophila melanogaster
(with-bio-rucksack (rucksack)
  (mapcar (lambda (id)
            (let ((node (get-tax-node id :rucksack rucksack)))
              (cons (rank node)
                    (get-preferred-tax-name id :rucksack rucksack))))
          (reverse
           (get-tax-node-ancestors
            (tax-id (car (lookup-tax-name "Drosophila melanogaster" :rucksack rucksack)))))))

;;; get the preferred tax-names of all of the children of Drosophila
(with-bio-rucksack (rucksack)
  (tree-map #'(lambda (x) (get-preferred-tax-name (tax-id x) :rucksack rucksack))
            (get-tax-node-descendents 7215 :rucksack rucksack)))
