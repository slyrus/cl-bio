
(in-package :bio)

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
   (lookup-tax-name "Canis" :partial t :rucksack rucksack)))

;;; show the path from the root of the tree to Drosophila melanogaster
(with-bio-rucksack (rucksack)
  (mapcar (lambda (id)
            (let ((node (get-tax-node id :rucksack rucksack)))
              (cons (rank node)
                    (get-preferred-tax-name id :rucksack rucksack))))
          (reverse
           (get-tax-node-ancestors
            (tax-id (car (lookup-tax-name "Drosophila melanogaster" :rucksack rucksack)))))))

;;; show the path from the root of the tree to Homo sapiens
(with-bio-rucksack (rucksack)
  (mapcar (lambda (id)
            (let ((node (get-tax-node id :rucksack rucksack)))
              (cons (rank node)
                    (get-preferred-tax-name id :rucksack rucksack))))
          (reverse
           (get-tax-node-ancestors
            (tax-id (car (lookup-tax-name "Homo sapiens" :rucksack rucksack)))))))

;;; get the preferred tax-names of all of the children of Drosophila
(with-bio-rucksack (rucksack)
  (tree-map #'(lambda (x) (get-preferred-tax-name (tax-id x) :rucksack rucksack))
            (get-tax-node-descendents
             (tax-id (find "genus"
                           (lookup-tax-name "Drosophila" :rucksack rucksack)
                           :test 'equal
                           :key (lambda (x)
                                  (rank (get-tax-node (tax-id x))))))
             :rucksack rucksack)))

;;; get the preferred tax-names of all of the children of Primates
(with-bio-rucksack (rucksack)
  (tree-map #'(lambda (x) (get-preferred-tax-name (tax-id x) :rucksack rucksack))
            (get-tax-node-descendents
             (tax-id (find "order"
                           (lookup-tax-name "Primates" :rucksack rucksack)
                           :test 'equal
                           :key (lambda (x)
                                  (rank (get-tax-node (tax-id x))))))
             :rucksack rucksack)))

