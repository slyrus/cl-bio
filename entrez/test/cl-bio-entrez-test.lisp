
(in-package :entrez-test)

#|

(defparameter *seq* (entrez-fetch 5))

(defparameter *dpp-search* (entrez-search "dpp" :database "gene"))

(defparameter *dpp* (entrez-fetch (cadr (car (children (car (children *dpp-search* :type :|IdList|)))))
                                  :database "gene"))

(attribute
 (attributes
  (car (children
        (cadr (children
               (car (children
                     (car (children *dpp*))
                     :type :|Entrezgene_locus|))
               :type :|Gene-commentary|))
        :type :|Gene-commentary_type|)))
 :|value|)


(children
        (cadr (children
               (car (children
                     (car (children *dpp*))
                     :type :|Entrezgene_locus|))
               :type :|Gene-commentary|))
        :type :|Gene-commentary_type|)

(mapcar #'(lambda (x)
            (cadr (car (children x :type :|Gene-commentary_accession|))))
        (children
         (car
          (children
           (car
            (member-if #'(lambda (x)
                           (equal (attribute (attributes (car (children x :type :|Gene-commentary_type|)))
                                             :|value|)
                                  "genomic"))
                       (children
                        (car (children
                              (car (children *dpp*))
                              :type :|Entrezgene_locus|))
                        :type :|Gene-commentary|)))
           :type :|Gene-commentary_products|))))


(mapcar #'(lambda (x)
            (car (children (car (children x :type :|Gene-commentary_genomic-coords|)))))
        (children
         (car
          (children
           (car
            (member-if #'(lambda (x)
                           (equal (attribute (attributes (car (children x :type :|Gene-commentary_type|)))
                                             :|value|)
                                  "genomic"))
                       (children
                        (car (children
                              (car (children *dpp*))
                              :type :|Entrezgene_locus|))
                        :type :|Gene-commentary|)))
           :type :|Gene-commentary_products|))))


(children
 (car (children
       (car (children *dpp*))
       :type :|Entrezgene_locus|))
 :type :|Gene-commentary|)

(describe (entrez::parse-seq-id (cadr (entrez::child (cadr (cadr (cadr (cadr (cadr (cadr q)))))) :type :|Seq-interval_id|))))

(defparameter foo
  (car (mapcar #'(lambda (x)
            (car (children (car (children x :type :|Gene-commentary_genomic-coords|)))))
        (children
         (car
          (children
           (car
            (member-if #'(lambda (x)
                           (equal (attribute (attributes (car (children x :type :|Gene-commentary_type|)))
                                             :|value|)
                                  "genomic"))
                       (children
                        (car (children
                              (car (children *dpp*))
                              :type :|Entrezgene_locus|))
                        :type :|Gene-commentary|)))
           :type :|Gene-commentary_products|))))))

(mapcar #'(lambda (x) (list (entrez::from x) (entrez::to x))) (mapcar #'entrez::seq-int (entrez::seq-locs (entrez::parse-seq-loc foo))))

|#
