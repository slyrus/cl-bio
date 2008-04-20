
(in-package :entrez)

(defun parse-entrez-gene-rifs (node gene)
  (xpath:do-node-set
      (rif-node
       (xpath:evaluate
        (concatenate 'string
                     "Entrezgene_comments/Gene-commentary[Gene-commentary_type/attribute::value=\"generif\"]")
        node))
    (let ((text (xpath:string-value (xpath:evaluate "Gene-commentary_text" rif-node)))
          (refs-node (xpath:evaluate "Gene-commentary_refs" rif-node)))
      (unless (equal text "")
        (let ((rif (make-instance 'generif :generif-text text)))
          (xpath:do-node-set
              (ref refs-node)
            (let ((pmid (xpath::number-value (xpath:evaluate "Pub/Pub_pmid/PubMedId/text()"
                                                             ref))))
              (when pmid
                (let ((pub (make-instance 'pubmed-reference :pubmed-id pmid)))
                  (push pub (generif-references rif))))))
          (setf (generif-references rif)
                (nreverse (generif-references rif)))
          (bio:add-descriptor gene rif))))))

(defun parse-entrez-gene (node)
  (let ((obj (make-instance 'bio:gene)))
    (let ((gene-type
           (xpath:string-value
            (xpath:evaluate "Entrezgene_type/attribute::value" node))))
      (when gene-type (setf (bio:gene-type obj) gene-type)))
    (let ((tax-name
           (xpath:string-value
            (xpath:evaluate
             "Entrezgene_source/BioSource/BioSource_org/Org-ref/Org-ref_taxname/text()"
             node))))
      (when tax-name (setf (bio:gene-source obj) tax-name)))
    (let ((gene-ref-locus
           (xpath:string-value
            (xpath:evaluate "Entrezgene_gene/Gene-ref/Gene-ref_locus/text()"
                            node))))
      (when gene-ref-locus
        (bio:add-descriptor
         obj
         (make-instance 'locus-id :id gene-ref-locus :type "locus"))))
    (let ((gene-locus-products
           (xpath::map-node-set->list
            (lambda (commentary-node)
              (let ((type (xpath:string-value
                           (xpath:evaluate
                            "Gene-commentary_type/attribute::value"
                            commentary-node)))
                    (accession (xpath:string-value
                                (xpath:evaluate
                                 "Gene-commentary_accession/text()"
                                 commentary-node))))
                (when (and (equalp type "mRNA")
                           accession)
                  (let ((id
                         (make-instance 'bio:genbank-accession
                                        :accession accession))
                        (product
                         (make-instance 'bio::gene-product
                                        :type type)))
                    (bio:add-descriptor product id)
                    product))))
            (xpath:evaluate
             (concatenate 'string
                          "Entrezgene_locus"
                          "/Gene-commentary"
                          "/Gene-commentary_products"
                          "/Gene-commentary")
             node))))
      (when gene-locus-products
        (setf (bio:gene-products obj)
              (append (bio:gene-products obj)
                      gene-locus-products))))
    (parse-entrez-gene-rifs node obj)
    obj))

(defun parse-entrez-gene-set (node)
  (let ((set (make-instance 'bio:gene-set)))
    (let ((entrez-genes
           (xpath:all-nodes
            (xpath:evaluate "Entrezgene-Set/Entrezgene" node))))
      (setf (bio:genes set)
            (mapcar #'parse-entrez-gene entrez-genes)))
    set))


