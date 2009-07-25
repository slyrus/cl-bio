
(in-package :entrez)

(defun parse-author-node (node)
  (let ((last-name (xpath:string-value
                    (xpath:evaluate "LastName" node)))
        (fore-name (xpath:string-value
                    (xpath:evaluate "ForeName" node)))
        (initials (xpath:string-value
                   (xpath:evaluate "Initials" node))))
    (apply #'make-instance 'bio:author
           :last-name last-name
           (append
            (when fore-name `(:fore-name ,fore-name))
            (when initials `(:initials ,initials))))))

(defun fix-pages (medline-pages)
  (destructuring-bind (start partial-end)
      (ppcre:split "-" medline-pages)
    (let ((end (copy-seq start)))
      (let ((pos (- (length start) (length partial-end))))
        (format nil "~A-~A" start (replace end partial-end :start1 pos))))))

(defun parse-article (node)
  (declare (optimize (debug 3)))
  (xpath:with-namespaces ()
    (let ((citation (xpath:first-node (xpath:evaluate "MedlineCitation" node)))
          (doi (xpath:string-value
                (xpath:first-node
                 (xpath:evaluate "PubmedData/ArticleIdList/ArticleId[attribute::IdType=\"doi\"]"
                                 node)))))
      (let ((pmid
             (xpath:number-value
              (xpath:evaluate "PMID" citation)))
            (article (xpath:first-node (xpath:evaluate "Article" citation)))
            (mesh-headings
             (xpath:map-node-set->list
              #'xpath:string-value
              (xpath:evaluate "MeshHeadingList/MeshHeading/DescriptorName" citation)))
            
            (short-journal-title
             (xpath:string-value (xpath:evaluate "MedlineJournalInfo/MedlineTA" citation))))
        (let ((article-title
               (xpath:string-value
                (xpath:evaluate "ArticleTitle" article)))
              (pages
               (string-value-if
                (xpath:evaluate "Pagination/MedlinePgn" article)))
              (authors
               (mapcar #'parse-author-node
                       (xpath:all-nodes (xpath:evaluate "AuthorList/Author" article))))
              (abstract (xpath:string-value
                         (xpath:evaluate "Abstract/AbstractText" article)))
              (affiliation (xpath:string-value
                            (xpath:evaluate "Affiliation" article)))
              (journal-node
               (xpath:first-node (xpath:evaluate "Journal" article))))
          (let ((journal-title
                 (xpath:string-value
                  (xpath:evaluate "Title" journal-node)))
                (journal-issue-node (xpath:first-node (xpath:evaluate "JournalIssue" journal-node))))
            (let ((pub-date-node (xpath:first-node
                                  (xpath:evaluate "PubDate" journal-issue-node)))
                  (volume (xpath:string-value
                           (xpath:evaluate "Volume" journal-issue-node)))
                  (issue (xpath:string-value
                          (xpath:evaluate "Issue" journal-issue-node))))
              (let ((year (xpath:number-value
                           (xpath:evaluate "Year" pub-date-node)))
                    (month (xpath:string-value
                            (xpath:evaluate "Month" pub-date-node)))
                    (day (xpath:number-value
                          (xpath:evaluate "Day" pub-date-node))))
                (let ((obj (make-instance 'bio:article
                                          :pmid pmid
                                          :title article-title
                                          :journal journal-title
                                          :short-journal short-journal-title
                                          :authors authors
                                          :abstract abstract
                                          :affiliation affiliation
                                          :pages (when pages (fix-pages  pages))
                                          :volume volume
                                          :issue issue
                                          :date (list year month day)
                                          :mesh-headings mesh-headings
                                          :doi doi)))
                  obj)))))))))

(defun parse-pubmed-article-set (node)
  (declare (optimize (debug 3)))
  (xpath:with-namespaces ()
    (let ((set (make-instance 'bio:article-set)))
      (let ((articles
             (xpath:all-nodes
              (xpath:evaluate "PubmedArticleSet/PubmedArticle" node))))
        (setf (bio:article-set-articles set)
              (mapcar #'parse-article articles)))
      set)))

