
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

(defun parse-article (node)
  (declare (optimize (debug 3)))
  (xpath:with-namespaces ()
    (let ((citation (xpath:first-node (xpath:evaluate "MedlineCitation" node))))
      (let ((pmid
             (xpath:number-value
              (xpath:evaluate "PMID" citation))))
        (let ((article (xpath:first-node (xpath:evaluate "Article" citation))))
          (let ((article-title
                 (xpath:string-value
                  (xpath:evaluate "ArticleTitle" article)))
                (pages (xpath:string-value
                        (xpath:evaluate "Pagination/MedlinePgn" article)))
                (authors
                 (mapcar #'parse-author-node
                         (xpath:all-nodes (xpath:evaluate "AuthorList/Author" article))))
                (abstract (xpath:string-value
                           (xpath:evaluate "Abstract/AbstractText" article)))
                (journal-node (xpath:first-node (xpath:evaluate "Journal" article))))
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
                                            :authors authors
                                            :abstract abstract
                                            :pages pages
                                            :volume volume
                                            :issue issue
                                            :date (format nil "~A ~A ~A" year month day))))
                    obj))))))))))

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

