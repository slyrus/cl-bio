
(in-package :entrez)

(defparameter *entrez-data-cache-directory*
  (ch-asdf:asdf-lookup-path "asdf:/cl-bio-entrez/cache/data"))
(ensure-directories-exist *entrez-data-cache-directory*)

(defparameter *entrez-search-cache-directory*
  (ch-asdf:asdf-lookup-path "asdf:/cl-bio-entrez/cache/search"))
(ensure-directories-exist *entrez-search-cache-directory*)

(defclass entrez-xml-dictionary (bio::dictionary) ())

(defparameter *entrez-xml-dictionary*
  (make-instance 'entrez-xml-dictionary))

(defmethod bio:lookup (object (dictionary entrez-xml-dictionary)
                       &key
                       (database "nucleotide")
                       (cache-results t)
                       (use-cache-for-lookup t)
                       builder
                       refresh
                       retstart
                       retmax)
  (let ((directory (merge-pathnames (make-pathname :directory (list :relative database))
                                    *entrez-search-cache-directory*)))
    (ensure-directories-exist directory)
    (let* ((file (merge-pathnames (make-pathname
                                   :name (format nil "~A-~A-~A" object retstart retmax)
                                   :type "xml") directory))
           (file-exists (probe-file file)))
      (if (and use-cache-for-lookup file-exists (not refresh))
          (with-open-file (stream file :element-type :default)
            (apply #'parse-entrez-xml-stream stream
                   (when builder `(:builder ,builder))))
          (let ((stream (apply #'get-entrez-search-stream
                               object
                               (append
                                (when cache-results `(:copy-to-file ,file))
                                (when database `(:database ,database))
                                (when retstart `(:retstart ,retstart))
                                (when retmax `(:retmax ,retmax))))))
            (apply #'parse-entrez-xml-stream stream
                   (when builder `(:builder ,builder))))))))

(defmethod bio:fetch (object (dictionary entrez-xml-dictionary)
                      &key
                      (database "nucleotide")
                      (cache-results t)
                      (use-cache-for-lookup t)
                      builder
                      refresh)
  (declare (optimize (debug 3)))
  (let ((directory (merge-pathnames (make-pathname :directory (list :relative database))
                                    *entrez-data-cache-directory*)))
    (ensure-directories-exist directory)
    (let* ((file (and (or cache-results use-cache-for-lookup)
                      (merge-pathnames (make-pathname
                                        :name (princ-to-string object) :type "xml")
                                       directory)))
           (file-exists (and file (probe-file file))))
      (if (and use-cache-for-lookup file-exists (not refresh))
          (with-open-file (stream file :element-type :default)
            (apply #'parse-entrez-xml-stream stream
                   (when builder `(:builder ,builder))))
          (let ((stream (apply #'get-entrez-stream
                               object
                               (append
                                (when cache-results `(:copy-to-file ,file))
                                (when database `(:database ,database))))))
            (apply #'parse-entrez-xml-stream stream
                   (when builder `(:builder ,builder))))))))



(defclass entrez-dictionary (bio::dictionary) ())

(defparameter *entrez-dictionary*
  (make-instance 'entrez-dictionary))

(defmethod bio:lookup (object (dictionary entrez-dictionary)
                       &key
                       database
                       use-cache-for-lookup
                       cache-results
                       refresh
                       retstart
                       retmax)
  (get-search-result-ids
   (apply #'bio:lookup object *entrez-xml-dictionary*
          (append
           (when database `(:database ,database))
           (when cache-results `(:cache-results ,cache-results))
           (when use-cache-for-lookup `(:use-cache-for-lookup ,use-cache-for-lookup))
           (when refresh `(:refresh ,refresh))
           (when retstart `(:retstart ,retstart))
           (when retmax `(:retmax ,retmax))))
   :id-class (cond 
               ((string-equal database"pubmed") 'bio::ncbi-pmid)
               (t 'bio:ncbi-gi))))

(defmethod bio:fetch (object (dictionary entrez-dictionary)
                                            &rest args
                      &key
                      database
                      (use-cache-for-lookup t use-cache-for-lookup-supplied-p)
                      (cache-results t cache-results-supplied-p)
                      refresh
                      retstart
                      retmax
                      )
  (declare (optimize (debug 3)))
  (let ((xml (apply #'bio:fetch object *entrez-xml-dictionary*
                    args
                    #+nil (append
                     (when database `(:database ,database))
                     (when cache-results `(:cache-results ,cache-results))
                     (when use-cache-for-lookup `(:use-cache-for-lookup ,use-cache-for-lookup))
                     (when refresh `(:refresh ,refresh))
                     (when retstart `(:retstart ,retstart))
                     (when retmax `(:retmax ,retmax))))))
    (let ((root (stp:root-element-name
                 (stp:document-type xml))))
      (cond ((equal root "Entrezgene-Set")
             (parse-entrez-gene-set xml))
            ((equal root "Entrezgene")
             (parse-entrez-gene xml))
            ((equal root "GBSet")
             (parse-gbset xml))
            ((equal root "GBSeq")
             (parse-gbseq xml))
            ((equal root "PubmedArticleSet")
             (parse-pubmed-article-set xml))
            (t (warn "returning raw xml")
               (print root)
               xml)))))
