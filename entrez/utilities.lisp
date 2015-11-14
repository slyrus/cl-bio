
(in-package :entrez)

#+nil 
(defun find-car (value list &key (test 'eql))
  (when (and list (listp list))
    (if (funcall test (car list) value)
        list
        (or (find-car value (car list) :test test)
            (find-car value (cdr list) :test test)))))

(defun find-sublists (list value &key (test 'eql))
  (let ((results))
    (subst-if nil
              (lambda (x)
                (and (listp x)
                     (funcall test (car x) value)
                     (push x results)))
              list)
    (nreverse results)))

#+nil
(defun escape-uri-query-parameter (param)
  (puri::encode-escaped-encoding
   param
   puri::*reserved-characters*
   t))

(defun make-query-string (query-list)
  (format nil "~{~{~A=~A~}~^&~}"
          (mapcar #'(lambda (x)
                      (mapcar #'(lambda (y)
                                  ;; we used to call
                                  ;; escape-uri-query-parameter
                                  ;; here. let's try without.
                                  (cond ((stringp y) y)
                                        ((numberp y)
                                         (format nil "~A" y))))
                              x))
                  query-list)))

(defun make-query-uri (query-list)
  (make-instance 'puri:uri :query (make-query-string query-list)))

;;; utility function for copying remote documents
(defun http-get-file (url file
                      &key
                        parameters
                        (if-exists :supersede)
                        (if-does-not-exist :create))
  (with-open-file (out file
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist if-does-not-exist)
    (let ((stream (drakma:http-request url :parameters parameters :want-stream t)))
      (loop for line = (read-line stream nil)
         while line
         do (write-line line out)))))

(defun stp-node->list (node)
  (typecase node
    (stp:element
     (list* (stp:local-name node)
            (stp:map-attributes 'list
                                (lambda (attr)
                                  (list (stp:local-name attr)
                                        (stp:value attr)))
                                node)
            (stp:map-children 'list #'stp-node->list node)))
    (stp:text
     (stp:string-value node))))

(defun stp-document->list (document)
  (stp-node->list (stp:document-element document)))

(defun string-if (string)
  (unless (equal "" string) string))

(defun string-value-if (node)
  (when (and node (or (not (xpath:node-set-p node))
                      (not (xpath:node-set-empty-p node))))
    (string-if (xpath:string-value node))))

(defun number-value-if (node)
  (when (and node (or (not (xpath:node-set-p node))
                      (not (xpath:node-set-empty-p node))))
    (xpath:number-value node)))

