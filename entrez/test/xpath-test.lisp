
(in-package :entrez-test)

(defun test-xpath (node path)
  (let* ((parsed-path (xpath:parse-xpath path))
         (compiled-path (xpath:compile-xpath parsed-path (entrez::make-entrez-environment))))
    (funcall compiled-path (xpath::make-context node))))

(defparameter *test-dom-1*
  (cxml:parse-rod
   (concatenate
    'string
    "<Gene>"
    "<Product>"
    "<Name>This is great!</Name>"
    "</Product>"
    "<Product>"
    "<Name>So is this!</Name>"
    "</Product>"
    "</Gene>")
   (cxml-dom:make-dom-builder)))

(defparameter *test-dom-2*
  (cxml:parse-rod
   (concatenate
    'string
    "<Gene>"
    "<Product>"
    "<Name>This is lame!</Name>"
    "</Product>"
    "<Product>"
    "</Product>"
    "</Gene>")
   (cxml-dom:make-dom-builder)))

(defparameter *test-dom-3*
  (cxml:parse-rod
   (concatenate
    'string
    "<Gene>"
    "<Product>"
    "</Product>"
    "<Product>"
    "<Name>This is bogus!</Name>"
    "</Product>"
    "</Gene>")
   (cxml-dom:make-dom-builder)))

(defparameter *test-dom-4*
  (cxml:parse-rod
   (concatenate
    'string
    "<Gene>"
    "<Product value=\"mRNA\">"
    "<Name>This is bogus!</Name>"
    "</Product>"
    "</Gene>")
   (cxml-dom:make-dom-builder)))

(defun join-xpath-result (result)
  (if (xpath::node-set-p result)
      (format nil "~{~a~^|||~}"
              (mapcar #'(lambda (x)
                          (when x
                            (xpath::get-node-text x)))
                      (xpath::force (xpath::pipe-of result))))
      (xpath:string-value result)))

(join-xpath-result (test-xpath *test-dom-1* "Gene/Product/Name"))
(join-xpath-result (test-xpath *test-dom-1* "Gene/Product[2]/Name"))
(join-xpath-result (test-xpath *test-dom-2* "Gene/Product/Name"))
(join-xpath-result (test-xpath *test-dom-3* "Gene/Product/Name"))
(join-xpath-result (test-xpath *test-dom-4* "Gene/Product/attribute::*"))
(join-xpath-result (test-xpath *test-dom-4* "Gene/Product/attribute::value"))
