
(in-package :cl-bio)

;;;
;;; identifier class

(defclass identifier ()
  ((id :accessor id :initarg :id)
   (type :accessor identity-type :initarg :type)
   (version :accessor version :initarg :version)
   (authority :accessor authority :initarg :authority)))

;;;
;;; identifier subclasses

(defclass ncbi-gi (identifier)
  ((id :accessor id :initarg :id :initarg :gi)
   (type :accessor identity-type :initarg :type :initform "gi")
   (authority :accessor authority :initarg :authority :initform "ncbi")))

(defclass genbank-accession (identifier)
  ((id :accessor id :initarg :id :initarg :accession)
   (type :accessor identity-type :initarg :type :initform "accession")
   (authority :accessor authority :initarg :authority :initform "genbank")))

(defclass affymetrix-probe-set-id (identifier)
  ((id :accessor id :initarg :id :initarg :accession)
   (type :accessor identity-type :initarg :type :initform "probe-set-id")
   (authority :accessor authority :initarg :authority :initform "affymetrix")))

(defclass flybase-identifier (identifier)
  ((id :accessor id :initarg :id :initarg :flybase-id)
   (authority :accessor authority :initarg :authority :initform "flybase")))

(defclass flybase-gene-identifier (flybase-identifier)
  ((type :accessor identity-type :initarg :type :initform "gene-identifier")))
