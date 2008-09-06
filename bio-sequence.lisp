;;; Bio-sequence
;;; Classes, generic functions, methods and functions for working
;;; with biological sequences
;;;
;;; Copyright (c) 2006-2008 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(in-package :bio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Biological sequences, such as nucleic acid and
;;; protein sequences.

;;; biological sequence protocol class
(defclass bio-sequence (bio-object)
  ()
  (:documentation "A protocol class for representing biological
sequences. It is intended that appropriate subclasses will represent
distinct sequences such as nucleic acid and protein sequences."))

;;; biological sequence-set protocol class
(defclass bio-sequence-set (bio-set)
  ((members :initarg :bio-sequences :accessor bio-sequences)))

(defgeneric seq-length (seq)
  (:documentation "Returns the length of a bio-sequence. Subclasses of
bio-sequence are free to use arbitrary units for the length, although
it is expected that sequences with residues will return the number
of residues as the length."))

(defclass sequence-range (bio-object)
  ((sequence :accessor sequence-range-sequence :initarg sequence)
   (range :accessor sequence-range-range :initarg range))
  (:documentation "A defined spatial extent of a given seqeunce."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Alignments. Alignments represent relationships between sequences

(defclass alignment (bio-object) 
  ()
  (:documentation "An alignment represents a relationship between
  sequences."))

(defclass simple-pairwise-alignment (alignment)
  ((alpha-sequence :accessor alpha-sequence :initarg :alpha-sequence)
   (alpha-range :accessor alpha-range :initarg :alpha-range)
   (beta-sequence :accessor beta-sequence :initarg :beta-sequence)
   (beta-range :accessor beta-range :initarg :beta-range))
  (:documentation "A simple pairwise alignment between portions of two
  sequences."))

(defun filter-alignments (list type)
  (remove-if-not
   (lambda (x)
     (etypecase x
       (simple-pairwise-alignment
        (subtypep (class-name (class-of (beta-sequence x))) type))))
   list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Sequence with residues. Anticipating the need for sequences
;;; without residues, we explicitly define a sequence-with-reisidues
;;; class and define generic methods that are intended to be
;;; associated with these classes, as well as simple reference
;;; implementation of these functions.

;;; Protocol class for biological sequences having residues
(defclass sequence-with-residues ()
  ((residues :accessor residues))
  (:documentation "The protocol class for sequences which have
residues (or other individually linearly-addressable subunits)
associated with them."))

(defgeneric residues-string (seq)
  (:documentation "Returns all of the residues of the sequence as a
single string."))

(defgeneric (setf residues-string) (val seq)
  (:documentation "Sets the residues of the sequence to be the residues
contained in the string. The sequence may or may not modify the
string or use the provided string as the storage for the reisdues."))1

(defgeneric residues-string-range (seq range)
  (:documentation "Returns the residues of the sequence in a particular
range."))

(defgeneric residue (seq i)
  (:documentation "Returns the residue of seq at position i. Note
that for residue-coded sequences this is the external character
representation of the residues, not the internal integer
representation."))

(defgeneric (setf residue) (val seq i)
  (:documentation "Sets the ith residue of seq to val."))

(defgeneric seq-reverse (seq)
  (:documentation "Returns a new instance of the same class as seq
whose residues have been reversed (AACCGT -> TGCCAA)"))


;;; some reference implementations for
;;; sequence-with-residues. subclasses are free to override these as
;;; they see fit
(defmethod seq-length ((seq sequence-with-residues))
  (length (residues seq)))

(defmethod residues-string ((seq sequence-with-residues))
  (let ((str (make-string (seq-length seq))))
    (loop for i from 0 below (seq-length seq)
       do (setf (elt str i) (residue seq i)))
    str))

(defmethod (setf residues-string) (residues (seq sequence-with-residues))
  (loop for res across residues
       for i from 0
       do (setf (residue seq i) (elt residues i)))
  residues)

(defmethod residues-string-range ((seq sequence-with-residues) (range range))
  (let ((str (make-string (range-length range))))
    (loop
       for i from (range-start range) below (range-end range)
       for j from 0
       do (setf (elt str j) (residue seq i)))
    str))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Annotated sequences. Sequences that can have annotations attached
;;; to them.

(defclass annotated-sequence (bio-sequence)
  ((annotations :accessor annotations
                :initarg :annotations
                :initform nil))
  (:documentation "A protocol class for sequences that can have
  annotations attached to them."))

(defgeneric get-annotations (bio-sequence &key type))

(defmethod get-annotations ((seq bio-sequence) &key type)
  (if type
      (remove-if-not
       (lambda (x)
         (subtypep (class-name (class-of x)) type))
       (annotations seq))
      (annotations seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Residue codes. Rather than just using characters to represent the
;;; residues of sequences, we allow for an encoding of a residue as an
;;; integer. The protocol class sequence-with-residue-codes is to be a
;;; superclass of classes that use residue codes and support the
;;; residue-code method and its friends.

;;; seuqence with residue codes protocol class
(defclass sequence-with-residue-codes (sequence-with-residues)
  ()
  (:documentation "Protocol class for sequences with residues that
store the value of the residue as a residue code, rather than as a
character."))

(defgeneric residue-code (seq i)
  (:documentation "Returns the residue code value (the internal
integer representation) of the ith residue of seq."))

(defgeneric (setf residue-code) (val seq i)
  (:documentation "Sets the internal interger representation of the
ith residue of seq to the residue code val."))

(defmethod seq-reverse ((forward sequence-with-residue-codes))
  (let* ((length (seq-length forward))
         (rev (make-instance (class-of forward) :length length)))
    (loop for i below length
       for j from (1- length) downto 0
       do (setf (residue-code rev i) (residue-code forward j)))
    rev)
  
  ;; the following is a slightly less efficient, if simpler way of
  ;; doing the same thing:
  #+nil (make-instance (class-of forward)
                       :initial-contents (reverse (residues-string forward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple sequences. 

;;; simple sequence protocol class
(defclass simple-sequence (sequence-with-residue-codes
                           annotated-sequence) 
  ()
  (:documentation "A simple sequence is a sequence that extends
 sequence-with-residue-codes (and therefore sequence-with-residues and
 bio-sequence) that stores its residues using an array. Simple
 operations such as getting and setting residue values are supported,
 but more involved operations such as inesrtion and deletion are
 not."))

(defmethod shared-initialize :after ((seq simple-sequence)
                                     slot-names
                                     &rest initargs
                                     &key length initial-contents)
  (declare (ignore slot-names initargs))
  (let ((element-type (slot-value seq 'element-type))
        (length (or (and initial-contents
                         (length initial-contents))
                    length)))
    (when element-type
      (setf (slot-value seq 'element-type)
            (upgraded-array-element-type element-type)))
    (setf (residues seq)
          (apply #'make-array length
                 (when element-type `(:element-type ,element-type)))))
  (when initial-contents
    (setf (residues-string seq) initial-contents)))

(defmethod residue ((seq simple-sequence) i)
  (seq-code-to-char seq (aref (residues seq) i)))

(defmethod (setf residue) (val (seq simple-sequence) i)
  (setf (aref (residues seq) i)
        (char-to-seq-code seq val)))

(defmethod residue-code ((seq simple-sequence) i)
  (aref (residues seq) i))

(defmethod (setf residue-code) (val (seq simple-sequence) i)
  (setf (aref (residues seq) i) val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adjustable sequences. 

;;; adjustable sequence protocol class
(defclass adjustable-sequence (sequence-with-residue-codes)
  ()
  (:documentation "An adjustable sequence is a sequence that supports
 operations for insertion, deletion and appending of residues"))

(defgeneric insert-residue (seq pos res)
  (:documentation "Insert reside res at position pos in sequence
  seq."))

(defgeneric insert-residues (seq pos str)
  (:documentation "Insert the residues corresponding the characters in
  str at position pos in sequence seq."))

(defgeneric insert-residue-codes (seq pos vec)
  (:documentation "Insert the residue codes contained in vec at
  position pos in sequence seq."))

(defgeneric append-residue (seq res)
  (:documentation "Append the residue res to the and of seq."))

(defgeneric append-residues (seq str)
  (:documentation "Append the residues corresponding to the characters
  in str to the end of seq."))

(defgeneric append-residue-codes (seq vec)
  (:documentation "Append the the residue codes contained in vec to
  the end of seq."))

(defgeneric delete-residue (seq pos)
  (:documentation "Delete the residue at position pos in seq."))

(defgeneric delete-residues (seq pos count)
  (:documentation "Delete count residues starting at position pos in
  seq."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flexichain Sequences. Flexichain sequences are adjustable
;;; sequences that ues a flexichain as the storage for the residues.

;;; flexichain sequences class
(defclass flexichain-sequence (adjustable-sequence annotated-sequence)
  ((initial-element :initform 0))
  (:documentation "Implementation class for sequences that use
flexichains to store their residues."))

(defun make-residues-flexichain (seq
                                 &key
                                 length
                                 initial-contents
                                 initial-residue-codes
                                 initial-element
                                 element-type)
  (apply #'make-instance 'flexichain:standard-flexichain
                 :initial-element (or initial-element (slot-value seq 'initial-element))
                 (append (when length `(:initial-nb-elements ,length))
                         (if initial-residue-codes
                             `(:initial-contents ,initial-residue-codes)
                             (when initial-contents `(:initial-contents
                                                      ,(map 'vector
                                                            #'(lambda (c)
                                                                (char-to-seq-code seq c))
                                                            initial-contents))))
                         (when length `(:initial-nb-elements ,length))
                         (when element-type `(:element-type ,element-type)))))

(defmethod shared-initialize :after ((seq flexichain-sequence)
                                     slot-names
                                     &rest initargs
                                     &key
                                     length
                                     initial-contents
                                     initial-residue-codes
                                     initial-element)
  (declare (ignore slot-names initargs))
  (let ((element-type (slot-value seq 'element-type))
        (length (or (and initial-contents
                         (length initial-contents))
                    length)))
    (setf (residues seq)
          (apply #'make-residues-flexichain
                 seq
                 (append
                  (when length `(:length ,length))
                  (when initial-contents `(:initial-contents ,initial-contents))
                  (when initial-residue-codes `(:initial-residue-codes ,initial-residue-codes))
                  (when initial-element `(:initial-element ,initial-element))
                  (when element-type `(:element-type ,element-type)))))))

(defmethod seq-length ((seq flexichain-sequence))
  (flexichain:nb-elements (residues seq)))

(defmethod (setf residues-string) (residues (seq flexichain-sequence))
  (setf (residues seq)
        (make-residues-flexichain seq :initial-contents residues))
  residues)

(defmethod residue ((seq flexichain-sequence) i)
  (seq-code-to-char seq (flexichain:element* (residues seq) i)))

(defmethod (setf residue) (val (seq flexichain-sequence) i)
  (setf (flexichain:element* (residues seq) i)
        (char-to-seq-code seq val)))

(defmethod residue-code ((seq flexichain-sequence) i)
  (flexichain:element* (residues seq) i))

(defmethod (setf residue-code) (val (seq flexichain-sequence) i)
  (setf (flexichain:element* (residues seq) i) val))

(defmethod insert-residue ((seq flexichain-sequence) pos res)
  (flexichain:insert* (residues seq)
                      pos
                      (char-to-seq-code seq res)))

(defmethod insert-residues ((seq flexichain-sequence) pos str)
  (flexichain:insert-vector* (residues seq)
                             pos
                             (map (if (slot-value seq 'element-type)
                                      `(vector ,(slot-value seq 'element-type))
                                      'vector)
                                  #'(lambda (x)
                                      (char-to-seq-code seq x))
                                  str)))

(defmethod insert-residue-codes ((seq flexichain-sequence) pos vec)
  (flexichain:insert-vector* (residues seq) pos vec))


(defmethod append-residue ((seq flexichain-sequence) res)
  (flexichain:push-end (residues seq)
                       (char-to-seq-code seq res)))

(defmethod append-residues ((seq flexichain-sequence) str)
  (flexichain:insert-vector* (residues seq)
                             (seq-length seq)
                             (map (if (slot-value seq 'element-type)
                                      `(vector ,(slot-value seq 'element-type))
                                      'vector)
                                  #'(lambda (x)
                                      (char-to-seq-code seq x))
                                  str)))

(defmethod append-residue-codes ((seq flexichain-sequence) vec)
  (flexichain:insert-vector* (residues seq) (seq-length seq) vec))

(defmethod delete-residue ((seq flexichain-sequence) pos)
  (flexichain:delete* (residues seq) pos))

(defmethod delete-residues ((seq flexichain-sequence) pos count)
  (loop for i below count
     do (flexichain:delete* (residues seq) pos)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences for handling specific size residues

;;; 2-bit sequence protocol class
(defclass 2-bit-sequence (sequence-with-residues)
  ((element-type :initform '(unsigned-byte 2) :allocation :class))
  (:documentation "A sequence with a 2-bit residue encoding"))

(defclass 4-bit-sequence (sequence-with-residues)
  ((element-type :initform '(unsigned-byte 4) :allocation :class))
  (:documentation "A sequence with a 4-bit residue encoding"))

;;; 5-bit sequence protocol class
(defclass 5-bit-sequence (sequence-with-residues)
  ((element-type :initform '(unsigned-byte 5) :allocation :class))
  (:documentation "A sequence with a 5-bit residue encoding"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sequences for handling specific types of biomolecules, such as
;;; nucleic acid, DNA, RNA, or amino acids (proteins).

;;; nucleic acid sequence protocol class
(defclass na-sequence (bio-sequence)
  ()
  (:documentation "Nucleic acid sequence protocol class"))

(defclass na-sequence-with-residues (na-sequence
                                     sequence-with-residues) 
  ()
  (:documentation "Nucleic acid sequence with residues protocol class."))

(defclass simple-na-sequence (simple-sequence
                              na-sequence-with-residues)
  ()
  (:documentation "Simple sequence for nucleic acids."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for DNA sequences

;;; DNA sequence protocol class
(defclass dna-sequence (na-sequence)
  ()
  (:documentation "DNA sequence protocol class"))

(defclass dna-sequence-with-residues (dna-sequence
                                      na-sequence-with-residues)
  ()
  (:documentation "DNA sequence with residues protocol class."))

;;; 2-bit DNA sequence protocol class
(defclass 2-bit-dna-sequence (dna-sequence
                              2-bit-sequence
                              2-bit-dna-sequence-encoding)
  ()
  (:documentation "Protocol class for representing DNA sequences with
a 2-bit encoding corresponding to A, C, G, T."))

;;; 2-bit DNA sequence protocol class
(defclass 4-bit-dna-sequence (dna-sequence
                              4-bit-sequence
                              4-bit-dna-sequence-encoding)
  ()
  (:documentation "Protocol class for representing DNA sequences with
a 4-bit encoding, allowing not just ACGT but IUPAC ambiguity codes as
well."))

(defgeneric reverse-complement (seq)
  (:documentation "Returns a new sequence that is the reverse
complement of seq."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for DNA sequences

;;; simple DNA sequence class
(defclass simple-dna-sequence (simple-na-sequence
                               dna-sequence-with-residues
                               2-bit-dna-sequence)
  ()
  (:documentation "A DNA sequence that is a simple-sequence and which
uses a 2-bit sequence encoding."))

;;; adjustable DNA sequence class
(defclass adjustable-dna-sequence (2-bit-dna-sequence
                                   flexichain-sequence
                                   dna-sequence-with-residues)
  ()
  (:documentation "An adjustable DNA sequence using a
2-bit-dna-sequence, implemented using the flexichain-sequence."))

(defclass 4-bit-adjustable-dna-sequence (4-bit-dna-sequence
                                         flexichain-sequence
                                         dna-sequence-with-residues)
  ()
  (:documentation "A 4-bit adjustable"))

(defun make-simple-dna-sequence (length)
  "Returns a simple-dna-sequence of the specified length."
  (make-instance 'simple-dna-sequence :length length))

(defun make-adjustable-dna-sequence (length)
  "Returns an adjustable-dna-sequence of the specified length."
  (make-instance 'adjustable-dna-sequence :length length))

(defun make-dna-sequence-from-string (residues)
  "Returns a simple-dna-sequence initialized with residues as its
contents."
  (make-instance 'simple-dna-sequence :initial-contents residues))

(defun make-random-dna-sequence (length)
  "Returns a simple-dna-sequence of the specified length whose
contents are randomly chosen residues."
  (let* ((dna (make-simple-dna-sequence length)))
    (let ((k 4))
      (loop for i below length
         do (setf (residue-code dna i) (random k))))
    dna))

;;; TODO: write reverse-complement for 4-bit-dna-sequences!
(defmethod reverse-complement ((forward 2-bit-dna-sequence))
  (let* ((length (seq-length forward))
         (revcomp (make-instance (class-of forward) :length length)))
    (loop for i below length
       for j from (1- length) downto 0
       do (setf (residue-code revcomp i) (- 3 (residue-code forward j))))
    revcomp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for RNA sequences

;;; RNA sequence protocol class
(defclass rna-sequence (na-sequence)
  ()
  (:documentation "RNA seqeunce protocol class."))

(defclass rna-sequence-with-residues (rna-sequence
                                      na-sequence-with-residues)
  ()
  (:documentation "RNA sequence with residues protocol class."))

;;; 2-bit RNA sequence protocol class
(defclass 2-bit-rna-sequence (rna-sequence 2-bit-sequence
                                           2-bit-rna-sequence-encoding)
  ()
  (:documentation "Protocol class for representing RNA sequnences
with a 2-bit representation for A, C, G and U."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for RNA sequences

;;; simple RNA sequence class
(defclass simple-rna-sequence (simple-na-sequence
                               rna-sequence-with-residues
                               2-bit-rna-sequence)
  ()
  (:documentation "Simple RNA sequence class with a 2-bit encoding (ACGU)."))

;;; adjustable RNA sequence class
(defclass adjustable-rna-sequence (2-bit-rna-sequence
                                   flexichain-sequence
                                   rna-sequence-with-residues)
  ()
  (:documentation "Adjustable RNA sequence class with a 2-bit encoding
 (ACGU)"))

(defun make-simple-rna-sequence (length)
  "Returns a simple-rna-sequence of the specified length."
  (make-instance 'simple-rna-sequence :length length))

(defun make-adjustable-rna-sequence (length)
  "Returns an adjustable-rna-sequence of the specified length."
  (make-instance 'adjustable-rna-sequence :length length))

(defun make-rna-sequence-from-string (residues)
  "Returns a simple-rna-sequence whose residues have been set to the
specified residues."
  (make-instance 'simple-rna-sequence :initial-contents residues))

(defun make-random-rna-sequence (length)
  "Returns a simple-rna-sequence of the specified length whose
residues are randomly chosen."
  (let* ((rna (make-simple-rna-sequence length)))
    (let ((k 4))
      (loop for i below length
         do (setf (residue-code rna i) (random k))))
    rna))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; protocol classes for amino acid sequences

;;; amino acid sequence protocol class
(defclass aa-sequence (bio-sequence)
  ()
  (:documentation "Protocol class for amino acid sequences (e.g. proteins)"))

(defclass aa-sequence-with-residues (aa-sequence
                                     sequence-with-residues)
  ()
  (:documentation "Protocl class for amino acid sequences with residues."))

;;; 5-bit amino acid sequence protocol class
(defclass 5-bit-aa-sequence (aa-sequence
                             5-bit-sequence
                             5-bit-aa-sequence-encoding)
  ()
  (:documentation "Implementation class for amino acid sequences
backed by a 5-bit residue encoding."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; implementation classes for amino acid sequences

;;; simple amino acid sequence class
(defclass simple-aa-sequence (5-bit-aa-sequence
                              simple-sequence
                              aa-sequence-with-residues)
  ()
  (:documentation "A subclass of simple-sequence for representing
amino acid sequences using a 5-bit residue encoding."))

;;; adjustable amino acid sequence class
(defclass adjustable-aa-sequence (5-bit-aa-sequence
                                  flexichain-sequence
                                  aa-sequence-with-residues)
  ()
  (:documentation "An adjustable sequence of amino acids using a 5-bit
residue encoding."))

(defmethod update-instance-for-different-class :after ((old simple-sequence)
                                                       (new adjustable-sequence)
                                                       &rest initargs
                                                       &key
                                                       initial-contents
                                                       initial-residue-codes)
  (declare (ignore initargs))
  (unless (or initial-contents initial-residue-codes)
    (typecase (residues old)
      (array
       (setf (residues new)
             (make-residues-flexichain
              new
              :initial-residue-codes (residues old)))))))


(defun make-simple-aa-sequence (length)
  "Returns a simple-aa-sequence of the specified length."
  (make-instance 'simple-aa-sequence :length length))

(defun make-aa-sequence-from-string (residues)
  "Returns a simple-aa-sequence whose contents are specified by
residues."
  (make-instance 'simple-aa-sequence :initial-contents residues))

(defun make-random-aa-sequence (length)
  "Retruns a simple-aa-sequence of the specified length whose residues
are set to random values."
  (let* ((aa (make-simple-aa-sequence length)))
    (let ((k 20))
      (loop for i below length
         do (setf (residue-code aa i) (random k))))
    aa))


;;;
;;; Codons, Translation and Friends
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass codon-target ()
    ((1-letter :initarg :1-letter :accessor 1-letter)
     (3-letter :initarg :3-letter :accessor 3-letter)
     (name :initarg :name :accessor codon-target-name))
    (:documentation "A protocol class to serve as the parent class of
    both stop-codons and amino-acids."))
  
  (defclass amino-acid (codon-target)
    ((name :initarg :name :accessor amino-acid-name))
    (:documentation "A representation of one of the 20 standard amino acids"))
  
  (defparameter *aa-name-hash-table* (make-hash-table :test 'equalp)
    "hash-table used to (case-insensitively) look up amino-acids by
    name.")

  (defparameter *aa-1-letter-hash-table* (make-hash-table :test 'equalp)
    "hash-table used to (case-insensitively) look up amino-acids by
    single letter code.")

  (defparameter *aa-3-letter-hash-table* (make-hash-table :test 'equalp)
    "hash-table used to (case-insensitively) look up amino-acids by
    three letter code.")
  
  (macrolet ((defaa (var one-letter three-letter name)
               `(let ((aa (make-instance 'amino-acid
                                        :1-letter ,one-letter
                                        :3-letter ,three-letter
                                        :name ,name)))
                  (defparameter ,var aa)
                  (setf (gethash ,name *aa-name-hash-table*) aa
                        (gethash ,one-letter *aa-1-letter-hash-table*) aa
                        (gethash ,three-letter *aa-3-letter-hash-table*) aa))))
    
    (progn
      (defaa *ala* #\a "ala" "alanine")
      (defaa *arg* #\r "arg" "arginine")
      (defaa *asn* #\n "asn" "asparagine")
      (defaa *asp* #\d "asp" "aspartic acid")
      (defaa *cys* #\c "cys" "cysteine")
      (defaa *gln* #\q "gln" "glutamine")
      (defaa *glu* #\e "glu" "glutamic acid")
      (defaa *gly* #\g "gly" "glycine")
      (defaa *his* #\h "his" "histidine")
      (defaa *ile* #\i "ile" "isoleucine")
      (defaa *leu* #\l "leu" "leucine")
      (defaa *lys* #\k "lys" "lysine")
      (defaa *met* #\m "met" "methionine")
      (defaa *phe* #\f "phe" "phenylalanine")
      (defaa *pro* #\p "pro" "proline")
      (defaa *ser* #\s "ser" "serine")
      (defaa *thr* #\t "thr" "threonine")
      (defaa *trp* #\w "trp" "tryptophan")
      (defaa *tyr* #\y "tyr" "tyrosine")
      (defaa *val* #\v "val" "valine"))))

(defmethod print-object ((object codon-target) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" 
            (codon-target-name object))))

(defclass stop-codon (codon-target)
  ((name :initarg :name :accessor stop-codon-name))
  (:documentation "stop-codons are codon-targets that signal that
  translation should stop."))

(defparameter *stop* (make-instance 'stop-codon :name "stop"
                                    :1-letter #\*
                                    :3-letter "***"))
(defparameter *amber* (make-instance 'stop-codon :name "amber"
                                     :1-letter #\*
                                     :3-letter "***"))
(defparameter *opal* (make-instance 'stop-codon :name "opal"
                                    :1-letter #\*
                                    :3-letter "***"))
(defparameter *ochre* (make-instance 'stop-codon :name "ochre"
                                     :1-letter #\*
                                     :3-letter "***"))

(defun 1-letter-aa (letter)
  "Returns an amino acid instance specified by the given letter."
  (case letter
    (#\* *stop*)
    (t (nth-value 0 (gethash letter *aa-1-letter-hash-table*)))))

(defun 3-letter-aa (letter)
  "Returns an amino acid instance specified by the given 3-letter
abbreviation."
  (if (equal letter "***")
      *stop*
      (nth-value 0 (gethash letter *aa-3-letter-hash-table*))))

(defclass codon ()
  ((triplet :accessor codon-triplet :initarg :triplet)
   (target :accessor codon-target :initarg :target))
  (:documentation "Instances of the codon class map between a given
  nucleotide triplet (e.g. AUG) and a given codon-target, either an
  amino acid or a stop codon."))

(defclass codon-table ()
  ((name :initarg :name :accessor codon-table-name)
   (codon-table-hash-table
    :accessor codon-table-hash-table
    :initarg :hash-table
    :initform (make-hash-table :test 'equalp)))
  (:documentation "A codon-table maps contains a hash-table whose keys
  are the nucleotide triplets and whose values are instances of the
  codon class."))

(defparameter *standard-codon-table*
  (make-instance 'codon-table)
  "An instance of the codon-table class for the standard genetic
  code.")

(defun make-codon-target (target triplets
                          &key
                          (codon-table *standard-codon-table*))
  (loop for triplet in triplets
     do
       (let ((codon (make-instance 'codon :triplet triplet :target target)))
         (setf (gethash triplet (codon-table-hash-table codon-table))
               codon))))
  
(progn
  (make-codon-target *ala* '("gcu" "gcc" "gca" "gcg"))
  (make-codon-target *arg* '("cgu" "cgc" "cga" "cgg" "aga" "agg"))
  (make-codon-target *asn* '("aau" "aac"))
  (make-codon-target *asp* '("gau" "gac"))
  (make-codon-target *cys* '("ugu" "ugc"))
  (make-codon-target *gln* '("caa" "cag"))
  (make-codon-target *glu* '("gaa" "gag"))
  (make-codon-target *gly* '("ggu" "ggc" "gga" "ggg"))
  (make-codon-target *his* '("cau" "cac"))
  (make-codon-target *ile* '("auu" "auc" "aua"))
  (make-codon-target *leu* '("uua" "uug" "cuu" "cuc" "cua" "cug"))
  (make-codon-target *lys* '("aaa" "aag"))
  (make-codon-target *met* '("aug"))
  (make-codon-target *phe* '("uuu" "uuc"))
  (make-codon-target *pro* '("ccu" "ccc" "cca" "ccg"))
  (make-codon-target *ser* '("ucu" "ucc" "uca" "ucg" "agu" "agc"))
  (make-codon-target *thr* '("acu" "acc" "aca" "acg"))
  (make-codon-target *trp* '("ugg"))
  (make-codon-target *tyr* '("uau" "uac"))
  (make-codon-target *val* '("guu" "guc" "gua" "gug"))
  (make-codon-target *amber* '("uag"))
  (make-codon-target *opal* '("uga"))
  (make-codon-target *ochre* '("uaa")))

(defclass genetic-code ()
  ((codon-table :accessor codon-table :initarg :codon-table)))

(defparameter *standard-genetic-code*
  (make-instance 'genetic-code :codon-table *standard-codon-table*))

(defgeneric translate (na-sequence
                       &key
                       range
                       genetic-code
                       &allow-other-keys)
  (:documentation "Translates the nucleic acid sequence na-sequence
  into an amino acid sequence."))

(defun enumerate-triplets (&key (letters '(#\a #\c #\g #\u)))
  (loop for l1 in letters
     nconc (loop for l2 in letters
              nconc (loop for l3 in letters
                       collect (concatenate 'string
                                            (string l1)
                                            (string l2)
                                            (string l3))))))

(defun get-codon-target (triplet &key (genetic-code
                                       *standard-genetic-code*))
  (let ((codon (gethash triplet
                        (codon-table-hash-table
                         (codon-table genetic-code)))))
    (when codon (codon-target codon))))


(defun check-triplets ()
  (loop for triplet in (enumerate-triplets)
     nconc (let ((codon (get-codon-target triplet)))
             (unless codon
               (list triplet)))))

(defgeneric dna->rna (dna-seqeunce &key &allow-other-keys))

(defmethod dna->rna ((dna dna-sequence-with-residues)
                     &key
                     (rna-class (etypecase dna
                                  (simple-dna-sequence
                                   'simple-rna-sequence)
                                  (adjustable-dna-sequence 
                                   'adjustable-rna-sequence))))
  (let ((rna (make-instance rna-class
                            :length (seq-length dna))))
    (loop for i below (seq-length rna)
       do (setf (residue-code rna i)
                (residue-code dna i)))
    rna))

(defmethod translate ((rna rna-sequence-with-residues)
                      &key
                      (range (range 0 (seq-length rna)))
                      genetic-code
                      (aa-class 'simple-aa-sequence)
                      truncate-on-stop)
  (let ((len (ceiling (range-length range) 3)))
    (let ((aa (make-instance aa-class :length len)))
      (loop for i from (range-start range) to (- (range-end range) 3)
         by 3
         for j below len
         do
           (let ((residues
                  (let ((r (range i (+ i 3))))
                    (residues-string-range rna r))))
             (let ((target (apply #'get-codon-target residues
                                  (when genetic-code
                                    `(:genetic-code ,genetic-code)))))
               (when target
                 (when truncate-on-stop
                   (typecase target
                     (stop-codon 
                      (setf aa (make-instance aa-class :length j
                                              :initial-contents
                                              (residues-string-range
                                               aa (range 0 j))))
                      (return))))
                 (setf (residue aa j)
                       (1-letter target))))))
      aa)))

(defmethod translate ((dna dna-sequence-with-residues)
                      &key
                      range
                      genetic-code
                      aa-class
                      truncate-on-stop)
  (apply #'translate (dna->rna dna)
         (append
          (when range `(:range ,range))
          (when genetic-code `(:genetic-code ,genetic-code))
          (when aa-class `(:aa-class ,aa-class))
          (when truncate-on-stop `(:truncate-on-stop ,truncate-on-stop)))))

(defgeneric 3-letter-residues-list (aa-sequence)
  (:documentation "Returns a list containing the three letter residue
  codes for the given amino acid sequence."))

(defmethod 3-letter-residues-list ((aa-sequence aa-sequence-with-residues))
  (map 'list
       (lambda (c) (nstring-upcase (3-letter (1-letter-aa c)) :start 0 :end 1))
       (residues-string aa-sequence)))

(defmethod 3-letter-residues-list ((aa-sequence string))
  (map 'list
       (lambda (c) (nstring-upcase (3-letter (1-letter-aa c)) :start 0 :end 1))
       aa-sequence))

(defgeneric 3-letter-residues-string (aa-sequence &optional stream)
  (:documentation "Returns a string consisting of the concatenation of
  the three letter codes for the given amino acid sequence."))

(defmethod 3-letter-residues-string (aa-sequence  &optional stream)
  (format stream "~{~A~}"
          (3-letter-residues-list aa-sequence)))


