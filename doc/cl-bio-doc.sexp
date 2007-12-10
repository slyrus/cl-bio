((:smarkup-metadata
  (:copyright
   "Copyright 2007, Cyrus Harmon. All Rights Reserved.")
  (:title "cl-bio: A library for representing and processing biological sequence data")
  (:author "Cyrus L. Harmon")
  (:bibtex-database
   "(\"asdf:/ch-bib/lisp\" \"asdf:/ch-bib/bio\")")
  (:bibtex-style "Science"))
 (:html-metadata (:htmlcss "simple.css") )
 
 (:span
  (:h1 "Abstract")
 
  (:p "cl-bio is a library for the representation, processing, and
  analysis of various forms of biological data."))
 
 (:span
  (:h1 "Download and Installation")
  (:p "[TBW]"))

 (:span
  (:h1 "Foundational Biological Objects: ")
  (:p "")
  
  (:span
   (:h2 "bio-object:")
   (:p "The bio-object class is a parent class for various biological
   classes. The bio-object class is a subclass of the described-object
   class."))
  
  (:span
   (:h2 "bio-set:")
   (:p ""))
  
  (:span
   (:h2 "descriptor:")
   (:p "")))

 (:span
  (:h1 "Services")
  (:p ""))

 (:span
  (:h2 "dictionary:")
  (:p ""))
 
 (:span
  (:h1 "Conceptual Biological Objects")
  (:p "")
  
  (:span
   (:h2 "Taxonomies"))

  (:span
   (:h2 "bio-sequence:")
   (:p "The concept of a sequence, or a linear arrangement of
   repeating elements is central to molecular biology. The central
   dogma dictates that DNA codes for RNA which codes for Proteins. All
   of these molecules can be viewed as sequences, based on a linear
   ordering of the subunits that create these molecules. The concept
   of sequence is so central to bioinformatics that it the
   representation of the sequence is often conflated with other
   biological concepts such as a chromosome, a gene, or a protein. In
   general, in cl-bio we represent these biological entities as
   distinct instances of various classes that have a sequence
   associated, rather than being a gene. So instances of the gene
   class, for instance, may have a sequence as a member, but do not
   extend the sequence class directly.")

   (:p "A sequence, at its most general, is a linear arrangement of
   repeating subunits with a given length. Given that the common lisp
   standard defines the class sequence to be an ordered collection of
   objects, we use the term bio-sequence to describe biological
   sequences.")

   (:span
    (:h3 "Sequence Residues")
    (:p "The repeated element in biological sequences is usually
   referred to as a residue and represents the portions of the
   precursor molecules, nucleotide bases or amino acids, that are
   incorporated into the polymer. The sequence-with-residues class is
   designed to represent sequences that have an explicit
   representation of the individual subunits in the sequence."))

   (:span
    (:h3 "Sequence Residue Encodings")
    (:p "Biological sequences are made up of repeated subunits and
    generally represented by a concise encoding of the possible
    subunits, such as A for Adenine, G for Guanine, etc... for
    nucleotides (for DNA and RNA), or A for Alanine, C for Cytosine,
    etc... for amino acid residues (for proteins).")

    (:p "When representing these sequences computationally, there is
    no need to use a character such #\A or #\C to represnt, a compact
    encoding using small integer values allows for a two-bit encoding
    for DNA and RNA and a five-bit enconding for protein
    sequences. Besides space efficiency, one would presumably like
    efficient (roughly constant time) access to random elements in the
    sequence. Fortunately, common lisp arrays provide both efficient
    access through the aref accessor function and the ability to
    sepcialize the array storage to particular types such as 8-bit
    integers or, importantly, 2-bit integers. Therefore we can use
    common lisp's built in arrays to represent DNA and RNA sequences
    with a 2-bit per element encoding and get fast random access to
    individual elements."))

   (:span
    (:h3 "Editable Sequences")
    (:p "Different representations of sequences or sets have different
    properties such as efficiency of accessing random elments,
    efficiency of inserting or appending elements, etc... Lisp arrays
    offer efficient random access to elements and for the setting of
    individual elements, but do not support the insertion of elements
    with the array, with the exception of the adjust-array function
    which allows for the appending of new elements to the end of the
    array.")))

  
  (:span
   (:h2 "gene:")
   (:p ""))

  (:span
   (:h2 "annotation")
   (:p "")))

 (:span
  (:h1 "Examples")
  (:p "To create an adjustable DNA sequence one could use the
  following code:")
  (:lisp-no-results
   #q{(defparameter *dna-seq-1* (make-instance 'bio:adjustable-dna-sequence))
   (bio:insert-residues *dna-seq-1* 0 "GAATTC")
   (bio:residues-string *dna-seq-1*)}))
 

#+nil (:bibliography))
