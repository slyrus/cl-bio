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
  (:title "cl-bio")
 
  (:p "cl-bio is a library for the representation, processing, and
  analysis of various forms of biological data."))
  
  (:p "cl-bio was written by Cyrus Harmon and is available under a
  BSD-style license.")
 
 (:span
  (:h1 "Introduction")
  (:p "cl-bio is a common lisp library for working with biological
  sequences in the spirit of BioPERL. "))

 (:span
  (:h1 "Download and Installation")
  (:p "For the moment, cl-bio is only available through git. No formal
  release has been made yet. The git repository can be found at "
      :newline
      (:a :href "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=cl-bio.git" 
          "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=cl-bio.git")))

 (:span
  (:h1 "Foundational Biological Objects")
  (:p "")
  
  (:span
   (:h2 "bio-object")
   (:p "The bio-object class is a parent class for various biological
   classes. The bio-object class is a subclass of the described-object
   class."))
  
  (:span
   (:h2 "bio-set")
   (:p ""))
  
  (:span
   (:h2 "descriptor")
   (:p "")))

 (:span
  (:h1 "Services")
  (:p ""))

 (:span
  (:h2 "dictionary")
  (:p ""))
 
 (:span
  (:h1 "Conceptual Biological Objects")
  (:p "")
  
  (:span
   (:h2 "Biological Sequences")

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
    no need to use a character such as #\\A or #\\C to represent a
    subuint of the sequence; a compact encoding using small integer
    values allows for a two-bit encoding for DNA and RNA and a
    five-bit enconding for protein sequences. Besides space
    efficiency, one would presumably like efficient (roughly constant
    time) access to random elements in the sequence. Fortunately,
    common lisp arrays provide both efficient access through the aref
    accessor function and the ability to sepcialize the array storage
    to particular types such as 8-bit integers or, importantly, 2-bit
    integers. Therefore we can use common lisp's built in arrays to
    represent DNA and RNA sequences with a 2-bit per element encoding
    and get fast random access to individual elements."))

   (:span
    (:h3 "Editable Sequences")
    (:p "Different representations of sequences or sets have different
    properties such as efficiency of accessing random elments,
    efficiency of inserting or appending elements, etc... Lisp arrays
    offer efficient random access to elements and for the setting of
    individual elements, but do not support the insertion of elements
    with the array, with the exception of the adjust-array function
    which allows for the appending of new elements to the end of the
    array."))
   
   (:span
    (:h3 "Ranges")
    (:p "The range class is used to specify a range on a biological sequence.")))
  
  (:span
   (:h2 "Genes")
   (:p "[TBW]"))

  (:span
   (:h2 "Annotations")
   (:p "[TBW]"))

  (:span
   (:h2 "Alignments")
   (:p "[TBW]")))
 
 (:span
   (:h2 "Species Taxonomies")
   (:p "It is often important to understand the origin of a given
   biological entity, that is to say which species did the biological
   entity come from. There are cases where this is implicit, such as
   large databases from a single species, but, especially as the
   number of organisms from which substantial amounts of sequences
   have been generated, it is important to track not just the name of
   the species from which the entity was derived, but also the
   relationship of said species to other species, usually in the form
   of a taxonomic tree."))

 (:span
  (:h1 "Bio-sequences")

  (:p "")

  (:span
   (:h2 "bio-sequence class")
   
   (:p "The protocol class bio-sequence is a class that all
  implementations of bio-sequence should inherit from."))
  
  (:span

   (:h2 "sequence-range class")

   (:p "The sequence-range class is used to represent ranges on a
   given sequence. It contains the slots sequence and range, with the
   respective accessor sequence-range-sequence and
   sequence-range-range.")
   
   (:class bio:range)
   (:generic-function bio:range-equal)
   (:generic-function bio:range-contains)
   (:generic-function bio:range-start)
   (:generic-function bio:range-end)
   (:generic-function bio:range-min)
   (:generic-function bio:range-max)))

 
 (:span
  (:h1 "Examples")
  
  (:p "To create an adjustable DNA sequence one could use the
  following code:")
  (:lisp
   #q{(defparameter *dna-seq-1* (make-instance
                              'bio:adjustable-dna-sequence
                              :initial-contents "GAATTC"))})

  (:p "And to retrieve all the residues of the sequence:")
  (:lisp
   #q{(bio:residues-string *dna-seq-1*)}))

#+nil (:bibliography))
