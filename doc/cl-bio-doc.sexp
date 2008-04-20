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
  data such as nucleic acid and protein sequences, annotations on
  sequences, sequence alignments, species databases, etc... in the
  spirit of " (:a :href "http://www.bioperl.org" "BioPERL") ".")
  
  (:span
   (:h2 "Download and Installation")
   (:p "For the moment, cl-bio is only available through git. No formal
  release has been made yet. The git repository can be found at "
       :newline
       (:a :href "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=cl-bio.git" 
           "http://git.cyrusharmon.org/cgi-bin/gitweb.cgi?p=cl-bio.git"))
   (:span 
    (:h2 "Requirements")
    (:p "cl-bio has only been tested on SBCL but, in theory, should be
   relatively easily ported to other common lisp environments. The
   core cl-bio library depends on a number of external libraries,
   including:")

    (:list
     (:item (:a :href "http://common-lisp.net/project/alexandria/"
                "alexandria"))
     (:item (:a :href "http://www.weitz.de/cl-ppcre/" "cl-ppcre"))
     (:item (:a :href "http://common-lisp.net/project/flexichain/"
                "flexichain")) 
     (:item (:a :href "http://common-lisp.net/project/rucksack/"
                "rucksack"))))

   (:span
    (:h2 "Modules")
    (:p "cl-bio is organized as a core library that is extended by
     additional module libraries. There are both core modules which
     are automatically loaded when cl-bio is loaded, such as the io
     module and laodable modules that modules must be explicitly
     loaded by code that expects to use the functionality provided by
     the module, such as the align and entrez modules.")
    (:span
     (:h3 "Module Requirements")
     (:span
      (:h4 "cl-bio-rucksack")
      (:list
       (:item (:a :href "http://common-lisp.net/project/rucksack/"
                  "rucksack"))))
     (:span
      (:h4 "cl-bio-taxonomy")
      (:list
       (:item (:a :href "http://common-lisp.net/project/rucksack/"
                  "bio-rucksack"))))
     (:span
      (:h4 "cl-bio-entrez")
      (:list
       (:item (:a :href "http://puri.b9.com/" "puri"))
       (:item (:a :href "http://weitz.de/drakma/" "drakma"))
       (:item (:a :href "http://common-lisp.net/project/cxml/" "cxml"))
       (:item (:a :href "http://www.lichteblau.com/cxml-stp/" "cxml-stp"))
       (:item (:a :href "http://common-lisp.net/project/plexippus-xpath/" "plexippus xpath"))))))))
 
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
    accessor function and the ability to specialize the array storage
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
  :h1 "Common Bioinformatics Tasks with cl-bio"
  
  (:p "The following sections contain examples of performing a number
 of common (and not so common, completely contrived) bioinformatics
 tasks with the cl-bio library.")
  
  (:span
   (:h2 "Loading a FASTA file")
   (:lisp
    #q{(defparameter *dpp* (bio:read-fasta-file "data/dpp.fasta"))})
   
   (:lisp
    #q{(append (subseq (bio:split-string-into-lines-list
                        (bio:residues-string (car *dpp*)) :max-line-length 60)
                       0 3)
               '("..."))})))

 (:span
  (:h1 "Bio-sequences")

  (:p "The cl-bio Bio-sequence classes and generic functions are
  provided for working with various classes of biological sequences,
  such as DNA, RNA and protein sequences.")

  (:span
   (:h2 "bio-sequence class")
   
   (:p "The protocol class bio-sequence is a class that all
  implementations of bio-sequence should inherit from. The only
  property of bio-sequence that all sequences are expected to have is
  a length, although the length may be nil or other arbitrary
  values. It is generally intended that for sequences with residues
  the length will be the number of residues in the sequence.")
   
   (:class bio:bio-sequence)
   (:generic-function bio:seq-length))
   
  (:span
   (:h2 "Sequences with Residues")
    
   (:p "The core bio-sequence is only required to have a (possibly
   nil) length, it does not necessarily contain linearly addressable
   subunits, or residues. The bio:sequence-with-residues class is
   a protocol class for sequences that have associated residues.")
    
   (:class bio:sequence-with-residues)
   (:generic-function bio:residues-string)
   (:generic-function (setf bio:residues-string))
   (:generic-function bio:residues-string-range)
   (:generic-function bio:residue)
   (:generic-function (setf bio:residue))
   (:generic-function bio:seq-reverse)

   (:span
    (:h3 "Residue Codes")
    (:p "Rather than using characters directly to code for residues,
    the class bio:sequence-with-residue-codes is the protocol class
    that classes that use residue-codes should inherit from.")
    
    (:class bio:sequence-with-residue-codes)
    (:generic-function bio:residue-code)
    (:generic-function (setf bio:residue-code))))
  

  (:span
   (:h2 "Annotated Sequences")
   (:p "")

   (:class bio:annotated-sequence))

  (:span
   (:h2 "Simple Sequences")
   (:p "")

   (:class bio:simple-sequence))

  (:span
   (:h2 "Adjustable Sequences")
   (:p "")

   (:class bio:adjustable-sequence)
   (:generic-function bio:insert-residue)
   (:generic-function bio:insert-residues)
   (:generic-function bio:insert-residue-codes)
   (:generic-function bio:append-residue)
   (:generic-function bio:append-residues)
   (:generic-function bio:append-residue-codes)
   (:generic-function bio:delete-residues))
  
  (:span
   (:h2 "Nucleic Acid Sequences")
   (:p "")

   (:class bio:na-sequence)
   (:class bio:na-sequence-with-residues)

   (:span
    (:h3 "DNA Sequences")
    (:class bio:dna-sequence)
    (:class bio:dna-sequence-with-residues)
    (:class bio:simple-dna-sequence)
    (:class bio:adjustable-dna-sequence)
    
    (:generic-function bio:reverse-complement)
    
    (:function bio:make-simple-dna-sequence)
    (:function bio:make-adjustable-dna-sequence)
    (:function bio:make-dna-sequence-from-string)
    (:function bio:make-random-dna-sequence)

    (:p "To create an adjustable DNA sequence one could use the
  following code:")
    (:lisp
   #q{(defparameter *dna-seq-1* (make-instance
                                 'bio:adjustable-dna-sequence
                                 :initial-contents "GAATTC"))})
    
    (:p "And to retrieve all the residues of the sequence:")
    (:lisp
     #q{(bio:residues-string *dna-seq-1*)}))
   
   (:span
    (:h3 "RNA Sequences")
    (:class bio:rna-sequence)
    (:class bio:rna-sequence-with-residues)
    (:class bio:simple-rna-sequence)
    (:class bio:adjustable-rna-sequence)
    
    (:function bio:make-simple-rna-sequence)
    (:function bio:make-adjustable-rna-sequence)
    (:function bio:make-rna-sequence-from-string)
    (:function bio:make-random-rna-sequence)
    
    (:p "To create an adjustable RNA sequence one could use the
  following code:")
    (:lisp
   #q{(defparameter *rna-seq-1* (make-instance
                                 'bio:adjustable-rna-sequence
                                 :initial-contents "AAGUUUUAG"))})
    
    (:p "And to retrieve all the residues of the sequence:")
    (:lisp
     #q{(bio:residues-string *rna-seq-1*)})))

  (:span
   (:h2 "Amino Acid Sequences")
   (:p "")
   (:class bio:aa-sequence)
   (:class bio:aa-sequence-with-residues)
   (:class bio:simple-aa-sequence)
   (:class bio:adjustable-aa-sequence)
   (:class bio:aa-sequence-with-residues)
   
   (:function bio:make-simple-aa-sequence)
   (:function bio:make-aa-sequence-from-string)
   (:function bio:make-random-aa-sequence)
   
   (:p "To create an adjustable amino acid sequence one could use the
  following code:")
    (:lisp
   #q{(defparameter *aa-seq-1* (make-instance
                                 'bio:adjustable-aa-sequence
                                 :initial-contents "MYRSTVLKCDQCLP"))})
    
    (:p "And to retrieve all the residues of the sequence:")
    (:lisp
     #q{(bio:residues-string *aa-seq-1*)}))

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
  (:h1 "Input and Output")
  
  (:span
   (:h2 "FASTA Files")
   (:p "The sequence similarity searching program FASTA uses a simple
   file format for storing biological sequences, consisting of a
   header line, starting with \">\", followed by the sequence
   itself. FASTA files can be read with read-fasta-file function")

   ))

 (:span
  (:h1 "Dictionaries")
  (:p "Dictionaries provide two simple operations, lookup and
  fetch. The idea behind lookup is that one can use lookup to see if
  the dictionary contains information about an object without
  explicitly retrieving the object. (I suppose we could use other
  names like, say, search and retrieve here.)"))

 
 (:span
  (:h1 "Loadable Module: cl-bio-rucksack")
  (:p "The cl-bio-rucksack module exposes a rucksack for use by other
  cl-bio modules for persistently storing lisp objects."))

 (:span
  (:h1 "Loadable Module: cl-bio-taxonomy")
  (:p "It is often important to understand the origin of a given
  biological entity, that is to say which species did the biological
  entity come from. There are cases where this is implicit, such as
  large databases from a single species, but, especially as the number
  of organisms from which substantial amounts of sequences have been
  generated, it is important to track not just the name of the species
  from which the entity was derived, but also the relationship of said
  species to other species, usually in the form of a taxonomic
  tree."))

 (:span
  (:h1 "Loadable Module: cl-bio-entrez")
  (:p "The entrez module provides functions and classes for
  interfacing with NCBI's " (:a :href
  "http://ncbi.nlm.nih.gov/sites/entrez" "Entrez") " database and
  services. Entrez provides a web interface for accessing "))

 (:span
  (:h1 "Examples")
  
  )

#+nil (:bibliography))
