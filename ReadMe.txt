#|
SWCLOS is an RDF(S) and OWL Full processor built on top of CLOS. Every resources in RDF, 
e.g., rdfs:Class, rdfs:Resource, rdf:Property, and resource instances and properties 
are realized as CLOS objects with straightforward-mapping RDFS classes/instances to CLOS 
classes/instances. Axioms and entailment rules in RDF(S) and OWL are embodied in the system 
so that a lisp programmer can make ontology in RDF(S) and OWL, and then use the ontology 
within the semantics specified by RDF(S) documents and OWL documents. 

The OWL semantics is implemented on top of RDF(S) as the extension and augmentation of RDF(S) 
semantics. In SWCLOS, every instance of owl:Thing is also an instance of rdfs:Resource, and 
every class of owl:Class is also a class of rdfs:Class. Therefore, any rule or method in RDF(S) 
works in OWL. 

This SWCLOS version runs on top of Allegro Common Lisp version 8.2 and 8.1. The modern version 
of ACL is requisite to distinguish upper and lower letter cases. 

In this version, asdf file and defsystem file are prepared to easy loading without IDE. 

* RDFS.asdf/RDFSsys.cl     : Compile and load RDFS modules
* OWL.asdf/OWLsys.cl       : Compile and load RDFS + OWL modules
* SWCLOS.asdf/SWCLOSsys.cl : Compile and load RDFS + OWL + Ntriple modules

To load SWCLOS into ACL, there are three ways as follows.

1. Click `Open Project ...' item in `File' menu in the IDE environment, and select RDFS.LPR in RDFS folder 
   or OWL.LPR in OWL folder or SWCLOS.LPR in SWCLOS folder.

2. Use RDFSsys.cl in RDFS folder, or OWLsys.cl in OWL folder, or SWCLOSsys.cl in SWCLOS fonder 
   for ACL Defsystem. 
   See http://www.franz.com/support/documentation/8.1/doc/defsystem.htm.
   
   2.1 Load the file "RDFSsys.cl", or "OWLsys.cl", or "SWCLOSsys.cl".
   2.2 Compile RDFS or OWL system by typing "(compile-system :rdfs)", or 
       "(compile-system :owl)", or "(compile-system :swclos)".
   2.3 Load them by typeing "(load-system :rdfs)", or "(load-system :owl), 
       or "(laod-system :swclos)".

3. Use RDFS.asdf in RDFS folder or OWL.asdf in OWL folder for asdf Defsystem.

   3.1 Load "RDFS.asdf", or "OWL.asdf", or "SWCLOS.rdfs".
   3.2 Compile and Load RDFS or OWL or SWCLOS system. 
       See http://constantly.at/lisp/asdf/ to know how to operate ASDF.

After loading, use gx-user package by entering "(in-package gx-user)".

PPT files are prepared for hands on tutorials. They will be good introductions for you to
SWCLOS.
The manual folder includes user's manual for SWCLOS users.
The doc folder will help you when you read source files.

Note that *reify-p* and reification function are obsolete.
If you need such function, please use "collect-all-extensions-of <property>" 

Seiji Koide
8, Jan., 2011
National Institute of Informatics and IHI corporation
|#