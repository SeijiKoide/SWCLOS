;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Packages in RDFS System
;;;
;;; This module defines basic symbols in xml, xsd, rdf, rdfs, and owl package.
;;; Those symbols are exported so as to be QNames.

(in-package :cl-user)

(require :cg)

(defpackage :_     (:use))
(defpackage :x     (:use :cl))
(defpackage :xmlns (:use))
(defpackage :xml   (:use) (:export lang))

(defpackage :xsd   (:use) (:documentation "http://www.w3.org/2001/XMLSchema#")
            (:export string boolean decimal float double dataTime
                     time date base64Binary unsignedByte Name NCName
                     gYearMonth gYear gMonthDay gDay gMonth hexBinary
                     anyURI normallizedString token language NMTOKEN
                     integer nonPositiveInteger negativeInteger long int 
                     nonNegativeInteger unsignedLong unsignedInt unsignedShort
                     duration-hour duration-minute duration-second short byte
                     positiveInteger simpleType anySimpleType true false
                     duration duration-year duration-month duration-day))

(defpackage :rdf   (:use) (:documentation "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
            (:export _1 _2 _3 _4 _5 _6 _7 _8 _9 about XMLDatatype inLang 
                     resource ID parseType datatype nodeID nil first rest 
                     Property List Statement subject predicate object 
                     Bag Seq Alt value li XMLDecl RDF Description type
                     XMLLiteral XMLLiteral-equal type-p subclass-p PlainLiteral))

(defpackage :rdfs  (:use) (:documentation "http://www.w3.org/2000/01/rdf-schema#")
            (:export Resource Class subClassOf subPropertyOf seeAlso domain range
                     isDefinedBy range domain Literal Container label comment member
                     ContainerMembershipProperty Datatype))

(defpackage :owl   (:use) (:documentation "http://www.w3.org/2002/07/owl#")
            (:export Class Thing Nothing Restriction onProperty allValuesFrom
                     someValuesFrom hasValue minCardinality maxCardinality cardinality
                     allValuesFromRestriction someValuesFromRestriction hasValueRestriction 
                     cardinalityRestriction Ontology distinctMembers equivalentClass
                     oneOf differentFrom sameAs AllDifferent describe-slot-constraint 
                     TransitiveProperty ObjectProperty DatatypeProperty FunctionalProperty 
                     InverseFunctionalProperty SymmetricProperty inverseOf backwardCompatibleWith
                     intersectionOf unionOf disjointWith complementOf equivalentProperty
                     DataRange DeprecatedProperty DeprecatedClass incompatibleWith 
                     priorVersion versionInfo imports OntologyProperty AnnotationProperty))

(defpackage :gx
  (:shadow parse-uri subtypep type typep type-of uri value intern-uri)
  (:import-from :net.uri render-uri uri-fragment copy-uri uri-scheme)
  (:use :common-lisp :net.uri)
  (:export
   *NameSpaces*
   *autoepistemic-local-closed-world*
   *base-uri*
   *default-namespace*
   *entity-decls*
   *force-recursive-p*
   *nonUNA*
   *reify-p*
   *uri2symbol-name-mapping-fun*
   *uri2symbol-package-mapping-fun*
   ->
   ./ ; N3
   /. ; N3
   @
   Description-att&vals
   Description-elements
   Description-p
   Description-tag
   NCNameChar-p
   NCNameStartChar-p
   NameChar-p
   NameStartChar-p
   ^^
   addClass
   addForm
   addInstance
   addObject
   addRdfXml
   addTriple
   all-concept-names
   all-extensions-of-generator
   all-individuals
   all-instances-generator
   all-role-names
   anonymous-p
   class-direct-instances
   class?
   collect-all-extensions-of
   collect-all-instances-of
   collect-all-subs
   collect-all-subtypes
   collect-all-supers
   collect-direct-instances-of
   collect-domains
   collect-prop-names-from
   collect-ranges
   comment-p
   content
   dah
   datatype-p
   datatype?
   defConcept
   defIndividual
   defProperty
   defResource
   defTriple
   disjoint-p
   do-all-entity-iris
   do-all-entity-uris
   domain-value
   domainp
   dph
   export-as-QName
   get-domain
   get-form
   get-range
   get-slots
   get-triple
   get-triple-uri
   get-uri-namedspace
   get-value
   iri
   iri-p
   iri-value
   irregular-name&pkg
   lang
   line
   list-all-entities-in
   list-all-entity-iris
   list-all-entity-uris
   list-all-resources
   list-all-statements
   list-all-uri-namedspaces
   make-unique-nodeID
   mclasses
   metaRDFSclass
   metaclass?
   most-abstract-concepts
   most-specific-concepts
   name
   name-ontology
   named-p
   nodeID-p
   nodeID2symbol
   nodeID?
   object?
   owl-class-p
   owl-oneof-p
   owl-same-p
   owl-thing-p
   parse-XMLDecl
   parse-iri
   parse-rdf
   print-all-entity-iris
   print-all-entity-uris
   property?
   put-value
   quiet-warning-handler
   range-value
   rangep
   rdf-class-p
   rdf-equalp
   rdf-instance-p
   rdf-metaclass-p
   rdf-subtypep
   rdfsClass
   read-AttValue
   read-Eq
   read-as-datatype
   read-plane-text
   read-rdf-file
   resource?
   rsc-object-p
   set-uri-namedspace
   set-uri-namedspace-from-pkg
   strict-class-p
   subPropertyOf
   subclasses-of
   subproperty
   subproperty-p
   subsumed-p
   subtypep
   superclasses-of
   superproperty-of
   symbol2uri
   type-of
   typep
   uri-namedspace
   uri2env
   uri2package
   uri2symbol
   value-of
   with-quiet-warnings
   write-nt
   write-rdf-all-entities-in
   write-resource write-xml
   write-triple
   write-xml-all-entities-in
   |rdfs:Resource|
   ))

(defpackage :gx-test (:use :cl))

