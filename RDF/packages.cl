;;;-*- Mode: common-lisp; syntax: common-lisp; package: cg; base: 10 -*-
;;;
;;;; Packages in RDF, RDFS, and OWL System
;;;
;;; This module defines basic symbols in xml, xsd, rdf, rdfs, and owl package.
;;; Those symbols are exported so as to be QNames.

(cl:provide :swclospackages)

(cl:defpackage :xmlns
  (:use ) ; supressing using common lisp package
  )
(cl:defpackage :xml
  (:use ) ; supressing using common lisp package
  (:export lang ))
(cl:defpackage :_                 ; this package is provided for nodeID symbol.
  (:use ) ; supressing using common lisp package
  )

(cl:defpackage :xsd
  (:nicknames :xs)
  (:use net.uri) ; supressing using common lisp package
  (:export "string" "boolean" "decimal" "float" "double" "dataTime" "time" "date"
           "gYearMonth" "gYear" "gMonthDay" "gDay" "gMonth" "hexBinary" "base64Binary"
           "anyURI" "normallizedString" "token" "language" "NMTOKEN" "Name" "NCName"
           "integer" "nonPositiveInteger" "negativeInteger" "long" "int" "short" "byte"
           "nonNegativeInteger" "unsignedLong" "unsignedInt" "unsignedShort" "unsignedByte"
           "positiveInteger" "simpleType" "anySimpleType" "true" "false"
           "duration" "duration-year" "duration-month" "duration-day" "duration-hour"
           "duration-minute" "duration-second")
  (:documentation "http://www.w3.org/2001/XMLSchema#"))

(cl:defpackage :dc
  (:use ) ; supressing using common lisp package
  (:export #:contributor #:coverage #:creator #:date #:description #:format #:identifier
           #:language #:publisher #:relation #:rights #:source #:subject #:title #:type))

(cl:defpackage :rdf
  (:use ) ; supressing using common lisp package
  (:export about
           XMLDatatype 
           inLang 
           resource ID parseType datatype nodeID
           Property List Statement subject predicate object 
           Bag Seq Alt value li XMLDecl RDF Description type
           nil first rest XMLLiteral XMLLiteral-equal
           _1 _2 _3 _4 _5 _6 _7 _8 _9
           type-p subclass-p
           PlainLiteral)
  (:documentation "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))

(cl:defpackage :rdfs
  (:use ) ; supressing using common lisp package
  (:export Resource Class subClassOf subPropertyOf seeAlso domain range
           isDefinedBy range domain Literal Container label comment member
           ContainerMembershipProperty Datatype)
  (:documentation "http://www.w3.org/2000/01/rdf-schema#"))

(defpackage :owl
    (:use ) ; supressing using common lisp package
    (:export Class Thing Nothing Restriction onProperty allValuesFrom someValuesFrom hasValue
             minCardinality maxCardinality cardinality
             allValuesFromRestriction someValuesFromRestriction hasValueRestriction 
             cardinalityRestriction Ontology
             oneOf differentFrom sameAs AllDifferent distinctMembers equivalentClass
             TransitiveProperty ObjectProperty DatatypeProperty FunctionalProperty 
             InverseFunctionalProperty SymmetricProperty inverseOf
             intersectionOf unionOf disjointWith complementOf equivalentProperty
             describe-slot-constraint 
             DataRange DeprecatedProperty DeprecatedClass incompatibleWith backwardCompatibleWith
             priorVersion versionInfo imports OntologyProperty AnnotationProperty
             )
  ;; documentation is supplied from OWL.RDF file.
  )

(defpackage :gx
  (:use :common-lisp :net.uri)
  (:shadow typep subtypep type-of)
  (:documentation "http://www.TopOntologies.com/tools/SWCLOS#"))
