;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; RDFS Objects module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2011 Seiji Koide
;;; ==================================================================================

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  (require :namespace)
  (require :rdfboot)
  (require :gxtype)
) ; end of eval-when

(in-package :gx)

(export '(subproperty superproperty-of @))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (uri-namedspace-package (set-uri-namedspace "http://www.w3.org/2001/XMLSchema#"))
    (find-package :xsd))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :xsd) t)))
    (find-package :xsd))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :rdf) t)))
    (find-package :rdf))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :rdfs) t)))
    (find-package :rdfs))
  )

;;
;; Instance data for rdfs:Class and rdfs:Resource
;;
;; Up to here, rdfs:Resource, rdfs:Class, rdf:Property, and rdfs:Literal are defined. 
;; Then, slot data for those metaobjects are added by reinitialize-instance.

(reinitialize-instance (load-time-value rdfs:Resource)
                       'rdf:type rdfs:Class
                       'rdfs:label "Resource"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#Resource"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:comment "The class resource, everything.")
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:Resource) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:Resource) 'rdf:about))))
  (symbol-value 'rdfs:Resource))

(reinitialize-instance rdfs:Class
                       'rdf:type rdfs:Class
                       'rdfs:label "Class"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#Class"
                       'rdfs:comment "The class of classes."
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:Class) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:Class) 'rdf:about))))
  (symbol-value 'rdfs:Class))

(reinitialize-instance rdfs:Literal
                       'rdf:type rdfs:Class
                       'rdfs:label "Literal"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#Literal"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:comment
                       "The class of literal values, e.g. textual strings and integers."
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:Literal) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:Literal) 'rdf:about))))
  (symbol-value 'rdfs:Literal))

;;;
;;;; Shadow Class
;;; When an object belongs to multiple classes, SWCLOS sets up a class that has the multiple classes in superclass list, 
;;; and change the class of the object to the new class instead of multiple classes. 
;;; Such a special class is called a shadow class which is an instance of metaclass <shadowed-class>.
;;; See following example in Wine Ontology.
;;; ----------------------------------------------------------------------------------
;;;  (mclasses vin:SaucelitoCanyonZinfandel1998)
;;;   -> (#&lt;owl:Class vin:Zinfandel&gt; #&lt;owl:Class vin:Vintage&gt;)
;;; 
;;;  (class-of vin:SaucelitoCanyonZinfandel1998)
;;;   -> #&lt;shadowed-class vin:Zinfandel.34&gt;
;;; ----------------------------------------------------------------------------------

(defclass shadowed-class (rdfs:Class)
  ()
  (:metaclass rdfs:Class)
  (:documentation "The shadow class for multiple classing."))

(defun shadow-name (abst mclasses)
  "creates and returns a name for shadowing class from <abst>.
   Note that 'shadow-name' is expressed as '<abst-name>.nn'. If <abst> is null, 
   the first class in <mclasses> that has name is used instead."
  (let ((sym (class-name abst)))
    (unless sym (setq sym (some #'(lambda (cls) (slot-value cls 'excl::name))
                                mclasses)))
    (cond ((shadowed-class-p (symbol-value sym))
           (let ((supers (mop:class-direct-superclasses (symbol-value sym))))
             (some #'(lambda (super) (shadow-name super supers)) supers)))
          (sym (let ((str (symbol-name sym))
                     (pkg (symbol-package sym)))
                 (gentemp (concatenate 'string str ".")
                          pkg))))))

(defun make-shadow (old-class mclasses)
  "changes <old-class> to a shadow class with <mclasses> as superclasses and returns it.
   Note that this function does not make a new shadow class, if mclasses of <old-class> 
   is a same set to <mclasses> or if some of subclasses of someone of <mclasses> is a 
   same set to <mclasses>."
  (assert (not (disjoint-pairs-p mclasses)) ()
          "There is a disjoint pair in ~S.~@[~%Check the form in the file.~]"
          mclasses)
  ;(format t "~%MAKE-SHADOW(~S ~S)" old-class mclasses)
  (cond ((set-eq (mklist old-class) mclasses) old-class)  ; 2011-05-28
        (t (let ((refined-classes (most-specific-concepts-for-refining (append (mklist old-class) mclasses))))
             ;(format t "~%  refined-classes ~S" refined-classes)
             (cond ((set-eq (mklist old-class) refined-classes) old-class)
                   ((null (cdr refined-classes)) (car refined-classes))
                   (t (apply #'mop:ensure-class-using-class nil (shadow-name old-class mclasses)
                             :direct-superclasses refined-classes
                             :metaclass 'shadowed-class
                             nil)))))))

(defclass destroyed-class (cl:standard-class) ())

;;;
;;; class-direct-instances are maintained by update-instance-for-different-class:after(gnode).
;;; Therefore, old-class is cleared when it falls here.
(defmethod update-instance-for-different-class :after ((previous rdfs:Resource) current &rest initargs)
  (declare (ignore initargs))
  (unless (cl:typep current 'destroyed-class)
    (let ((old-class (class-of previous)))
      (when (shadowed-class-p old-class)
        (when (and (null (mop:class-direct-subclasses old-class))
                   (null (class-direct-instances old-class)))
          (destroy old-class))))))

(defun destroy (class)
  ;(format t "~%~S is destroyed ..." class)
  (change-class class (find-class 'destroyed-class))
  (loop for super in (mop:class-direct-superclasses class)
      do (setf (slot-value super 'excl::direct-subclasses)
           (remove class (mop:class-direct-subclasses super))))
  (setf (slot-value class 'excl::direct-superclasses) nil)
  (setf (slot-value class 'excl::class-precedence-list) nil)
  (setf (slot-value class 'excl::slots) nil)
  (setf (slot-value class 'excl::wrapper) nil)
  )

(defun most-specific-concepts-for-refining (classes)
  "This function is used for class refining for an instance. So, we treat only rigid classes.
   <classes> must be clos objects."
  (let ((l (remove-duplicates classes
                              :test #'(lambda (x y)
                                        (cond ((eql x y))
                                              ((and (not (class-name x)) (not (class-name y)))
                                               (owl-equalp-for-refining x y)))))))
    (set-difference l l :test #'clos-strict-supertype-p)))
;; (typep vin:StGenevieveTexasWhite vin:WhiteNonSweetWine) -> tt
;; (typep vin:StGenevieveTexasWhite vin:DryWhiteWine)      -> tt
;; However, 
;; (clos-strict-supertype-p vin:DryWhiteWine vin:WhiteNonSweetWine) -> nil
  
;;
;; rdf:Property Final
;;

(reinitialize-instance rdf:Property
                       :direct-slots
                       `((:name rdfs:domain :initargs (rdfs:domain) )
                         (:name rdfs:range :initargs (rdfs:range) )
                         (:name rdfs:subPropertyOf :initform common-lisp:nil
                                :initfunction ,(load-time-value #'excl::false)
                                :type rdf:Property :initargs (rdfs:subPropertyOf)
                                :readers (superproperty-of) :writers ((setf superproperty-of)))
                         (:name subproperty :initform common-lisp:nil 
                                :initfunction ,(load-time-value #'excl::false)
                                :type rdf:Property :initargs (:subproperty))
                         (:name slotds :initargs (:slotds) :initform common-lisp:nil
                                :initfunction ,(load-time-value #'excl::false) )
                         (:name equivs :initargs (:equivs) :initform common-lisp:nil 
                                :initfunction ,(load-time-value #'excl::false) )
                         ;; this is for OWL
                         (:name equivalent-property :initargs (:equivalent-property)
                          :initform common-lisp:nil
                          :initfunction ,(load-time-value #'excl::false))
                         ))

(reinitialize-instance rdf:Property
                       'rdf:type rdfs:Class
                       'rdfs:label "Property"
                       'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                       'rdfs:comment "The class of RDF properties."
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdf:Property) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:Property) 'rdf:about))))
  (symbol-value 'rdf:Property))

;;;
;;;; Properties Final
;;;

(reinitialize-instance rdfs:label 
                       'rdf:type rdf:Property
                       'rdfs:label "label"
                       'rdf:type rdf:Property
                       'rdfs:comment "A human-readable name for the subject."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#label"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdfs:Resource
                       'rdfs:range rdfs:Literal)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:label) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:label) 'rdf:about))))
  (symbol-value 'rdfs:label))

(reinitialize-instance rdfs:domain
                       'rdf:type rdf:Property
                       'rdfs:label "domain"
                       'rdf:type rdf:Property
                       'rdfs:comment "A domain of the subject property."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#domain"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdf:Property
                       'rdfs:range rdfs:Class)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:domain) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:domain) 'rdf:about))))
  (symbol-value 'rdfs:domain))

(reinitialize-instance rdfs:range
                       'rdf:type rdf:Property
                       'rdfs:label "range"
                       'rdf:type rdf:Property
                       'rdfs:comment "A range of the subject property."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#range"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdf:Property
                       'rdfs:range rdfs:Class)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:range) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:range) 'rdf:about))))
  (symbol-value 'rdfs:range))

(reinitialize-instance rdfs:comment
                       'rdf:type rdf:Property
                       'rdfs:label "comment"
                       'rdf:type rdf:Property
                       'rdfs:comment "A description of the subject resource."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#comment"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdfs:Resource
                       'rdfs:range rdfs:Literal)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:comment) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:comment) 'rdf:about))))
  (symbol-value 'rdfs:comment))

(reinitialize-instance rdfs:subClassOf
                       'rdf:type rdf:Property
                       'rdfs:label "subClassOf"
                       'rdf:type rdf:Property
                       'rdfs:comment "The subject is a subclass of a class."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#subClassOf"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdfs:Class
                       'rdfs:range rdfs:Class)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:subClassOf) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:subClassOf) 'rdf:about))))
  (symbol-value 'rdfs:subClassOf))

(reinitialize-instance rdfs:subPropertyOf
                       'rdf:type rdf:Property
                       'rdfs:label "subPropertyOf"
                       'rdf:type rdf:Property
                       'rdfs:comment "The subject is a subproperty of a property."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#subPropertyOf"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:domain rdf:Property
                       'rdfs:range rdf:Property)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:subPropertyOf) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:subPropertyOf) 'rdf:about))))
  (symbol-value 'rdfs:subPropertyOf))

(defparameter rdfs:seeAlso
  (make-instance 'rdf:Property
    :name 'rdfs:seeAlso
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/2000/01/rdf-schema#seeAlso"
    'rdfs:label "seeAlso"
    'rdfs:comment "Further information about the subject resource."
    'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
    'rdfs:domain rdfs:Resource
    'rdfs:range rdfs:Resource
    :subproperty `(,rdfs:isDefinedBy))
  "is used to indicate a resource that might provide additional information about the subject resource.")
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:seeAlso) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:seeAlso) 'rdf:about))))
  (symbol-value 'rdfs:seeAlso))

(reinitialize-instance rdfs:isDefinedBy
                       'rdf:type rdf:Property
                       'rdfs:label "isDefinedBy"
                       'rdfs:comment "The definition of the subject resource."
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#isDefinedBy"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:subPropertyOf `(,rdfs:seeAlso)
                       'rdfs:domain rdfs:Resource
                       'rdfs:range rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:isDefinedBy) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:isDefinedBy) 'rdf:about))))
  (symbol-value 'rdfs:isDefinedBy))

(defparameter rdf:type
  (make-instance 'rdf:Property
    :name 'rdf:type
    'rdf:type rdf:Property
    'rdfs:label "type"
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    'rdfs:comment "The subject is an instance of a class."
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:domain rdfs:Resource
    'rdfs:range rdfs:Class)
  "is used to state that a resource is an instance of a class.")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:type) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:type) 'rdf:about))))
  (symbol-value 'rdf:type))

;===========================================================================================
(defparameter rdf:predicate
  (make-instance 'rdf:Property
    :name 'rdf:predicate
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate"
    'rdfs:label "predicate"
    'rdfs:comment "The predicate of the subject RDF statement."
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:range rdf:Property)
  "is used to state the predicate of a statement.")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:predicate) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:predicate) 'rdf:about))))
  (symbol-value 'rdf:predicate))

(defparameter rdf:subject
  (make-instance 'rdf:Property
    :name 'rdf:subject
    'rdf:type rdf:Property
    'rdfs:label "subject"
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"
    'rdfs:comment "The subject of the subject RDF statement."
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:range rdfs:Resource)
  "is used to state the subject of a statement.")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:subject) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:subject) 'rdf:about))))
  (symbol-value 'rdf:subject))

(defparameter rdf:object
  (make-instance 'rdf:Property
    :name 'rdf:object
    'rdf:type rdf:Property
    'rdfs:label "object"
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#object"
    'rdfs:comment "The object of the subject RDF statement."
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:range rdfs:Resource)
  "is used to state the object of a statement.")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:object) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:object) 'rdf:about))))
  (symbol-value 'rdf:object))

(defparameter rdf:Statement
  (defclass rdf:Statement (rdfs:Resource)
    ((excl::name :initarg :name :initform ())
     (rdf:subject :initarg :subject :accessor rdf:subject)
     (rdf:predicate :initarg :predicate )
     (rdf:object :initarg :object ))
    (:metaclass rdfs:Class))
  "It is intended to represent the class of RDF statements. 
An RDF statement is the statement made by a token of an RDF triple.")

(reinitialize-instance rdf:Statement
                       'rdf:type rdfs:Class
                       'rdfs:label "Statement"
                       'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                       'rdfs:comment "The class of RDF statements."
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdf:Statement) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:Statement) 'rdf:about))))
  (symbol-value 'rdf:Statement))

(reinitialize-instance rdf:predicate
                       'rdfs:domain rdf:Statement)
(reinitialize-instance rdf:subject
                       'rdfs:domain rdf:Statement)
(reinitialize-instance rdf:object
                       'rdfs:domain rdf:Statement)

;===========================================================================================
;;;
;;;; rdfs:Datatype, rdf:PlainLiteral, and rdf:XMLLiteral
;;;

(reinitialize-instance rdfs:Datatype
                       'rdf:type rdfs:Class
                       'rdfs:label "Datatype"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#Datatype"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:comment "The class of RDF datatypes."
                       'rdfs:subClassOf rdfs:Class)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:Datatype) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:Datatype) 'rdf:about))))
  (symbol-value 'rdfs:Datatype))

(reinitialize-instance rdf:PlainLiteral
                       'rdf:type rdfs:Datatype
                       'rdfs:label "PlainLiteral"
                       'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/TR/rdf-plain-literal/")
                       'rdfs:comment "The class of plain (i.e. untyped) literal values."
                       'rdfs:subClassOf rdfs:Literal)
(setf (iri-value (setf (slot-value (symbol-value 'rdf:PlainLiteral) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:PlainLiteral) 'rdf:about))))
  (symbol-value 'rdf:PlainLiteral))

(defmethod print-object ((object rdf:XMLLiteral) stream)
  "This method is not intended to use by programmer. XMLLiteral data is printed as 1^^xsd:|nonNegativeInteger|."
  (format stream "~W^^~S"
    (format nil "~A" (slot-value object 'value))
    (class-name (class-of object))))

(defclass ill-structured-XMLLiteral (rdfs:Resource)
  ((data :initarg :data)
   (type :initarg :type))
  (:metaclass rdfs:Class))

(defmethod print-object ((object ill-structured-XMLLiteral) stream)
  (format stream "~W^^~A"
    (format nil "~A" (slot-value object 'data))
    (format nil "~A" (class-name (slot-value object 'type)))))

(reinitialize-instance rdf:XMLLiteral
                       'rdf:type rdfs:Datatype
                       'rdfs:label "XMLLiteral"
                       'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                       'rdfs:comment "The class of XML literal values."
                       'rdfs:subClassOf rdfs:Literal)
(setf (iri-value (setf (slot-value (symbol-value 'rdf:XMLLiteral) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:XMLLiteral) 'rdf:about))))
  (symbol-value 'rdf:XMLLiteral))

;;;
;;;; Datatype Objects
;;;
;;; Every RDFS datatype is defined as lisp type and RDF resource, too.
;;; As lisp type, data as lisp object is typed as follows.
;;; ----------------------------------------------------------------------------------
;;;  (cl:typep 1 'xsd:|nonNegativeInteger|)
;;; ----------------------------------------------------------------------------------
;;; See XML module.
;;;
;;; As RDFS datatype, RDF resource is typed as follows.
;;; ----------------------------------------------------------------------------------
;;;  (setq foo (^^ 1 xsd:|nonNegativeInteger|))
;;;  (typep foo xsd:|nonNegativeInteger|)
;;; ----------------------------------------------------------------------------------
;;;
;;; Note that excl:sub-subtypep works well on the following datatype classes
;;;

(defparameter xsd:|anySimpleType|
  (mop:ensure-class-using-class () 'xsd:|anySimpleType| 
                                :direct-superclasses '(rdf:XMLLiteral)
                                :form '(cl:or xsd:|boolean| xsd:|anyURI| xsd:|string| xsd:|float| xsd:|double| xsd:|decimal|)
                                :metaclass 'rdfs:Datatype)
  "xsd:|anySimpleType| class object")
(reinitialize-instance xsd:|anySimpleType|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "anySimpleType"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#anySimpleType"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf rdf:XMLLiteral)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|anySimpleType|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|anySimpleType|) 'rdf:about))))
  (symbol-value 'xsd:|anySimpleType|))

(defparameter xsd:|decimal|
  (mop:ensure-class-using-class () 'xsd:|decimal| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form 'cl:rational
                                :metaclass 'rdfs:Datatype)
  "xsd:|decimal| class object")
(reinitialize-instance xsd:|decimal|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "decimal"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#decimal"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|decimal|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|decimal|) 'rdf:about))))
  (symbol-value 'xsd:|decimal|))

(defparameter xsd:|integer|
  (mop:ensure-class-using-class () 'xsd:|integer| 
                                :direct-superclasses '(xsd:|decimal|)
                                :form 'cl:integer
                                :metaclass 'rdfs:Datatype
                                'rdf:about "http://www.w3.org/2001/XMLSchema#integer")
  "xsd:|integer| class object")
(reinitialize-instance xsd:|integer|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "integer"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#integer"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|decimal|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|integer|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|integer|) 'rdf:about))))
  (symbol-value 'xsd:|integer|))

(defparameter xsd:|nonPositiveInteger|
  (mop:ensure-class-using-class () 'xsd:|nonPositiveInteger| 
                                :direct-superclasses '(xsd:|integer|)
                                :form '(cl:integer cl:* 0)
                                :metaclass 'rdfs:Datatype)
  "xsd:|nonPositiveInteger| class object")
(reinitialize-instance xsd:|nonPositiveInteger|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "nonPositiveInteger"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#nonPositiveInteger"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|integer|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|nonPositiveInteger|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|nonPositiveInteger|) 'rdf:about))))
  (symbol-value 'xsd:|nonPositiveInteger|))

(defparameter xsd:|negativeInteger|
  (mop:ensure-class-using-class () 'xsd:|negativeInteger| 
                                :direct-superclasses '(xsd:|nonPositiveInteger|)
                                :form '(cl:integer cl:* -1)
                                :metaclass 'rdfs:Datatype)
  "xsd:|negativeInteger| class object")
(reinitialize-instance xsd:|negativeInteger|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "negativeInteger"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#negativeInteger"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|nonPositiveInteger|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|negativeInteger|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|negativeInteger|) 'rdf:about))))
  (symbol-value 'xsd:|negativeInteger|))

(defparameter xsd:|long|
  (mop:ensure-class-using-class () 'xsd:|long| 
                                :direct-superclasses '(xsd:|integer|)
                                :form '(cl:signed-byte 64)
                                :metaclass 'rdfs:Datatype)
  "xsd:|long| class object")
(reinitialize-instance xsd:|long|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "long"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#long"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|integer|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|long|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|long|) 'rdf:about))))
  (symbol-value 'xsd:|long|))

(defparameter xsd:|int|
  (mop:ensure-class-using-class () 'xsd:|int| 
                                :direct-superclasses '(xsd:|long|)
                                :form '(cl:signed-byte 32)
                                :metaclass 'rdfs:Datatype)
  "xsd:|int| class object")
(reinitialize-instance xsd:|int|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "int"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#int"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|long|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|int|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|int|) 'rdf:about))))
  (symbol-value 'xsd:|int|))

(defparameter xsd:|short|
  (mop:ensure-class-using-class () 'xsd:|short| 
                                :direct-superclasses '(xsd:|int|)
                                :metaclass 'rdfs:Datatype)
  "xsd:|short| class object")
(reinitialize-instance xsd:|short|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "short"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#short"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|int|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|short|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|short|) 'rdf:about))))
  (symbol-value 'xsd:|short|))

(defparameter xsd:|byte|
  (mop:ensure-class-using-class () 'xsd:|byte| 
                                :direct-superclasses '(xsd:|short|)
                                :form '(cl:signed-byte 8)
                                :metaclass 'rdfs:Datatype)
  "xsd:|byte| class object")
(reinitialize-instance xsd:|byte|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "byte"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#byte"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|short|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|byte|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|byte|) 'rdf:about))))
  (symbol-value 'xsd:|byte|))

(defparameter xsd:|nonNegativeInteger|
  (mop:ensure-class-using-class () 'xsd:|nonNegativeInteger| 
                                :direct-superclasses '(xsd:|integer|)
                                :form '(cl:integer 0 cl:*)
                                :metaclass 'rdfs:Datatype)
  "xsd:|nonNegativeInteger| class object")
(reinitialize-instance xsd:|nonNegativeInteger|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "nonNegativeInteger"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#nonNegativeInteger"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|integer|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|nonNegativeInteger|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|nonNegativeInteger|) 'rdf:about))))
  (symbol-value 'xsd:|nonNegativeInteger|))

(defparameter xsd:|positiveInteger|
  (mop:ensure-class-using-class () 'xsd:|positiveInteger| 
                                :direct-superclasses '(xsd:|nonNegativeInteger|)
                                :form '(cl:integer 1 cl:*)
                                :metaclass 'rdfs:Datatype)
  "xsd:|positiveInteger| class object")
(reinitialize-instance xsd:|positiveInteger|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "positiveInteger"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#positiveInteger"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|nonNegativeInteger|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|positiveInteger|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|positiveInteger|) 'rdf:about))))
  (symbol-value 'xsd:|positiveInteger|))

(defparameter xsd:|unsignedLong|
  (mop:ensure-class-using-class () 'xsd:|unsignedLong| 
                                :direct-superclasses '(xsd:|nonNegativeInteger|)
                                :form '(cl:unsigned-byte 64)
                                :metaclass 'rdfs:Datatype)
  "xsd:|unsignedLong| class object")
(reinitialize-instance xsd:|unsignedLong|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "unsignedLong"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#unsignedLong"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|nonNegativeInteger|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|unsignedLong|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|unsignedLong|) 'rdf:about))))
  (symbol-value 'xsd:|unsignedLong|))

(defparameter xsd:|unsignedInt|
  (mop:ensure-class-using-class () 'xsd:|unsignedInt| 
                                :direct-superclasses '(xsd:|unsignedLong|)
                                :form '(cl:unsigned-byte 32)
                                :metaclass 'rdfs:Datatype)
  "xsd:|unsignedInt| class object")
(reinitialize-instance xsd:|unsignedInt|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "unsignedInt"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#unsignedInt"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|unsignedLong|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|unsignedInt|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|unsignedInt|) 'rdf:about))))
  (symbol-value 'xsd:|unsignedInt|))

(defparameter xsd:|unsignedShort|
  (mop:ensure-class-using-class () 'xsd:|unsignedShort| 
                                :direct-superclasses '(xsd:|unsignedInt|)
                                :form '(cl:unsigned-byte 16)
                                :metaclass 'rdfs:Datatype)
  "xsd:|unsignedShort| class object")
(reinitialize-instance xsd:|unsignedShort|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "unsignedShort"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#unsignedShort"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|unsignedInt|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|unsignedShort|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|unsignedShort|) 'rdf:about))))
  (symbol-value 'xsd:|unsignedShort|))

(defparameter xsd:|unsignedByte|
  (mop:ensure-class-using-class () 'xsd:|unsignedByte| 
                                :direct-superclasses '(xsd:|unsignedShort|)
                                :form '(cl:unsigned-byte 8)
                                :metaclass 'rdfs:Datatype)
  "xsd:|unsignedByte| class object")
(reinitialize-instance xsd:|unsignedByte|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "unsignedByte"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#unsignedByte"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|unsignedShort|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|unsignedByte|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|unsignedByte|) 'rdf:about))))
  (symbol-value 'xsd:|unsignedByte|))

(defparameter xsd:|string|
  (mop:ensure-class-using-class () 'xsd:|string| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form 'cl:string
                                :metaclass 'rdfs:Datatype)
  "xsd:|string| class object")
(reinitialize-instance (symbol-value 'xsd:|string|)
                       'rdf:type rdfs:Datatype
                       'rdfs:label "string"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#string"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|string|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|string|) 'rdf:about))))
  (symbol-value 'xsd:|string|))

(defparameter xsd:|float|
  (mop:ensure-class-using-class () 'xsd:|float| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form 'cl:single-float
                                :metaclass 'rdfs:Datatype)
  "xsd:|float| class object")
(reinitialize-instance xsd:|float|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "float"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#float"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|float|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|float|) 'rdf:about))))
  (symbol-value 'xsd:|float|))

(defparameter xsd:|double|
  (mop:ensure-class-using-class () 'xsd:|double| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form 'cl:double-float
                                :metaclass 'rdfs:Datatype)
  "xsd:|double| class object")
(reinitialize-instance xsd:|double|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "double"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#double"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|double|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|double|) 'rdf:about))))
  (symbol-value 'xsd:|double|))

(defparameter xsd:|anyURI|
  (mop:ensure-class-using-class () 'xsd:|anyURI| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form 'uri
                                :metaclass 'rdfs:Datatype)
  "xsd:|anyURI| class object")
(reinitialize-instance xsd:|anyURI|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "anyURI"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#anyURI"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|anyURI|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|anyURI|) 'rdf:about))))
  (symbol-value 'xsd:|anyURI|))

(defparameter xsd:|boolean|
  (mop:ensure-class-using-class () 'xsd:|boolean| 
                                :direct-superclasses '(xsd:|anySimpleType|)
                                :form '(cl:member xsd:|true| xsd:|false|)
                                :metaclass 'rdfs:Datatype)
  "xsd:|boolean| class object")
(reinitialize-instance xsd:|boolean|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "boolean"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#boolean"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|boolean|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|boolean|) 'rdf:about))))
  (symbol-value 'xsd:|boolean|))

(defparameter xsd:|true|
  (make-instance 'xsd:|boolean| :value t)
  "xsd:|boolean| instance")
(defparameter xsd:|false|
  (make-instance 'xsd:|boolean| :value nil)
  "xsd:|boolean| instance")

(defparameter xsd:|duration|
  (mop:ensure-class-using-class
   () 'xsd:|duration| 
   :direct-superclasses '(xsd:|anySimpleType|)
   :direct-slots `((:name year :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:year)
                    :readers (xsd:|duration-year|) :writers ((setf xsd:|duration-year|)))
                   (:name month :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:month)
                    :readers (xsd:|duration-month|) :writers ((setf xsd:|duration-month|)))
                   (:name day :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:day)
                    :readers (xsd:|duration-day|) :writers ((setf xsd:|duration-day|)))
                   (:name hour :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:hour)
                    :readers (xsd:|duration-hour|) :writers ((setf xsd:|duration-hour|)))
                   (:name minute :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:minute)
                    :readers (xsd:|duration-minute|) :writers ((setf xsd:|duration-minute|)))
                   (:name second :initform () :initfunction ,(load-time-value #'excl::false)
                    :initargs (:second)
                    :readers (xsd:|duration-second|) :writers ((setf xsd:|duration-second|))))
   :metaclass 'rdfs:Datatype)
  "xsd:|duration| class object")
(reinitialize-instance xsd:|duration|
                       'rdf:type rdfs:Datatype
                       'rdfs:label "duration"
                       'rdf:about "http://www.w3.org/2001/XMLSchema#duration"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2001/XMLSchema#")
                       'rdfs:subClassOf xsd:|anySimpleType|)
(setf (iri-value (setf (slot-value (symbol-value 'xsd:|duration|) 'rdf:about)
                   (iri (slot-value (symbol-value 'xsd:|duration|) 'rdf:about))))
  (symbol-value 'xsd:|duration|))

(defmethod print-object ((obj xsd:|duration|) stream)
  "This method is not intended to be used by users."
  (princ "P" stream)
  (with-slots ((yy year) (mo month) (dd day) (hh hour) (mi minute) (ss second))
      obj
    (when yy (princ yy stream) (princ "Y" stream))
    (when mo (princ mo stream) (princ "M" stream))
    (when dd (princ dd stream) (princ "D" stream))
    (princ "T" stream)
    (when hh (princ hh stream) (princ "H" stream))
    (when mi (princ mi stream) (princ "M" stream))
    (when ss (princ ss stream) (princ "S" stream))))

;===========================================================================================

(reinitialize-instance rdf:List
                       'rdf:type rdfs:Class
                       'rdfs:label "List"
                       'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                       'rdfs:comment "The class of RDF Lists."
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdf:List) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:List) 'rdf:about))))
  (symbol-value 'rdf:List))

(defparameter rdf:nil
  (make-instance 'rdf:List
    :name 'rdf:nil
    'rdfs:label "nil"
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:comment
    "The empty list, with no items in it. If the rest of a list is nil then the list has no
more items in it.")
  "rdf:List instance")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:nil) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:nil) 'rdf:about))))
  (symbol-value 'rdf:nil))

(defparameter rdf:first
  (make-instance 'rdf:Property
    :name 'rdf:first
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
    'rdfs:label "first"
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:comment "The first item in the subject RDF list."
    'rdfs:domain rdf:List
    'rdfs:range rdfs:Resource)
  "rdf:Property instance")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:first) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:first) 'rdf:about))))
  (symbol-value 'rdf:first))

(defparameter rdf:rest
  (make-instance 'rdf:Property
    :name 'rdf:rest
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
    'rdfs:label "rest"
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:comment "The rest of the subject RDF list after the first item."
    'rdfs:domain rdf:List
    'rdfs:range rdf:List)
  "rdf:Property instance")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:rest) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:rest) 'rdf:about))))
  (symbol-value 'rdf:rest))

;===========================================================================================
;;
;; rdf:value
;;

(defparameter rdf:value
  (make-instance 'rdf:Property
    :name 'rdf:value
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#value"
    'rdfs:label "value"
    'rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    'rdfs:comment "Idiomatic property used for structured values."
    'rdfs:domain rdfs:Resource
    'rdfs:range rdfs:Resource)
  "rdf:Property instance")
(setf (iri-value (setf (slot-value (symbol-value 'rdf:value) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdf:value) 'rdf:about))))
  (symbol-value 'rdf:value))

;===========================================================================================
;;;
;;;; Containers in RDF
;;; rdfs:Container, rdfs:member, rdfs:ContainerMembershipProperty, 

(defparameter rdfs:Container
  (defclass rdfs:Container (rdfs:Resource)
    ()
    (:metaclass rdfs:Class))
  "The class of RDF containers.")
(reinitialize-instance rdfs:Container
                       'rdf:type rdfs:Class
                       'rdfs:label "Container"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#Container"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:comment "The class of RDF containers."
                       'rdfs:subClassOf rdfs:Resource)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:Container) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:Container) 'rdf:about))))
  (symbol-value 'rdfs:Container))

(defparameter rdfs:member
  (make-instance 'rdf:Property
    :name 'rdfs:member
    'rdf:type rdf:Property
    'rdf:about "http://www.w3.org/2000/01/rdf-schema#member"
    'rdfs:label "member"
    'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
    'rdfs:comment "A member of the subject container."
    'rdfs:domain rdfs:Container                              ; reaxiomatized by Seiji 2008/6/27
    'rdfs:range rdfs:Resource)
  "rdf:Property instance")
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:member) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:member) 'rdf:about))))
  (symbol-value 'rdfs:member))

(defparameter rdfs:ContainerMembershipProperty
  (defclass rdfs:ContainerMembershipProperty (rdf:Property)
    ()
    (:metaclass rdfs:Class))
  "The class of RDF containers.")
(reinitialize-instance rdfs:ContainerMembershipProperty
                       'rdf:type rdfs:Class
                       'rdfs:label "Container"
                       'rdf:about "http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty"
                       'rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#")
                       'rdfs:comment "The class of container membership properties, rdf:_1, rdf:_2, ...,
  all of which are sub-properties of 'member'."
                       'rdfs:subClassOf rdf:Property)
(setf (iri-value (setf (slot-value (symbol-value 'rdfs:ContainerMembershipProperty) 'rdf:about)
                   (iri (slot-value (symbol-value 'rdfs:ContainerMembershipProperty) 'rdf:about))))
  (symbol-value 'rdfs:ContainerMembershipProperty))

;; End of module
;; --------------------------------------------------------------------

(cl:provide :rdfsobjects)
