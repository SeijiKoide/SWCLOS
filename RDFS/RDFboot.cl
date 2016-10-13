;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf Boot module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2010 Seiji Koide
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfboot1)
) ; end of eval-when

(in-package :gx)

;;;
;;;; rdfs:Literal & rdf:XMLLiteral
;;;

(defparameter rdfs:|Literal|
  (defclass rdfs:|Literal| (rdfs:|Resource|) () (:metaclass rdfs:|Class|))
  "rdfs:Literal is a subclass of rdfs:Resource and an instance of rdfs:Class.")

(defparameter rdf:|PlainLiteral|
  (defclass rdf:|PlainLiteral| (rdfs:|Literal|)
    ((value :initarg :value))
    (:metaclass rdfs:|Datatype|))
  "rdf:PlainLiteral is a subclass of rdfs:Literal and an instance of rdfs:Datatype.
   An instance has a value of PlainLiteral data.")
    
(defparameter rdf:|XMLLiteral|
  (defclass rdf:|XMLLiteral| (rdfs:|Literal|)
    ((value :initarg :value))
    (:metaclass rdfs:|Datatype|))
  "rdf:XMLLiteral is a subclass of rdfs:Literal and an instance of rdfs:Datatype.
   An instance has a value of XMLLiteral data.")

(defmethod value-of ((x rdf:|PlainLiteral|))
  "retrieves a value of XMLLiteral data."
  (slot-value x 'value))

(defmethod value-of ((x rdf:|XMLLiteral|))
  "retrieves a value of XMLLiteral data."
  (slot-value x 'value))

(defmethod value-of (x)
  "otherwise returns itself."
  x)

;;;
;;;; rdfs:Resource final
;;; rdfs:Resource is reinitialized with slots.

(reinitialize-instance (load-time-value rdfs:|Resource|)
                       :direct-slots
                       `((:name rdf:|about| :initargs (rdf:|about|))
                         (:name rdf:|ID| :initargs (rdf:|ID|))
                         (:name xml:lang :initform common-lisp:nil
                          :initfunction ,(load-time-value #'excl::false)
                          :initargs (xml:lang))
                         (:name rdfs:|label| :type ,(load-time-value rdfs:|Literal|)
                          :initargs (rdfs:|label|))
                         (:name rdfs:|isDefinedBy| :type ,(load-time-value rdfs:|Resource|)
                          :initargs (rdfs:|isDefinedBy|))
                         (:name rdfs:|comment| :type ,(load-time-value rdfs:|Literal|)
                                :initargs (rdfs:|comment|))
                         (:name rdf:|type| :type ,(load-time-value rdfs:|Class|)
                                :initargs (rdf:|type|)
                                :documentation "for bookkeeping user-definition"))
                       )

;;;
;;;; rdf:List
;;;

(defparameter rdf:|List|
  (defclass rdf:|List| (rdfs:|Resource|)
    ()
    (:metaclass rdfs:|Class|))
  "rdf:List is a subclass of rdfs:Resource and an instance of rdfs:Class.")

;;;
;;;; rdf:Property
;;; Now, here many properties are defined.

(defparameter rdfs:|label|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|label|
    )
  "may be used to provide a human-readable version of a resource's name.")

(defparameter rdfs:|comment|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|comment|
    )
  "may be used to provide a human-readable description of a resource.")

(defparameter rdfs:|isDefinedBy|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|isDefinedBy|
    )
  "is used to indicate a resource defining the subject resource. 
This property may be used to indicate an RDF vocabulary in which a resource is described.")

(defparameter rdfs:|domain|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|domain|
    )
  "is used to state that any resource that has a given property is an instance of one or more classes.")

(defparameter rdfs:|range|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|range|
    )
  "is used to state that the values of a property are instances of one or more classes.")

(defparameter rdfs:|subClassOf|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|subClassOf|
    )
  "is used to state that all the instances of one class are instances of another.")

(defparameter rdfs:|subPropertyOf|
  (make-instance 'rdf:|Property|
    :name 'rdfs:|subPropertyOf|
    )
  " is used to state that all resources related by one property are also related by another.")

(reinitialize-instance (load-time-value rdfs:|Class|)
                       :direct-instances `(,(load-time-value rdf:|Property|)
                                           ,(load-time-value rdfs:|Resource|)
                                           ,(load-time-value rdfs:|Class|))
                       )

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;

(cl:provide :rdfboot)
