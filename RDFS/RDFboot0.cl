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
;; History
;; -------
;;
;;; ==================================================================================
;;; The aim of this file is to establish the complex relation of meta-circularity in RDF 
;;; incrementally step by step. Loading this file yealds the following hierachical structure 
;;; at the end.
;;; ----------------------------------------------------------------------------------
;;;                                         ........................
;;;                                         :                      :
;;;                                         :                    rdfsClass
;;;                                         :              ...../..:
;;;                                         :              :   /  ..........
;;; cl:standard-class -- rdf-node ----------:--------rdfs:Class---:---shadowed-class
;;;                           : :...........:       /      : ::...:
;;;                           :               ...../.......: :
;;;                           :               :   /          :
;;;  cl:standard-object -- gnode --- rdfs:Resource --- rdf:Property
;;;
;;;  ---, /  super/sub class relation, the direction of super is right to left.
;;;  ..., :  class/instance relation, the direction of class is upward and left to right.
;;; ----------------------------------------------------------------------------------
;;;
;;; The twisted relation between rdfs:Class and rdfsClass, that is, rdfs:Class is a super 
;;; class and an instance of rdfsClass produces a trick of (cl:typep rdfs:Class rdfs:Class).
;;; Note that every method defined at rdfs:Class affects rdfs:Class itself. Therefore, the 
;;; world is closed by the membership loop of rdfs:Class just like cl:standard-class in CLOS.
;;;
;;; The twisted relation between rdfs:Resource and rdfs:Class yields semantics of RDFS, that is, 
;;; (cl:typep rdfs:Resource rdfs:Class) and (cl:typep rdfs:Resource rdfs:Resource).
;;;
;;; The method <class-direct-instances>, which maintains direct instances of a class, is 
;;; inherited to rdfs:Class and rdfsClass. So, subclasses of rdfs:Resource and rdfs:Class, 
;;; including rdfs:Class itself can hold their instances.

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  (require :slotdef)
  (require :rdfnode)
) ; end of eval-when

(in-package :gx)

(export '(|rdfs:Resource| metaRDFSclass rdfsClass *reify-p*
          nodeID? nodeID2symbol mclasses))
(export '(property? subPropertyOf class-direct-instances))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class rdf-node)
				  (superclass standard-class))
    t))

;;; To be re-defined in GxType.cl
(defun typep (obj type &optional env)
  (cl:typep obj type env))
(defun subtypep (sub super &optional env)
  (cl:subtypep sub super env))
(defun type-of (object)
  (cl:type-of object))

;;; Portable versions of private MOP APIs in Allegro CL
(eval-when (:compile-toplevel :execute)
  (declaim (inline standard-instance-p)))

(defun standard-instance-p (object)
  (subtypep (type-of object) 'standard-object))

;;;
;;;; First of all, 
;;; we make skeltons that provide subtyping and metaclassing. 
;;;
;;;; rdfsClass & rdfs:Class
;;; rdfsClass is invented in order to realize the rdfs:Class membership loop. Namely, the class of 
;;; rdfs:Class is rdfs:Class itself in RDF(S) semantics. In SWCLOS rdfs:Class is actually the 
;;; class of rdfs:Class, because rdfs:Class is a superclass of rdfsClass and rdfsClass is a class of 
;;; rdfs:Class. Thus, all methods are for instances of rdfs:Class is effective for rdfs:Class itself. 

(defclass rdfsClass (rdf-node) () ; this is redefined later.
  (:metaclass rdf-node))

(defclass rdfs:Class (rdf-node) ()
  (:metaclass rdfsClass))

(cl:provide :rdfboot0)
