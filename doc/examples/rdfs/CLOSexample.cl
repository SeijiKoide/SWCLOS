;; -*- mode: common-lisp; syntax: common-lisp; package: cl-user; base: 10 -*-
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; Copyright © 2002, 2003, 2004, 2005 by Galaxy Express Corporation
;;
;; This file is created by Seiji Koide from the indtroductory example 
;; in http://www.w3.org/TR/2002/WD-rdf-schema-20021112/
;;

(defpackage rdf
  (:export "about")
  (:documentation "http://www.w3.org/1999/02/22-rdf-syntax-ns"))
(defpackage rdfs
  (:export "Resource")
  (:documentation "http://www.w3.org/2000/01/rdf-schema"))
(defpackage eg
  (:export "Work" "Document" "Agent" "Person" "author" "Proposal" "name")
  (:documentation "http://somewhere-for-eg/eg"))
(defpackage dc
  (:export "title")
  (:documentation "http://dublincore.org/2002/08/13/dces"))

(defclass rdfs:Resource ( ) ((rdf:about :initarg :about)))
(defclass eg:Work (rdfs:Resource) ( ))
(defclass eg:Agent (rdfs:Resource) ( ))
(defclass eg:Person (eg:Agent)
  ((eg:name :initarg :name :type string)))
(defclass eg:Document (eg:Work) 
  ((eg:author :initarg :author :type eg:Person)
   (dc:title :initarg :title)))

(defparameter eg:Proposal
  (make-instance 'eg:Document
    :author (make-instance 'eg:Person :name "Tim Berners-Lee")
    :title "Information Management: A Proposal"
    :about "http:/Åc/Proposal/"))
#|
;; in the pure CLOS environment

(load "CLOSexample.cl")
(describe eg:Proposal)
(typep eg:Proposal 'eg:Document)
(typep eg:Proposal 'eg:Work)
(typep eg:Proposal 'rdfs:Resource)
(slot-value (slot-value eg:Proposal 'eg:author) 'eg:name)
(slot-value eg:Proposal 'dc:title)
|#
