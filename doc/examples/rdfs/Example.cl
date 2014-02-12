;; -*- mode: common-lisp; syntax: common-lisp; package: gx-user; base: 10 -*-
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; Copyright © 2005 by Galaxy Express Corporation
;;
;; This file is created by Seiji Koide from the indtroductory example 
;; in http://www.w3.org/TR/2002/WD-rdf-schema-20021112/
;;

(defpackage eg
  (:documentation "http://somewhere-for-eg/eg"))
(defpackage dc
  (:documentation
   "http://dublincore.org/2002/08/13/dces")) 

(in-package gx-user)

(defResource eg::Work (rdfs:subClassOf rdfs:Resource))
(defResource eg::Agent (rdfs:subClassOf rdfs:Resource))
(defResource eg::Person (rdfs:subClassOf eg::Agent))
(defResource eg::Document (rdfs:subClassOf eg::Work))

(defProperty eg::author 
  (rdfs:domain eg::Document)
  (rdfs:range eg::Person))
(defProperty eg::name
  (rdfs:domain eg::Person)
  (rdfs:range rdfs:Literal))
(defProperty dc::title
  (rdfs:domain eg::Document)
  (rdfs:range rdfs:Literal))

(defIndividual eg::Proposal (rdf:type eg::Document)
  (eg::author (eg::Person (eg::name "Tim Berners-Lee")))
  (dc::title "Information Management: A Proposal")
  (rdf:about "http:/c/Proposal/"))

#|
;; in the RDFS environment
(load "Example.cl")
eg:Person
eg:author
eg:Proposal

(describe eg:Proposal)
(get-form eg:Proposal)
(-> eg:Proposal dc:title)
(-> eg:Proposal eg:author eg:name)
(-> eg:Proposal eg:author rdf:type)

(write-xml eg:Proposal)
|#
