;; -*- mode: common-lisp; syntax: common-lisp; package: gx-user; base: 10 -*-
;;
;; This file is from the indtroductory example in http://www.w3.org/TR/2002/WD-rdf-schema-20021112/
;;

(defpackage eg
  (:export "Work" "Document" "Agent" "Person" "author" "Proposal" "name")
  (:documentation "http://somewhere-for-eg/eg"))
(defpackage dc
  (:export "title")
  (:documentation "http://dublincore.org/2002/08/13/dces"))
(defpackage _ 
  (:export "a"))

(in-package :gx-user)

(defTriple eg:name rdfs:range rdfs:Literal)
(defTriple dc:title rdfs:range rdfs:Literal)
(defTriple eg:Person rdfs:subClassOf eg:Agent)
(defTriple eg:Document rdfs:subClassOf eg:Work)
(defTriple eg:author rdf:type rdf:Property)
(defTriple eg:author rdfs:domain eg:Document)
(defTriple eg:author rdfs:range eg:Person)
(defTriple _:a rdf:type eg:Person)
(defTriple _:a eg:name "Tim Berners-Lee")

(defTriple eg:Proposal rdf:type eg:Document)
(defTriple eg:Proposal eg:author _:a)
(defTriple eg:Proposal dc:title "Information Management: A Proposal")

#|
(get-form eg:Proposal)
(-> eg:Proposal eg:author eg:name)
(-> eg:Proposal dc:title)
(-> eg:Proposal eg:author rdf:type)
|#