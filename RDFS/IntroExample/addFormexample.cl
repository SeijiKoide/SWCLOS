;; -*- mode: common-lisp; syntax: common-lisp; package: gx-user; base: 10 -*-
;;
;; This file is from the indtroductory example in http://www.w3.org/TR/2002/WD-rdf-schema-20021112/
;;

(defpackage eg
  (:shadow "name")
  (:export "Work" "Document" "Agent" "Person" "author" "Proposal" "name")
  (:documentation "http://somewhere-for-eg/eg"))
(defpackage dc
  (:export "title")
  (:documentation "http://dublincore.org/2002/08/13/dces"))

(in-package :gx-user)

(addForm '(rdf:Property
           eg:name
           (rdfs:domain eg:Person)
           (rdfs:range rdfs:Literal)))
(addForm '(rdf:Property
           dc:title
           (rdfs:domain eg:Document)
           (rdfs:range rdfs:Literal)))
(addForm '(rdf:Property
           eg:author
           (rdfs:domain eg:Document)
           (rdfs:range eg:Person)))

(addForm '(rdfs:Class
           eg:Agent
           (rdfs:subClassOf rdfs:Resource)))
(addForm '(rdfs:Class
           eg:Person
           (rdfs:subClassOf eg:Agent)))

(addForm '(rdfs:Class
           eg:Work
           (rdfs:subClassOf rdfs:Resource)))
(addForm '(rdfs:Class
           eg:Document
           (rdfs:subClassOf eg:Work)))

(addForm '(eg:Document
           eg:Proposal
           (eg:author (eg:Person (eg:name "Tim Berners-Lee")))
           (dc:title "Information Management: A Proposal")
           (rdf:about "http:/c/Proposal/")))

#|
(get-form eg:Proposal)
(-> eg:Proposal dc:title)
(-> eg:Proposal eg:author eg:name)
(-> eg:Proposal eg:author rdf:type)
|#