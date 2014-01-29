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

(addInstance rdf:Property 'eg:name
             '((rdfs:domain eg:Person)
               (rdfs:range rdfs:Literal)))
(addInstance rdf:Property 'dc:title
             '((rdfs:domain eg:Document)
               (rdfs:range rdfs:Literal)))
(addInstance rdf:Property 'eg:author
             '((rdfs:domain eg:Document)
               (rdfs:range eg:Person)))

(addClass rdfs:Class 'eg:Agent '(rdfs:Resource)
          '((rdfs:subClassOf rdfs:Resource)) )
(addClass rdfs:Class 'eg:Person '(eg:Agent)
          '((rdfs:subClassOf eg:Agent)) )

(addClass rdfs:Class 'eg:Work '()
          '() )
(addClass rdfs:Class 'eg:Document '(eg:Work)
          '((rdfs:subClassOf eg:Work)) )

(addInstance eg:Document 'eg:Proposal
    `((eg:author ,(make-instance 'eg:Person 'eg:name "Tim Berners-Lee"))
      (dc:title "Information Management: A Proposal")
      (rdf:about "http:/c/Proposal/")))

#|
(get-form eg:Proposal)
(-> eg:Proposal dc:title)
(-> eg:Proposal eg:author eg:name)
(-> eg:Proposal eg:author rdf:type)
|#