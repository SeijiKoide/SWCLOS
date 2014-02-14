;;;-*- Mode: common-lisp; syntax: common-lisp; package: asdf; base: 10 -*-
;;;
;;;; SWCLOS: A Semantic Web Processor on CLOS
;;;
;;; IT Program Project in Japan:
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2003, 2004, 2006 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ASDF system definition.
;;; This file must be used without compiling.

(defpackage swclos-system (:use :common-lisp :asdf) (:export #:*base-directory*))

(in-package :swclos-system)

(defparameter asdf:*asdf-verbose* nil)

(defparameter *compile-verbose* nil)

;; experimental
(let ((loadpath *load-truename*))
  (defun swclos-directory ()
    (make-pathname :directory (pathname-directory loadpath))))

(setf (logical-pathname-translations "CODE")
      `(("**;*.*" ,(concatenate 'string (directory-namestring (swclos-directory)) "**/*.*"))))

(if (not (excl::logical-host-p "CODE"))
    (error "System :GX requires declaration of dominating logical host: CODE")
    (progn
      (setf (logical-pathname-translations "GX") `(("ontology;**;*.*" "CODE:src;ontology;**;*.*")
                                                   ("rdf;**;*.*"      "CODE:src;rdf;**;*.*")
                                                   ("rdfs;**;*.*"     "CODE:src;rdfs;**;*.*")
                                                   ("owl;**;*.*"      "CODE:src;owl;**;*.*")
                                                   ("ntriple;**;*.*"  "CODE:src;ntriple;**;*.*")))
      (setf (logical-pathname-translations "OWL") `(("*.*" "CODE:src;owl;*.*")))))

(defsystem :SWCLOS

  :name        "Galaxy Express" :version "0.9.2" :licence "SWCLOS"
  :description "SWCLOS implements OWL Full sematics for CLOS metaobject protcol."
  :author      "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer  "Dan Lentz <danlentz@gmail.com>"

  :long-description "Every resource in RDF and RDF(S), e.g.,
    rdfs:Class, rdfs:Resource, rdf:Property, and resource instances
    and properties are realized as CLOS objects with straightforward
    mapping RDF(S) classes/instances to CLOS classes/instances. Axioms
    and entailment rules in RDF(S) and OWL are embodied in the system
    so that a lisp programmer can make ontology in RDF(S) and use the
    ontology within the semantics specified by RDF(S) documents.

    OWL semantics are implemented on top of RDF(S) as the extension and
    augmentation of RDF(S) semantics. In SWCLOS, every instance of
    owl:Thing is an instance of rdfs:Resource, and every class of
    owl:Class is also a class of rdfs:Class. Therefore, any rule or
    method in RDF(S) works in OWL."

  :depends-on  (:cl-ppcre :iterate :local-time :logv)
  ;;  :do-first    (lambda () (require :cg))
  :properties  (:debug t :persistence nil :log-to #p"GX:log;message.log")
  :components
  ((:static-file "swclos.asd")
   (:module "src"
            :components
            ((:file "packages")
             (:module "rdf" :depends-on ("packages")
                      :serial t
                      :components ((:file "iri")
                                   (:file "literal")
                                   (:file "utils")
                                   (:file "rdf-io")
                                   (:file "xml")
                                   (:file "node")
                                   (:file "rdf-error"       :depends-on ("utils"))
                                   (:file "namespace")
                                   (:file "rdf-share"       :depends-on ("rdf-io" "namespace"))
                                   (:file "rdf-parser")
                                   (:file "rdf-reader")))

             (:module "rdfs" :depends-on ("rdf")
                      :serial t
                      :components ((:file "slot-def")
                                   (:file "rdf-boot" :depends-on ("slot-def"))
                                   (:file "gx-type") ;;            :depends-on ("SlotDef" "RDFboot"))
                                   (:file "domain-range") ;;     :depends-on ("RDFboot"))
                                   (:file "rdfs-objects") ;;     :depends-on ("RDFboot" "GxType"))
                                   (:file "rdfs-kernel") ;;      "RDFboot"))
                                   (:file "gx-forward-ref")
                                   (:file "rdfs-core")
                                   (:file "gx-utils")
                                   (:file "rdf-writer")
                                   (:file "turtle-parser")))

             (:module "owl" :depends-on ("rdfs")
                      :serial t
                      :components (
                                   (:file "owlerror")
                                   (:file "owlkernel")
                                   (:file "owlsamedifferent")
                                   (:file "owlequivalentdisjoint")
                                   (:file "nnf")
                                   (:file "tunify")
                                   (:file "subsume" :depends-on ("nnf" "tunify"))
                                   (:file "owl" :depends-on ("subsume"))))

             (:module "ntriple" :depends-on ("owl")
                      :serial t
                      :components ((:file "ntriple")
                                   (:file "nt-parser" :depends-on ("ntriple"))
                                   (:file "nt-writer" :depends-on ("nt-parser"))))

             (:module "utilities" :depends-on ("ntriple")
                      :serial t
                      :components ((:file "hyperspec")
                                   (:file "utilities")
                                   (:file "index")
                                   (:file "infix")))
             (:file "gx-user" :depends-on ("packages" "ntriple"))))))
