;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-
;;;
;;;; SWCLOS: A Semantic Web Processor on CLOS
;;;
;;; IT Program Project in Japan: 
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright © 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008 Seiji Koide

;;; ACL Defsystem Definition for RDFS + OWL

(eval-when (:load-toplevel :execute)
  (defparameter *owl-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "OWL")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *owl-directory*)
         :device (pathname-device *owl-directory*)
         :directory (pathname-directory *owl-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; end of eval-when

(excl:defsystem :rdf (:pretty-name "RDF subsystem of SWCLOS"
                       :default-pathname #,*owl-directory*)
  (:module :utils        "../RDF/Utils")
  (:module :rdfio        "../RDF/RdfIO")
  (:module :iri          "../RDF/IRI")
  (:module :swclospackages "../RDF/packages")
  (:module :xml          "../RDF/Xml"          (:load-before-compile :swclospackages))
  (:module :rdferror     "../RDF/rdferror"     (:load-before-compile :utils :swclospackages))
  (:module :namespace    "../RDF/NameSpace"    (:load-before-compile :swclospackages :iri))
  (:module :litreal      "../RDF/Literal"      (:load-before-compile :utils :swclospackages :xml))
  (:module :rdfshare     "../RDF/RDFShare"     (:load-before-compile :swclospackages :rdfio :namespace))
  (:module :rdfparser    "../RDF/RdfParser"    (:load-before-compile :swclospackages :namespace :rdfshare))
  (:module :rdfform      "../RDF/RdfReader"    (:load-before-compile :swclospackages :rdfparser))
  (:module :rdfnode      "../RDF/node"         )
  )

(excl:defsystem :rdfs (:pretty-name "RDFS subsystem of SWCLOS"
                       :default-pathname #,*owl-directory*)
  (:module :rdf :rdf)
  (:module :slotdef      "../RDFS/SlotDef"      (:load-before-compile :rdf))
  (:module :rdfboot      "../RDFS/RDFboot"      (:load-before-compile :rdf :slotdef))
  (:module :domainrange  "../RDFS/DomainRange"  (:load-before-compile :rdf :rdfboot))
  (:module :rdfskernel   "../RDFS/RdfsKernel"   (:load-before-compile :rdf :slotdef :rdfboot))
  (:module :gxtype       "../RDFS/GxType"       (:load-before-compile :rdf :slotdef :rdfboot))
  (:module :rdfsobjects  "../RDFS/RdfsObjects"  (:load-before-compile :rdf :rdfboot :gxtype))
  (:module :gxforwardref "../RDFS/GxForwardRef" (:load-before-compile :rdf :gxtype :rdfsobjects :domainrange :rdfskernel))
  (:module :rdfscore     "../RDFS/RdfsCore"     (:load-before-compile :rdf :domainrange :rdfsobjects :rdfskernel))
  (:module :gxutils      "../RDFS/gxutils"      (:load-before-compile :rdf :rdfscore))
  (:module :rdfwriter    "../RDFS/rdfwriter"    (:load-before-compile :rdf :gxutils :gxforwardref))
  )

;;;
;;; If you indicate gx package as default-package, name collegions occur, due to creating gx package at first
;;; without shadowing, maybe.
;;;

(excl:defsystem :owl (:pretty-name "OWL subsystem of SWCLOS"
                      :default-pathname #,*owl-directory*)
  (:module :rdfs :rdfs)
  ;; OWL module
  (:module :owlerror              "owlerror"  (:load-before-compile :rdfs))
  (:module :owlkernel             "owlkernel" (:load-before-compile :rdfs))
  (:module :owlsamedifferenct     "owlsamedifferent" (:load-before-compile :rdfs))
  (:module :owlequivalentdisjoint "owlequivalentdisjoint" (:load-before-compile :rdfs))
  (:module :nnf                   "NNF"     (:load-before-compile :rdfs))
  (:module :tunify                "tunify"  (:load-before-compile :rdfs))
  (:module :subsume               "subsume" (:load-before-compile :rdfs :nnf :tunify))
  (:module :leanowl               "OWL"     (:load-before-compile :rdfs :subsume))
  )
(format t "~%;;To recompile, execute these forms:~%~s~%"
  '(excl:compile-system :owl :recompile t))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(excl:load-system    :owl))


