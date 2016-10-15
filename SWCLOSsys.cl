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
;;; Copyright (c) 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ACL Defsystem Definition for RDF + RDFS + OWL + NTriple

(eval-when (:load-toplevel :execute)
  (defparameter *swclos-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "SWCLOS")
    `(("**;*.*"
       ,(make-pathname
         :host (pathname-host *swclos-directory*)
         :device (pathname-device *swclos-directory*)
         :directory (append (pathname-directory *swclos-directory*)
                            (list :wild-inferiors))
         :name :wild
         :type :wild
         ))))
  ) ; end of eval-when

(excl:defsystem :RDF (:pretty-name "RDF subsystem of SWCLOS"
                       :default-pathname #,*swclos-directory*)
  (:module :packages     "RDF/packages")
  (:module :Utils        "RDF/Utils"        (:load-before-compile :packages))
  (:module :RdfIO        "RDF/RdfIO"        (:load-before-compile :packages))
  (:module :IRI          "RDF/IRI"          (:load-before-compile :packages))
  (:module :Xml          "RDF/Xml"          (:load-before-compile :packages))
  (:module :rdferror     "RDF/rdferror"     (:load-before-compile :Utils :packages))
  (:module :NameSpace    "RDF/NameSpace"    (:load-before-compile :IRI :packages))
  (:module :Literal      "RDF/Literal"      (:load-before-compile :Utils :packages :Xml))
  (:module :RDFShare     "RDF/RDFShare"     (:load-before-compile :packages :RdfIO :NameSpace))
  (:module :RdfParser    "RDF/RdfParser"    (:load-before-compile :packages :NameSpace :RDFShare))
  (:module :RdfReader    "RDF/RdfReader"    (:load-before-compile :packages :RdfParser))
  (:module :node         "RDF/node"         )
  )

(excl:defsystem :RDFS (:pretty-name "RDFS subsystem of SWCLOS"
                       :default-pathname #,*swclos-directory*)
  (:module :RDF :RDF)
  (:module :SlotDef      "RDFS/SlotDef"      (:load-before-compile :RDF))
  (:module :RDFboot0     "RDFS/RDFboot0"     (:load-before-compile :RDF :SlotDef))
  (:module :RDFboot1     "RDFS/RDFboot1"     (:load-before-compile :RDF :RDFboot0))
  (:module :RDFboot      "RDFS/RDFboot"      (:load-before-compile :RDF :RDFboot1))
  (:module :GxType0      "RDFS/GxType0"      (:load-before-compile :RDF :SlotDef :RDFboot))
  (:module :GxType       "RDFS/GxType"       (:load-before-compile :RDF :GxType0))
  (:module :DomainRange  "RDFS/DomainRange"  (:load-before-compile :RDF :RDFboot))
  (:module :RdfsObjects  "RDFS/RdfsObjects"  (:load-before-compile :RDF :RDFboot :GxType))
  (:module :RdfsKernel   "RDFS/RdfsKernel"   (:load-before-compile :RDF :SlotDef :RDFboot))
  (:module :GxForwardRef "RDFS/GxForwardRef" (:load-before-compile :RDF :GxType :RdfsObjects :DomainRange :RdfsKernel))
  (:module :RdfsCore     "RDFS/RdfsCore"     (:load-before-compile :RDF :DomainRange :RdfsObjects :RdfsKernel))
  (:module :gxutils      "RDFS/gxutils"      (:load-before-compile :RDF :RdfsCore))
  (:module :RDFwriter    "RDFS/rdfwriter"    (:load-before-compile :RDF :gxutils :GxForwardRef))
  )

;;;
;;; If you indicate gx package as default-package, name collegions occur, due to creating gx package at first
;;; without shadowing, maybe.
;;;

(excl:defsystem :OWL (:pretty-name "OWL subsystem of SWCLOS"
                      :default-pathname #,*swclos-directory*)
  (:module :RDFS :RDFS)
  ;; OWL module
  (:module :owlerror  "OWL/owlerror"  (:load-before-compile :RDFS))
  (:module :owlkernel "OWL/owlkernel" (:load-before-compile :RDFS))
  (:module :owlsamedifferenct "OWL/owlsamedifferent" (:load-before-compile :RDFS))
  (:module :owlequivalentdisjoint "OWL/owlequivalentdisjoint" (:load-before-compile :RDFS))
  (:module :NNF     "OWL/NNF"     (:load-before-compile :RDFS))
  (:module :tunify  "OWL/tunify"  (:load-before-compile :RDFS))
  (:module :subsume "OWL/subsume" (:load-before-compile :RDFS :NNF :tunify))
  (:module :OWL     "OWL/OWL"     (:load-before-compile :RDFS :subsume))
  )

(excl:defsystem :NTriple (:pretty-name "NTriple subsystem of SWCLOS"
                                       :default-pathname #,*swclos-directory*)
  (:module :RDFS :RDFS)
  ;; NTriple module
  (:module :ntparser "NTriple/ntparser" (:load-before-compile :RDFS))
  (:module :ntriple  "NTriple/ntriple" (:load-before-compile :RDFS))
  (:module :ntwriter "NTriple/ntwriter" (:load-before-compile :RDFS))
  )

(excl:defsystem :swclos (:pretty-name "SWCLOS: A Semantic Web Processor on CLOS"
                                      :default-pathname #,*swclos-directory*)
  (:module :RDF :RDF)
  (:module :RDFS :RDFS)
  (:module :OWL :OWL)
  (:module :NTriple :NTriple)
  )

(format t "~%;=========== System Description ================")
(show-system :swclos)
(format t ";===============================================~%")

(format t "~%;;To recompile, execute this form:~%~s~%"
  '(excl:compile-system :swclos :recompile t))

(format t "~%;;To load, execute this form:~%~s~%"
  '(excl:load-system    :swclos))

