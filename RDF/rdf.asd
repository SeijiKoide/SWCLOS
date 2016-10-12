;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: asdf; Base: 10; Lowercase: Yes -*-
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
;;; Copyright (c) 2007, 2008, 2009, 2011 Seiji Koide

;;; RDF system definition by ASDF.
;;;
;;; This file must be located at RDF directory that includes many RDF related files.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)

(eval-when (:load-toplevel :execute)
  (defparameter *rdf-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "RDF")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *rdf-directory*)
         :device (pathname-device *rdf-directory*)
         :directory (pathname-directory *rdf-directory*)
         :name :wild
         :type :wild
         ))))
) ; End of eval-when

(defsystem :rdf
    :name "SWCLOS RDF subsystem"
  :author "Seiji Koide <koide@nii.ac.jp>"
  :maintainer "Seiji Koide <koide@nii.ac.jp>"
  :version "2.0.0"
  :licence "SWCLOS"
  :description "RDF subsystem of SWCLOS (an OWL Full processor on top of CLOS)."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan."
  :depends-on (puri flexi-streams closer-mop)
  :pathname #+(and :asdf (not :asdf2)) (translate-logical-pathname "RDF:")
            #+(and :asdf :asdf2)       nil
  :default-component-class cl-source-file.cl
  :components
  ((:file "packages")
   (:file "Utils"        :depends-on ("packages"))
   (:file "RdfIO"        :depends-on ("packages"))
   (:file "IRI"          :depends-on ("packages"))
   (:file "Xml"          :depends-on ("packages"))
   (:file "rdferror"     :depends-on ("Utils" "packages"))
   (:file "NameSpace"    :depends-on ("packages" "IRI"))
   (:file "Literal"      :depends-on ("packages" "Xml"))
   (:file "RDFShare"     :depends-on ("packages" "RdfIO" "NameSpace"))
   (:file "RdfParser"    :depends-on ("packages" "NameSpace" "RDFShare"))
   (:file "RdfReader"    :depends-on ("packages" "RdfParser"))
   (:file "node"         ))
)

(in-package #:cl-user)

(format t "~%;; To compile RDF module, execute these forms:~%;; ~s~%"
  '(asdf:operate 'asdf:compile-op :rdf))

(format t "~%;; To load RDF module, execute these forms:~%;; ~s~%"
  '(asdf:operate 'asdf:load-op :rdf))
