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
;;; Copyright © 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ASDF system definition.
;;;
;;; This file must be located at RDFS directory that includes many RDFS related files.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)  

(eval-when (:load-toplevel :execute)
  (defparameter *rdfs-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "RDFS")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *rdfs-directory*)
         :device (pathname-device *rdfs-directory*)
         :directory (pathname-directory *rdfs-directory*)
         :name :wild
         :type :wild
         ))))
) ; end of eval-when

(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system "rdf" nil)
    (defparameter *rdf-directory*
      (merge-pathnames
       (make-pathname
        :directory (substitute "RDF" "RDFS"
                               (pathname-directory *rdfs-directory*)
                               :test #'string=))
       *rdfs-directory*))
    (setf (logical-pathname-translations "RDF")
      `(("*.*"
         ,(make-pathname
           :host (pathname-host *rdf-directory*)
           :device (pathname-device *rdf-directory*)
           :directory (pathname-directory *rdf-directory*)
           :name :wild
           :type :wild
           ))))
    (load "RDF:RDF.asdf"))
)

;(defmethod source-file-type ((c cl-source-file) (s module)) "cl")

(defsystem :rdfs
    :name "SWCLOS RDFS system"
  :author "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer "Seiji Koide <SeijiKoide@aol.com>"
  :version "2.0.0"
  :licence "SWCLOS"
  :description "RDFS subsystem of SWCLOS (an OWL Full processor on top of CLOS)."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan."
  :depends-on ("rdf")
  :in-order-to ((compile-op (load-op "rdf"))  
                (load-op (load-op "rdf")))
  :pathname #+(and :asdf (not :asdf2)) (translate-logical-pathname "RDFS:")
            #+(and :asdf :asdf2)       nil
  :components
  ((:file "SlotDef"      :depends-on ())
   (:file "RDFboot"      :depends-on ("SlotDef"))
   (:file "DomainRange"  :depends-on ("RDFboot"))
   (:file "RdfsKernel"   :depends-on ("SlotDef" "RDFboot"))
   (:file "GxType"       :depends-on ("SlotDef" "RDFboot"))
   (:file "RdfsObjects"  :depends-on ("RDFboot" "GxType"))
   (:file "GxForwardRef" :depends-on ("GxType" "RdfsObjects" "DomainRange" "RdfsKernel"))
   (:file "RdfsCore"     :depends-on ("DomainRange" "RdfsObjects" "RdfsKernel"))
   (:file "gxutils"      :depends-on ("RdfsCore"))
   (:file "rdfwriter"    :depends-on ("gxutils" "GxForwardRef"))
   )
)

(in-package #:cl-user)

(format t "~%;;To compile, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:compile-op :rdfs))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(asdf:operate 'asdf:load-op :rdfs))
