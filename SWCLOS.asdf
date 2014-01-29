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
;;; Copyright © 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009 Seiji Koide

;;; ASDF system definition.
;;; This file must be used without compiling.

(defpackage gx-system (:use :common-lisp :asdf))  
 
(in-package :gx-system)

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

(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system "owl" nil)
    (defparameter *owl-directory*
      (merge-pathnames
       (make-pathname
        :directory (append (pathname-directory *swclos-directory*)
                           (list "OWL")))
       *swclos-directory*))
    (setf (logical-pathname-translations "OWL")
      `(("**;*.*"
         ,(make-pathname
           :host (pathname-host *owl-directory*)
           :device (pathname-device *owl-directory*)
           :directory (append (pathname-directory *owl-directory*)
                              (list :wild-inferiors))
           :name :wild
           :type :wild
           ))))
    (load "OWL:OWL.asdf"))
)

;(defmethod source-file-type ((c cl-source-file) (s module)) "cl")

(defsystem :swclos
    :name "SWCLOS"
  :author "Seiji Koide <SeijiKoide@aol.com>"
  :maintainer "Seiji Koide <SeijiKoide@aol.com>"
  :version "2.0.0"
  :licence "SWCLOS"
  :description "SWCLOS is an OWL Full processor on top of CLOS."
  :long-description "This code is written at Galaxy Express Corporation, Japan, for the realization of the MEXT IT Program in Japan, and is maintained by Seiji Koide."
  :depends-on ("owl")
  :in-order-to ((compile-op (load-op "owl"))  
                (load-op (load-op "owl")))
  :components
  ((:module "ntriple"
            :components
            ((:file "Ntriple")
             (:file "NTparser")
             (:file "ntwriter")))))

(in-package #:cl-user)
(format t "~%=========== System Description ================")
(describe (asdf:find-system :swclos))
(format t "===============================================~%")
(format t "~%;;To compile, execute these forms:~%~s or~%~s or~%~s or~%~s"
  '(asdf:operate 'asdf:compile-op :rdf)
  '(asdf:operate 'asdf:compile-op :rdfs)
  '(asdf:operate 'asdf:compile-op :owl)
  '(asdf:operate 'asdf:compile-op :swclos))

(format t "~%;;To load, execute these forms:~%~s or~%~s or~%~s or~%~s"
  '(asdf:operate 'asdf:load-op :rdf)
  '(asdf:operate 'asdf:load-op :rdfs)
  '(asdf:operate 'asdf:load-op :owl)
  '(asdf:operate 'asdf:load-op :swclos))
