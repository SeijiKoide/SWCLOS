;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; NTWriter module
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright © 2004 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2008 Seiji Koide
;;
;; History
;; -------
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2008.12.11    resource-p is renamed to rdf-objectp, and resource? is renamed to object?
;; 2008.01.06    Revised
;; 2004.07.23    File created
;;
;;; ==================================================================================

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :gx
    (:export get-triple-uri write-triple write-nt))
  (require :gxutils)
  )

(in-package :gx)

;;
;; N-Triples Writer
;;

(defun get-triple-uri (resource)
  (cond ((and (symbolp resource) (nodeID? resource))
         resource)
        ;((and (rsc-object-p resource) (nodeID-p resource))
        ; (name resource))
        ((and (symbolp resource) (object? resource))
         (symbol2uri resource))
        ((and (rsc-object-p resource) (slot-boundp resource 'rdf:about))
         (iri (slot-value resource 'rdf:about)))
        ((not (anonymous-p resource))
         (symbol2uri (name resource)))))

(defun print-uri-form (uri stream)
  (write-char #\< stream)
  (net.uri:render-uri uri stream)
  (write-char #\> stream))

(defun write-triple (triple &optional (stream *standard-output*))
  (flet ((write-it (obj)
                   (when (eq obj '|rdfs:Resource|) (setq obj 'rdfs:Resource))
                   (when (eq obj |rdfs:Resource|) (setq obj rdfs:Resource))
                   (if (net.uri:uri-p obj) (print-uri-form obj stream)
                     (let ((uri (get-triple-uri obj)))
                       (cond ((symbolp uri) (prin1 uri stream))
                             (uri (print-uri-form uri stream))
                             ((string= "_" (package-name (symbol-package (name obj))))
                              (prin1 (name obj) stream))
                             ((name obj) (write (name obj) :stream stream))
                             (t (write (make-unique-nodeID "g") :stream stream)))))))
    (let ((subject (car triple))
          (predicate (second triple))
          (object (third triple)))
      (format stream "~&")
      (write-it subject)
      (write-char #\space stream)
      (write-it predicate)
      (write-char #\space stream)
      (typecase object
        (rdf:XMLLiteral (format stream "~W^^<~A>"
                          (format nil "~A" (slot-value object 'value))
                          (symbol2uri (class-name (class-of object)))))
        (rdfs:Literal (write object :stream stream))
        (rdfs:Resource (write-it object))
        (symbol (cond ((nodeID? object) (write object :stream stream))
                      ((object? object) (write-it (symbol-value object)))
                      (t (write object :stream stream))))
        (string (prin1 object stream))
        (number (prin1 object stream))
        (otherwise (write object :stream stream)))
      (format stream " .~%"))))

(defun write-nt (resource &optional (stream *standard-output*))
  (mapc #'(lambda (triple) (write-triple triple stream))
    (get-triple resource))
  (values))

;; End of module
;; --------------------------------------------------------------------

(provide :ntwriter)
