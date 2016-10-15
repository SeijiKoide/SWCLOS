;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL Error Module
;;;
;;; IT Program Project in Japan: 
;;:    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2003, 2004, 2006 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2009, 2010 Seiji Koide
;;;
;; History
;; -------
;; 2010.11.15    File created and error definitions are moved from OWL module

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(in-package :gx)


;;
;; Unsatisfiable Error in OWL
;;

(define-condition owl-unsatiafiable (error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (cond ((and (numberp *line-number*) (plusp *line-number*))
              (format stream "~A~%Check the form before line ~S in the file." 
                (apply #'format nil fcont args)
                *line-number*))
             (t (format stream "~A" 
                  (apply #'format nil fcont args)))))))
  (:documentation "Top error for unsatisfiability in OWL universe")
  )

(define-condition owl-unsatiafiable-warning (warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (cond ((and (numberp *line-number*) (plusp *line-number*))
              (format stream "~A~%Check the form before line ~S in the file." 
                (apply #'format nil fcont args)
                *line-number*))
             (t (format stream "~A" 
                  (apply #'format nil fcont args)))))))
  (:documentation "Top warning for unsatisfiability in OWL universe")
  )

(define-condition oneof-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "oneof condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in oneOf")
  )

(define-condition oneof-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "oneof condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in oneOf")
  )

(define-condition equivalentclass-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "equivalent class condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in equivalentClasses")
  )

(define-condition equivalentclass-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "equivalent class condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in equivalentClasses")
  )

(define-condition disjointwith-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "disjoint with condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in disjointWith")
  )

(define-condition disjointwith-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "disjoint with condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in disjointWith")
  )

(define-condition complementof-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "complement of condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in complementOf")
  )

(define-condition complementof-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "complement of condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in complementOf")
  )

(define-condition sameas-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "sameAs condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in sameAs")
  )

(define-condition sameas-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "sameAs condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in sameAs")
  )

(define-condition differentfrom-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "differentFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in differentFrom")
  )

(define-condition differentfrom-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "differentFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in differentFrom")
  )

(define-condition cardinality-constraint-condition-unsatiafiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "cardinality constraint condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable in cardinality")
  )

(define-condition cardinality-constraint-condition-unsatiafiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "cardinality constraint condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  (:documentation "unsatisfiable warning in cardinality")
  )

(define-condition forall-condition-unsatisfiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:allValuesFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition forall-condition-unsatisfiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:allValuesFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition exists-condition-unsatisfiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:someValuesFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition exists-condition-unsatisfiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:someValuesFrom condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition hasvalue-condition-unsatisfiable (owl-unsatiafiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:hasValue condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition hasvalue-condition-unsatisfiable-warning (owl-unsatiafiable-warning)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "owl:hasValue condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;

(cl:provide :owlerror)
