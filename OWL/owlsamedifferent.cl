;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL Same/Different Module
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
;; 2010.11.15    File created and same/different parts are moved from OWL module

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(in-package :gx)

(export 'owl-same-p)

;;;
;;;; Functional Property
;;;

;;; owl:FunctionalProperty is an instance of rdfs:Class.
;;; ----------------------------------------------------------------------------------
;;; (rdfs:Class owl:FunctionalProperty
;;;             (rdfs:label "FunctionalProperty")
;;;             (rdfs:subClassOf rdf:Property))
;;; ----------------------------------------------------------------------------------
;;; owl:FunctionalProperty does not belong to OWL universe, and an instance of 
;;; owl:FunctionalProperty may not belong to OWL universe.

(defun functional-property? (name)
  "returns true if <name> is an owl functional property name"
  (declare (inline))
  (and (boundp name) (functional-property-p (symbol-value name))))

(defun functional-property-p (obj)
  "Is this <obj> an instance of owl:FunctionalProperty?"
  ;;this is the same as '(cl:typep <obj> owl:FunctionalProperty)'
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value 
                           (symbol-value 'owl:FunctionalProperty))))
               ((mop:class-finalized-p class)
                (and (member (load-time-value
                              (symbol-value 'owl:FunctionalProperty))
                             (mop:class-precedence-list class)
                             :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member (load-time-value
                                            (symbol-value 'owl:FunctionalProperty))
                                           supers
                                           :test #'eq)
                               (return-from functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))

;;;
;;;; Inverse Functional Property
;;; owl:InverseFunctionalProperty is an instance of rdfs:Class.
;;; ----------------------------------------------------------------------------------
;;; (rdfs:Class owl:InverseFunctionalProperty
;;;             (rdfs:label "InverseFunctionalProperty")
;;;             (rdfs:subClassOf owl:ObjectProperty))
;;; ----------------------------------------------------------------------------------
;;; An instance of owl:InversefunctionalProperty may not be an instance of 
;;; owl:ObjectProperty. Then, the range value may not be owl:Thing and may be rdfs:Literal.

(defun inverse-functional-property-p (obj)
  "Is this <obj> an instance of owl:InverseFunctionalProperty?"
  ;;this is the same as '(cl:typep <obj> owl:InverseFunctionalProperty)'
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value 
                          (symbol-value 'owl:InverseFunctionalProperty))))
               ((not (mop:class-finalized-p class))
                (labels ((walk-partial-cpl (c)
                           (let ((supers (mop:class-direct-superclasses c)))
                             (when (member
                                    (load-time-value 
                                     (symbol-value 'owl:InverseFunctionalProperty))
                                    supers
                                    :test #'eq)
                               (return-from inverse-functional-property-p t))
                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))
               (t (and (member (load-time-value 
                                (symbol-value 'owl:InverseFunctionalProperty))
                               (mop:class-precedence-list class)
                               :test #'eq)
                       t))))))

(defun functional-property-equal-p  (x y &key (test #'definitely-%owl-same-p))
  ;; rdfp1 ter Horst
  (not (not (intersection (slot-value x 'funprop-inverse) (slot-value y 'funprop-inverse)
                          :test #'(lambda (xx yy)
                                    (and (eq (car xx) (car yy))                  ; funprop
                                         (funcall test (cdr xx) (cdr yy))))))))  ; object

(defun inverse-functional-property-equal-p (x y &key (test #'definitely-%owl-same-p))
  ;; rdfp2 cf. ter Horst
  (not (not 
        (some #'(lambda (role)
                  (let ((fil1 (slot-value x role))
                        (fil2 (slot-value y role)))
                    (cond ((equal fil1 fil2) t)
                          ; if fillers are string and equal, then equal (OWL-Full specs)
                          ((null fil1) nil)
                          ((null fil2) nil)
                          ((and (consp fil1) (consp fil2)) (intersection fil1 fil2 :test test))
                          ((consp fil1) (member fil2 fil1 :test test))
                          ((consp fil2) (member fil1 fil2 :test test))
                          ((funcall test fil1 fil2)))))
              (intersection (collect-owl-role-name-if #'inverse-functional-property-p x)
                            (collect-owl-role-name-if #'inverse-functional-property-p y))))))

;;;
;;;; sameAs
;;;

;; slot same-as includes same individuals including itself against instance.
(defun shared-initialize-after-for-sameAs (instance newsames)
  ;(format t "~%shared-initialize-after-for-sameAs(~S ~S)" instance newsames)
  (let ((oldsames (slot-value instance 'same-as)))
    ;(format t "~%Oldsames:~S" oldsames)
    (let ((sames (adjoin instance (union oldsames newsames)))
          (diffs ()))
      (cond ((setq diffs (intersection sames sames :test #'definitely-owl-different-p))
             (error 'sameas-condition-unsatiafiable 
               :format-control "Different pairs are found in ~S."
               :format-arguments `(,diffs)))
            (t (mapc #'(lambda (s) (when (owl-thing-p s)
                                     (setf (slot-value s 'same-as) sames)))
                 sames))))))

(excl:without-redefinition-warnings
(defun definitely-%owl-same-p (x y &optional pairs)
  "returns true if <x> and <y> is defined as the same in OWL semantics.
   <x> and <y> should be neither a symbol nor an iri."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and pairs
              (or (member (cons x y) pairs :test #'equal)
                  (member (cons y x) pairs :test #'equal)))
         nil)    ;; occurence check
        ((equalp x y))   ; treats 1 = 1.0, "a" = "A"
        ((and (owl-thing-p x) (owl-thing-p y)
              (or (and (name x) (name y) (eq (name x) (name y)))
                  (member x (same-as-of y)
                          :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs)))))
              t))
        ((and (rsc-object-p x) (rsc-object-p y)
              (or (and (name x) (name y) (eq (name x) (name y)))
                  ;; rdfp1 by ter Horst
                  (functional-property-equal-p
                   x y :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs))))
                  ;; rdfp2 by ter Horst
                  (inverse-functional-property-equal-p
                   x y :test #'(lambda (a b) (definitely-%owl-same-p a b (cons (cons x y) pairs))))))
         t)
        ((and (cl:typep x 'rdf:inLang) (cl:typep y 'rdf:inLang))
         (and (eq (lang x) (lang y)) (equal (content x) (content y))))
        ((and (cl:typep x 'rdf:XMLLiteral) (cl:typep y 'rdf:XMLLiteral))
         (and (eq (class-of x) (class-of y)) (equal (value-of x) (value-of y))))))
)

(defun definitely-owl-same-p (x y &optional pairs)
  "returns true if <x> and <y> is defined as the same in OWL semantics.
   <x> and <y> may be a symbol or an iri."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((definitely-%owl-same-p x y pairs))
        ((and (symbolp x) (object? x)) (definitely-owl-same-p (symbol-value x) y pairs))
        ((and (symbolp y) (object? y)) (definitely-owl-same-p x (symbol-value y) pairs))
        ((iri-p x) (definitely-owl-same-p (iri-value x) y pairs))
        ((iri-p y) (definitely-owl-same-p x (iri-value y) pairs))))

(excl:without-redefinition-warnings
(defmethod %owl-same-p ((x rdfs:Resource) (y rdfs:Resource) &optional pairs)
  "Non-resolution version. This is used in <owl-equalp> and <owl-equivalent-p>."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((definitely-%owl-same-p x y pairs))
        ((and (owl-thing-p x) (owl-thing-p y)
              (or (definitely-%%owl-different-p x y)
                  (definitely-%disjoint-p (class-of x) (class-of y))))
         nil)
        (t ;; else check in rdf graph equality
         (%rdf-equalp x y))
        ))
)

(defun definitely-%%owl-different-p (x y)
  (cond ((member x (slot-value y 'different-from))
         (values t t))
        ((and (name x) (name y))
         (cond (*nonUNA*
                (if (eql (name x) (name y)) (values nil t) (values t t)))
               (t (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                    (if graph (values (not result) graph) (values nil nil))))))
        (t (multiple-value-bind (result graph) (rdf-graph-equalp x y)
             (if graph (values (not result) graph) (values nil nil))))))

(defun definitely-%disjoint-p (c1 c2)
  (some #'(lambda (c1super)
            (some #'(lambda (c2super)
                      (or (and (slot-exists-p c1super 'disjoint-classes) (slot-boundp c1super 'disjoint-classes)
                               (member c2super (slot-value c1super 'disjoint-classes)))
                          (and (slot-exists-p c2super 'disjoint-classes) (slot-boundp c2super 'disjoint-classes)
                               (member c1super (slot-value c2super 'disjoint-classes)))))
                  (mop:class-precedence-list c2)))
        (mop:class-precedence-list c1)))

;;;
;;;; owl-different-p
;;;

(defun different-from-of (y)
  (and (slot-exists-p y 'different-from)
       (slot-boundp y 'different-from)
       (slot-value y 'different-from)))

(defun definitely-%owl-different-p (x y)
  (cond ((equal x y) nil)
        ((and (numberp x) (numberp y))
         (if (= x y) nil t))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (eq (class-of x) (class-of y))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 nil t)
           t))
        ((and (owl-thing-p x) (owl-thing-p y))
         (cond ((member x (slot-value y 'different-from))
                t)
               ((and (name x) (name y))
                (if (eql (name x) (name y)) (values nil t) (values t t)))  ; <--
               (t nil)))                                                   ; <--
        ((and (typep x rdfs:Literal) (typep y rdfs:Literal))
         ;; fall here when not equal
         t)
        (t nil)))

(defun %owl-different-p (x y)
  (cond ((equal x y) (values nil t))
        ((and (numberp x) (numberp y))
         (if (= x y) (values nil t) (values t t)))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (eq (class-of x) (class-of y))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 (values nil t)
               (values t t))
           (values t t)))
        ((and (owl-thing-p x) (owl-thing-p y))
         (definitely-%%owl-different-p x y))
        ((and (typep x rdfs:Literal) (typep y rdfs:Literal))
         ;; fall here when not equal
         (values t t))
        (t (values nil nil))))

(defun owl-different-p (x y)
  "Is <x> different from <y> as individual in semantics of OWL?"
  ;; not same does not imply difference in open world.
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y) (values nil t))
        ((and (numberp x) (numberp y))
         (if (= x y) (values nil t) (values t t)))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (eq (class-of x) (class-of y))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 (values nil t)
               (values t t))
           (values t t)))
        ((and (property-p x) (property-p y) (member x (equivalent-property-of y)))
         (values nil t))
        ((and (owl-restriction-p x) (owl-restriction-p y))
         (if (%owl-restriction-equal x y)
             (values nil t)
           (values t t)))
        ((and (owl-oneof-p x) (owl-oneof-p y))
         (if (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf))
             (values nil t) (values t t)))
        ((and (owl-class-p x) (owl-class-p y)
              (or (%owl-equivalent-p x y)
                  (%intersection-equivalent x y)
                  (%union-equivalent x y)
                  (%complemently-equal x y)))
         (values nil t))
        ((and (owl-thing-p x) (owl-thing-p y))
         (cond ((member x (slot-value y 'different-from))
                (values t t))
               ((member x (append (mklist (and (slot-boundp y 'owl:sameAs)
                                               (slot-value y 'owl:sameAs)))
                                     (%same-as-of y)) :test #'owl-equalp)
                (values nil t))
               ((member y (append (mklist (and (slot-boundp x 'owl:sameAs)
                                               (slot-value x 'owl:sameAs)))
                                     (%same-as-of x)) :test #'owl-equalp)
                (values nil t))
               ((and (name x) (name y))
                (cond (*nonUNA* (rdf-graph-different-p x y))
                      (t (if (eql (name x) (name y)) (values nil t) (values t t)))))
               (t (rdf-graph-different-p x y))))
        ;;
        ((and (typep x rdfs:Literal) (typep y rdfs:Literal))
         (values t t))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y))
                (cond (*nonUNA* (rdf-graph-different-p x y))
                      (t (if (eql (name x) (name y)) (values nil t) (values t t)))))
               (t (rdf-graph-different-p x y))))
        ((and (symbolp x) (symbolp y))
         (cond ((and (object? x) (object? y))
                (%owl-different-p (symbol-value x) (symbol-value y)))
               (t (values nil nil))))
        ((and (symbolp y) (object? y))
         (%owl-different-p x (symbol-value y)))
        ((and (symbolp x) (object? x))
         (%owl-different-p (symbol-value x) y))
        ((and (iri-p x) (iri-p y))
         (cond ((uri= x y) (values nil t))
               (t (%owl-different-p (iri-value x) (iri-value y)))))
        ((iri-p y)
         (owl-different-p x (iri-value y)))
        ((iri-p x)
         (owl-different-p (iri-value x) y))
        (t (values nil nil))))

(defun definitely-owl-different-p (x y)
  "returns true if <x> and <y> are definitely different and have no possibility of unification."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (stringp x) (stringp y))
         (if (string= x y) nil t))
        ((and (numberp x) (numberp y))
         (if (= x y) nil t))                           ; 1 and 1.0
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (if (or (cl:subtypep (class-of x) (class-of y))
                 (cl:subtypep (class-of y) (class-of x)))
             (if (equalp (value-of x) (value-of y))    ; equalp treats 1 and 1.0
                 nil t)
           t))   ; disjoint data type
        ((and (typep x rdfs:Literal) (typep y rdfs:Literal))
         t)
        ((and (owl-restriction-p x) (owl-restriction-p y))
         (if (%owl-restriction-equal x y)
             nil t))
        ((and (owl-oneof-p x) (owl-oneof-p y))
         (if (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf))
             nil t))
        ((and (property-p x) (property-p y)
              (member x (equivalent-property-of y)))
         nil) ; else goto next
        ((and (owl-class-p x) (owl-class-p y)
              (or (%owl-equivalent-p x y)
                  (%intersection-equivalent x y)
                  (%union-equivalent x y)
                  (%complemently-equal x y)))
         nil) ; else goto next
        ;; in OWL universe
        ((and (owl-thing-p x) (owl-thing-p y))
         (cond ((member x (slot-value y 'different-from))
                t)
               ((member x (append (mklist (and (slot-boundp y 'owl:sameAs)
                                               (slot-value y 'owl:sameAs)))
                                     (%same-as-of y)) :test #'owl-equalp)
                nil)
               ((member y (append (mklist (and (slot-boundp x 'owl:sameAs)
                                               (slot-value x 'owl:sameAs)))
                                     (%same-as-of x)) :test #'owl-equalp)
                nil)
               ((and (name x) (name y))
                (if (eql (name x) (name y)) nil nil))    ; <--
               (t nil)))                                 ; <--
        ;;
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y))
                (if (eql (name x) (name y)) nil nil))     ; <--
               (t nil)))                                  ; <--
        ;;
        ((and (symbolp x) (symbolp y))
         (cond ((and (object? x) (object? y))
                (definitely-%owl-different-p (symbol-value x) (symbol-value y)))
               (t nil)))
        ((and (symbolp y) (object? y))
         (definitely-%owl-different-p x (symbol-value y)))
        ((and (symbolp x) (object? x))
         (definitely-%owl-different-p (symbol-value x) y))
        ((and (iri-p x) (iri-p y))
         (cond ((uri= x y) nil)
               (t (definitely-%owl-different-p (iri-value x) (iri-value y)))))
        ((iri-p y)
         (definitely-owl-different-p x (iri-value y)))
        ((iri-p x)
         (definitely-owl-different-p (iri-value x) y))
        (t nil)))

;;;
;;;; differentFrom
;;;

;; slot different-from includes different individuals against instance.
(defun shared-initialize-after-for-differentFrom (instance differents)
  (let ((same (find instance differents :test #'%owl-same-p)))
    (cond (same (error 'differentfrom-condition-unsatiafiable
                  :format-control "~S is same as ~S."
                  :format-arguments `(,instance ,same)))
          (t (setf (slot-value instance 'different-from)
               (union (slot-value instance 'different-from) differents))
             (loop for dif in differents
                 do (pushnew instance (slot-value dif 'different-from))))))) ; rule13

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;

(cl:provide :owlsamedifferent)
