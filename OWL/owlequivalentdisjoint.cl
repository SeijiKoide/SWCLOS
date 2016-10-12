;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL Equivalent/Disjoint Module
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
;;;; Equivalency as Class
;;;
;;; owl:equivalentOf, owl:intersectionOf, owl:unionOf, owl:complementOf, and owl:oneOf are 
;;; complete relation. Therefore, at these relation the equivalency as class at righthand 
;;; side implies the equivalency as class at lefthand side.

;;;
;;;; equivalentClass
;;;

(defun shared-initialize-after-for-equivalentClass (class equivalentclasses)
  "owl:equivalentClass is a subproperty of rdfs:subClassOf, so 
  the objects are automatically captured as subclasses in nature. 
  However, this characteristics is very tricky and bad for construction of stable ontology.
  Therefore, the class is not asserted as subclass."
  (let ((oldequivs (slot-value class 'equivalent-classes))
        (newequivs equivalentclasses))
    (let ((equivs (adjoin class (union oldequivs newequivs)))
          (disjoints ()))
      (cond ((setq disjoints (intersection equivs equivs :test #'disjoint-p))
             (error 'equivalentclass-condition-unsatiafiable
               :format-control "between ~S and ~S."
               :format-arguments `(,class ,disjoints)))
            (t (mapc #'(lambda (c)
                         (when (owl-class-p c)
                           (setf (slot-value c 'equivalent-classes) equivs)))
                 equivs))))))

(defun %intersection-equivalent (x y)
  "If both <x> and <y> have an intersection of concepts and two sets of intersection are 
equivalent as class, then <x> and <y> is equivalent as class. If either or neither has an 
instersection, then returns false."
  (when (and (slot-exists-p x 'owl:intersectionOf)    ;added for Hotz's project 
             (slot-exists-p y 'owl:intersectionOf)    ;added for Hotz's project 
             (slot-boundp x 'owl:intersectionOf)
             (slot-boundp y 'owl:intersectionOf))
    (let ((interx (slot-value x 'owl:intersectionOf))
          (intery (slot-value y 'owl:intersectionOf)))
      (let ((xsupers (remove-if #'owl-restriction-p interx))
            (ysupers (remove-if #'owl-restriction-p intery))
            (xrestrictions (remove-if-not #'owl-restriction-p interx))
            (yrestrictions (remove-if-not #'owl-restriction-p intery)))
        (and (subsetp xsupers ysupers :test #'owl-equivalent-p)
             (subsetp ysupers xsupers :test #'owl-equivalent-p)
             (subsetp xrestrictions yrestrictions :test #'%owl-restriction-equal)
             (subsetp yrestrictions xrestrictions :test #'%owl-restriction-equal))))))

(defun %union-equivalent (x y)
  "If both <x> and <y> have an union of concepts and two sets of union are equivalent as class, 
   then <x> and <y> is equivalent as class."
  (when (and (slot-boundp x 'owl:unionOf)
             (slot-boundp y 'owl:unionOf))
    (let ((unionx (slot-value x 'owl:unionOf))
          (uniony (slot-value y 'owl:unionOf)))
      (and (subsetp unionx uniony :test #'owl-equivalent-p)
           (subsetp uniony unionx :test #'owl-equivalent-p)))))

(defun %complemently-equal (x y)
  ;; if both complements are set equal, then equal.
  (cond ((and (slot-boundp x 'owl:complementOf)
              (slot-boundp y 'owl:complementOf))
         (let ((xcomplement (slot-value x 'owl:complementOf))
               (ycomplement (slot-value y 'owl:complementOf)))
           (%owl-equivalent-p-without-complements xcomplement ycomplement)))
        ((and (slot-boundp x 'complement-class)
              (slot-boundp y 'complement-class))
         (let ((xcomplement (slot-value x 'complement-class))
               (ycomplement (slot-value y 'complement-class)))
           (%owl-equivalent-p-without-complements xcomplement ycomplement)))))

(defun %oneof-equivalent (x-ones y-ones)
  "If two OneOf sets are set-equal, then both are equal as class.
  sets are test by owl-same-p."
  (and (subsetp x-ones y-ones :test #'definitely-%owl-same-p)
       (subsetp y-ones x-ones :test #'definitely-%owl-same-p)))

(defun %owl-restriction-equal (c1 c2)
  (flet ((get-slot-value (r p) (and (slot-boundp r p) (slot-value r p)))
         (set-equal-with-equivalent-p (x y) (and (subsetp x y :test #'owl-equivalent-p)
                                                 (subsetp y x :test #'owl-equivalent-p)))
         (set-equal-with-same-p (x y) (and (subsetp x y :test #'%owl-same-p)
                                           (subsetp y x :test #'%owl-same-p))))
    (flet ((value-eql (c1 c2)
                      (and (eql (class-of c1) (class-of c2))
                           (etypecase c1
                             (owl:allValuesFromRestriction
                              (set-equal-with-equivalent-p
                               (mklist (slot-value c1 'owl:allValuesFrom))
                               (mklist (slot-value c2 'owl:allValuesFrom))))
                             (owl:someValuesFromRestriction
                              (set-equal-with-equivalent-p
                               (mklist (slot-value c1 'owl:someValuesFrom))
                               (mklist (slot-value c2 'owl:someValuesFrom))))
                             (owl:hasValueRestriction
                              (set-equal-with-same-p
                               (mklist (slot-value c1 'owl:hasValue))
                               (mklist (slot-value c2 'owl:hasValue))))
                             (owl:cardinalityRestriction
                              (let ((c1max (or (get-slot-value c1 'owl:cardinality)
                                               (get-slot-value c1 'owl:maxCardinality)
                                               most-positive-fixnum))
                                    (c1min (or (get-slot-value c1 'owl:cardinality)
                                               (get-slot-value c1 'owl:minCardinality)
                                               most-negative-fixnum))
                                    (c2max (or (get-slot-value c2 'owl:cardinality)
                                               (get-slot-value c2 'owl:maxCardinality)
                                               most-positive-fixnum))
                                    (c2min (or (get-slot-value c2 'owl:cardinality)
                                               (get-slot-value c2 'owl:minCardinality)
                                               most-negative-fixnum)))
                                ;; equality at cardinalities
                                (setq c1min (value-of c1min))
                                (setq c2min (value-of c2min))
                                (setq c1max (value-of c1max))
                                (setq c2max (value-of c2max))
                                (and (= c1min c2min) (= c1max c2max))))))))
      (and (eq (slot-value c1 'owl:onProperty) (slot-value c2 'owl:onProperty))
           (value-eql c1 c2)             ;if empty t
           ))))

(defun %owl-equivalent-p-without-type-equivalents (x y)
  (cond ((intersection (%equivalent-classes-of x) (list y) ; seiji 2009.01.11
                       :test #'%owl-equivalent-p-without-equivalents)
         t)
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p x y :test #'%owl-equivalent-p))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p x y :test #'%owl-equivalent-p))
        ))

(excl:without-redefinition-warnings
(defun owl-equivalent-p (x y)
  "returns true if <x> and <y> are equivalent classes."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))         ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal. 
               ;;name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
               ((%owl-equivalent-p x y))
               ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names 
               (t   ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (owl-equivalent-p (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (owl-equivalent-p (symbol-value x) y))
        ((and (symbolp y) (object? y)) (owl-equivalent-p x (symbol-value y)))
        ((and (uri-p x) (uri-p y) (uri= x y))) ; or go through next
        ((and (iri-p x) (iri-p y))                             ; uri-string different but
         (cond ((and *nonUNA* (iri-boundp x) (iri-boundp y))   ; if nonUNA and has value
                (owl-equivalent-p (iri-value x) (iri-value y)))      ; then check values
               (t nil)))                            ; else different uri means different
        ((and (iri-p x) (iri-boundp x)) (owl-equivalent-p (iri-value x) y))
        ((and (iri-p y) (iri-boundp y)) (owl-equivalent-p x (iri-value y)))
        ((and (consp x) (consp y))
         (and (owl-equivalent-p (car x) (car y))
              (owl-equivalent-p (cdr x) (cdr y))))))
)

(defun definitely-owl-equivalent-p (x y)
  "returns true if <x> and <y> are equivalent classes."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))         ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal. 
               ;;name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
               ((%owl-equivalent-p x y))
               ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (definitely-%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (definitely-%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (name x) (name y)) nil) ; <--
               (t nil)))                     ; <--
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (definitely-owl-equivalent-p (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (definitely-owl-equivalent-p (symbol-value x) y))
        ((and (symbolp y) (object? y)) (definitely-owl-equivalent-p x (symbol-value y)))
        ((and (uri-p x) (uri-p y) (uri= x y))) ; or go through next
        ((and (iri-p x) (iri-p y))                             ; uri-string different but
         (definitely-owl-equivalent-p (iri-value x) (iri-value y))) ; <--
        ((and (iri-p x) (iri-boundp x)) (definitely-owl-equivalent-p (iri-value x) y))
        ((and (iri-p y) (iri-boundp y)) (definitely-owl-equivalent-p x (iri-value y)))
        ((and (consp x) (consp y))
         (and (definitely-owl-equivalent-p (car x) (car y))
              (definitely-owl-equivalent-p (cdr x) (cdr y))))))

#|
(owl-equivalent-p food:Wine vin:Wine)
(owl-equivalent-p vin:TableWine vin:DryWine)
|#

(defun %owl-equivalent-p (x y)
  "checks owl:equivalentOf."
  (cond ((intersection (%equivalent-classes-of x) (%equivalent-classes-of y) ; seiji 2009.01.11
                       :test #'%owl-equivalent-p-without-equivalents)
         t)
        ;; rdfp1 by ter Horst
        ((functional-property-equal-p x y :test #'%owl-equivalent-p))
        ;; rdfp2 by ter Horst
        ((inverse-functional-property-equal-p x y :test #'%owl-equivalent-p))
        ))

(defun %owl-equivalent-p-without-equivalents (x y)
  "This subsubfunction prevents recursive call for %owl-equivalent-p."
  (declare (optimize (speed 3) (safety 0)))
  ;(error "Check it")
  (cond ((equal x y))            ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal. 
               ;; name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
  ;;;;;;;;;;;  ((%owl-equivalent-p x y))
               ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names 
               (t     ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (consp x) (consp y))
         (and (%owl-equivalent-p-without-equivalents (car x) (car y))
              (%owl-equivalent-p-without-equivalents (cdr x) (cdr y))))))

(defun %owl-equivalent-p-without-complements (x y)
  "returns true if <x> and <y> are equivalent classes or equal literals, symbols,
   and objects. At first, <x> and <y> should be a class."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))      ; symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (owl-class-p x) (owl-class-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal. 
               ;; name may be a cons
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf)))
               ((%intersection-equivalent x y)) ; if intersection slot is equal, then equivalent
               ((%union-equivalent x y))        ; if unionOf slot is equal, then equivalent
               ((%owl-equivalent-p x y))
  ;;;;;;;;;;;  ((%complemently-equal x y))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names 
               (t     ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (consp x) (consp y))
         (and (%owl-equivalent-p-without-complements (car x) (car y))
              (%owl-equivalent-p-without-complements (cdr x) (cdr y))))))


;;;
;;;; Disjointness
;;;
;;; When equivalent classes are given for arguments as disjoint classes, the unsatisfiable error 
;;; happens. Note that two concepts in disjointness cannot make intersection, and multiple 
;;; classing.

(defun shared-initialize-after-for-disjointWith (class disjoints)
  (let ((equiv (find class disjoints :test #'owl-equivalent-p)))
    (cond (equiv (cerror  "Anyway accept this inconsistency on disjointness."
                         'disjointwith-condition-unsatiafiable
                         :format-control "~S is equivalent to ~S."
                         :format-arguments `(,class ,equiv))
                 (warn 'disjointwith-condition-unsatiafiable-warning
                   :format-control "~S is equivalent to ~S."
                   :format-arguments `(,class ,equiv))
                 (loop for disjoint in disjoints with result
                     do (cond ((setq result (check-instance-sharing class disjoint))
                               (cerror "Anyway accept this inconsistency on disjointness."
                                       'disjointwith-condition-unsatiafiable
                                       :format-control "~S has super-sub relation to ~S."
                                       :format-arguments `(,(class-name class) ,(class-name result)))
                               (warn 'disjointwith-condition-unsatiafiable-warning
                                 :format-control "~S has super-sub relation to ~S, but you accepted it."
                                 :format-arguments `(,(class-name class) ,(class-name result)))
                               (pushnew disjoint (slot-value class 'disjoint-classes))
                               (pushnew class (slot-value disjoint 'disjoint-classes)))
                              (t (pushnew disjoint (slot-value class 'disjoint-classes))
                                 (pushnew class (slot-value disjoint 'disjoint-classes))))))
          (t (loop for disjoint in disjoints with result
                 do (cond ((setq result (check-instance-sharing class disjoint))
                           (cerror "Anyway accept this inconsistency on disjointness."
                                   'disjointwith-condition-unsatiafiable
                                   :format-control "~S has super-sub relation to ~S."
                                   :format-arguments `(,(class-name class) ,(class-name result)))
                           (warn 'disjointwith-condition-unsatiafiable-warning
                             :format-control "~S has super-sub relation to ~S, but you accepted it."
                             :format-arguments `(,(class-name class) ,(class-name result)))
                           (pushnew disjoint (slot-value class 'disjoint-classes))
                           (pushnew class (slot-value disjoint 'disjoint-classes)))
                          (t (pushnew disjoint (slot-value class 'disjoint-classes))
                             (pushnew class (slot-value disjoint 'disjoint-classes)))))))))

;;;
;;; Not only from the disjoint statement, but also owl:intersectionOf values and owl:oneOf 
;;; values decide the disjointness. Namely, if two values for such complete relational properties 
;;; are disjoint, then the arguments are disjoint.

(defun %owl-disjoint-p (c1 c2)
  "special rules of disjointness in OWL.
   This function is used internally in routines of default reasoning for disjointness.
   Namely, it returns <nil nil> if the disjointness is explicitly not stated."
  (cond ((equal c1 c2) (values nil t))
        ((and (slot-exists-p c1 'disjoint-classes) (slot-boundp c1 'disjoint-classes)
              (member c2 (slot-value c1 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((and (slot-exists-p c2 'disjoint-classes) (slot-boundp c2 'disjoint-classes)
              (member c1 (slot-value c2 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((cl:subtypep c1 c2) (values nil t))
        ((cl:subtypep c2 c1) (values nil t))
        ((check-instance-sharing c1 c2) (values nil t))
        ((and (owl-restriction-p c1) (owl-restriction-p c2))
         (multiple-value-bind (val1 val2) (%restriction-disjoint-p c1 c2)
           (when val2 (return-from %owl-disjoint-p (values val1 val2))))
         (values nil nil))
        ((owl-restriction-p c1) (values nil t))
        ((owl-restriction-p c2) (values nil t))
        ((and (intersection-of c1) (intersection-of c2)
              (multiple-value-bind (val1 val2)
                  (%intersection-disjoint-p (intersection-of c1) (intersection-of c2))
                (when val2 (return-from %owl-disjoint-p (values val1 val2))))))
        ((and (owl-oneof-p c1) (owl-oneof-p c2))
         (let ((c1-ones (slot-value c1 'owl:oneOf))
               (c2-ones (slot-value c2 'owl:oneOf)))
           (cond ((intersection c1-ones c2-ones :test #'%owl-same-p)
                  (values nil t))
                 (t (values t t)))))
        (t (let ((supers1 (remove-if #'owl-restriction-p (mop:class-direct-superclasses c1)))
                 (supers2 (remove-if #'owl-restriction-p (mop:class-direct-superclasses c2))))
             ;; this recursion stops at rdfs:Resource or owl:Class
             ;; rule4
             (cond ((some #'(lambda (s) (%owl-disjoint-p s c2)) supers1)
                    (values t t))
                   ((some #'(lambda (s) (%owl-disjoint-p c1 s)) supers2)
                    (values t t))
                   (t (values nil nil)))))))

(defun check-simple-disjoint-pair-p-in-supers (name supers)
  "simply checks the satifiability among <supers> of <class>."
  (loop for c1 in supers
      do (loop for c2 in supers
             when (simple-disjoint-pair-p-in-supers c1 c2)
             do (error "Disjoint pair ~S and ~S found in supers of ~S."
                  c1 c2 (or name 'anonymous)))))
(defun simple-disjoint-pair-p-in-supers (c1 c2)
  "This predicate cannot be applicable to slot type option."
  (typecase c1
    (owl:allValuesFromRestriction
     (typecase c2
       (owl:allValuesFromRestriction
        (simple-disjoint-pair-p-in-supers
         (slot-value c1 'owl:allValuesFrom)
         (slot-value c2 'owl:allValuesFrom)))
       (otherwise nil)))
    (owl:someValuesFromRestriction 
     (typecase c2
       (owl:someValuesFromRestriction
        #| for pizza.owl FruttiDiMare
        (simple-disjoint-pair-p-in-supers
         (slot-value c1 'owl:someValuesFrom)
         (slot-value c2 'owl:someValuesFrom))
        |#
        nil)
       (otherwise nil)))
    (owl:hasValueRestriction nil)
    (otherwise 
     (typecase c2
       (owl:allValuesFromRestriction )
       (owl:someValuesFromRestriction )
       (owl:hasValueRestriction )
       (otherwise 
        (or (and (slot-exists-p c1 'disjoint-classes) (slot-boundp c1 'disjoint-classes)
                 (member c2 (slot-value c1 'disjoint-classes)))
            (and (slot-exists-p c2 'disjoint-classes) (slot-boundp c2 'disjoint-classes)
                 (member c1 (slot-value c2 'disjoint-classes)))))))))

(defun check-simple-disjoint-pair-p-in-slot-types (class types)
  "simply checks the satifiability among slot type option <types> of <class>."
  (loop for c1 in types
      do (loop for c2 in types
             when (simple-disjoint-p-in-slot-types c1 c2)
             do (error "Disjoint pair ~S and ~S found in slot inheritance computation of ~S."
                  c1 c2 class))))
(defun simple-disjoint-p-in-slot-types (c1 c2)
  "This should be called in computation of slot type option inheritance. So, 
   the role or owl:onProperty of <c1> and <c2> must be equal."
  (typecase c1
          (forall (typecase c2
                    (forall (%disjoint-p
                             (forall-filler c1) (forall-filler c2)))
                    (otherwise nil)))
          (exists (typecase c2
                    (exists (%disjoint-p
                             (exists-filler c1) (exists-filler c2)))
                    (otherwise nil)))
          (fills nil)
          (otherwise 
           (typecase c2
             (forall nil)
             (exists nil)
             (fills nil)
             (otherwise (%disjoint-p c1 c2))))))

(defun %intersection-disjoint-p (xinter yinter)
  "returns true if <xinter> and <yinter> are disjoint.
   This function returns true if either of args has intersection, 
   and returns unknown if neither of args has intersection."
  (cond ((and (null xinter) (null yinter)) (values nil nil))
        ((or (null xinter) (null yinter)) (values t t))
        (t (let ((xsupers (remove-if #'owl-restriction-p xinter))
                 (ysupers (remove-if #'owl-restriction-p yinter))
                 (xrestrictions (remove-if-not #'owl-restriction-p xinter))
                 (yrestrictions (remove-if-not #'owl-restriction-p yinter)))
             (let ((intersects (intersection xsupers ysupers))
                   (val2 t))
               (loop for xsuper in (set-difference xsupers intersects)
                   do (loop for ysuper in (set-difference ysupers intersects)
                          do (multiple-value-bind (v1 v2) (%owl-disjoint-p xsuper ysuper)
                               (when v1 (return-from %intersection-disjoint-p (values t t)))
                               (setq val2 (and v2 val2)))))
               (loop for xrestriction in xrestrictions
                   do (loop for yrestriction in yrestrictions
                          do (multiple-value-bind (v1 v2)
                                 (%owl-disjoint-p xrestriction yrestriction)
                               (when v1 (return-from %intersection-disjoint-p (values t t)))
                               (setq val2 (and v2 val2)))))
               (values nil val2))))))

(defun %restriction-disjoint-p (c1 c2)
  (let ((role1 (name (slot-value c1 'owl:onProperty)))
        (role2 (name (slot-value c2 'owl:onProperty))))
    (unless (equivalent-property-p role1 role2)
      (return-from %restriction-disjoint-p (values nil nil)))
    ;; compare only if role1 and role2 is equivalent.
    (cond ((cl:typep c1 owl:hasValueRestriction)
           (cond ((cl:typep c2 owl:someValuesFromRestriction) (values t t))
                 ((cl:typep c2 owl:allValuesFromRestriction) (values t t))
                 ((cl:typep c2 owl:hasValueRestriction)
                  (some #'(lambda (fil1)
                            (some #'(lambda (fil2)
                                      (cond ((multiple-value-bind (val1 val2)
                                                 (owl-different-p fil1 fil2)
                                               (when val2
                                                 (return-from %restriction-disjoint-p
                                                   (values val1 val2)))))
                                            ;; else nothing done
                                            ))
                                  (mklist (slot-value c2 'owl:hasValue))))
                        (mklist (slot-value c1 'owl:hasValue))))
                 ((error "Cant happen"))))
          ((cl:typep c1 owl:someValuesFromRestriction)
           (cond ((cl:typep c2 owl:hasValueRestriction) (values t t))
                 ((cl:typep c2 owl:allValuesFromRestriction) (values t t))
                 ((cl:typep c2 owl:someValuesFromRestriction)
                  (some #'(lambda (fil1)
                            (some #'(lambda (fil2) 
                                      (cond ((owl-equivalent-p fil1 fil2)
                                             (return-from %restriction-disjoint-p
                                               (values nil t)))
                                            ((multiple-value-bind (val1 val2)
                                                 (%owl-disjoint-p fil1 fil2)
                                               (when val2
                                                 (return-from %restriction-disjoint-p
                                                   (values val1 val2)))))))
                                  ;; else nothing done
                                  (mklist (slot-value c2 'owl:someValuesFrom))))
                        (mklist (slot-value c1 'owl:someValuesFrom))))
                 ((error "Cant happen"))))
          ((cl:typep c1 owl:allValuesFromRestriction)
           (cond ((cl:typep c2 owl:hasValueRestriction) (values t t))
                 ((cl:typep c2 owl:someValuesFromRestriction) (values t t))
                 ((cl:typep c2 owl:allValuesFromRestriction)
                  (if (every #'(lambda (fil1)
                                 (every #'(lambda (fil2)
                                            (when (%owl-disjoint-p fil1 fil2)
                                              (return-from %restriction-disjoint-p
                                                (values t t)))
                                            (owl-equivalent-p fil1 fil2))
                                        (mklist (slot-value c2 'owl:allValuesFrom))))
                             (mklist (slot-value c1 'owl:allValuesFrom)))
                      (values nil t)
                    (values nil nil)))
                 ((error "Cant happen"))))
          ((error "Cant happen")))))

#|
(%owl-disjoint-p vin:Zinfandel vin:Vintage)                                -> false
(%owl-disjoint-p vin:RedWine vin:WhiteWine)                                -> true
(%owl-disjoint-p vin:CaliforniaWine vin:ItalianWine)                       -> true
;; because names are different.
(let ((*nonUNA* t)) (%owl-disjoint-p vin:CaliforniaWine vin:ItalianWine))  -> true,
;; because vin:CaliforniaRegion is graph-different from vin:ItalianRegion.
|#

;;;
;;; disjointness in OWL universe is ????

(excl:without-redefinition-warnings
(defun disjoint-p (c d)
  "returns true if <c> and <d> are disjoint in OWL."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq c d) (values nil t))
        ((and c (eq d t)) (values nil t))
        ((and (eq c t) d) (values nil t))
        ((eq c rdfs:Resource) (values nil t)) ; in RDF universe
        ((eq d rdfs:Resource) (values nil t))
        ((eq d owl:Nothing) (values t t))     ; owl:Nothing shares no instances with any class.
        ((eq c owl:Nothing) (values t t))
        ((and (eq c owl:Thing) (owl-class-p d)) (values nil t)) ; owl:Thing subsumes owl classes
        ((and (owl-class-p c) (eq d owl:Thing)) (values nil t))
        ((and (consp c) (consp d))
         (case (op c)
           (and (case (op d)
                  (and (some #'(lambda (cc) (some #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                  (or (some #'(lambda (cc) (every #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                  (not (not (every #'(lambda (cc) (disjoint-p cc (arg1 d))) (args c))))
                  (satisfies (error "Not Yet!"))
                  (forall (error "Not Yet!"))
                  (exists (error "Not Yet!"))
                  (fills (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (or (case (op d)
                 (and (every #'(lambda (cc) (some #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                 (or (every #'(lambda (cc) (every #'(lambda (dd) (disjoint-p cc dd))
                                                  (args d))) (args c)))
                 (not (not (some #'(lambda (cc) (disjoint-p cc (arg1 d))) (args c))))
                 (satisfies (error "Not Yet!"))
                 (forall (error "Not Yet!"))
                 (exists (error "Not Yet!"))
                 (fills (error "Not Yet!"))
                 (otherwise (error "Not Yet!"))))
           (not (case (op d)
                  (and (error "Not Yet!"))
                  (or (error "Not Yet!"))
                  (not (error "Not Yet!"))
                  (satisfies (error "Not Yet!"))
                  (forall (error "Not Yet!"))
                  (exists (error "Not Yet!"))
                  (fills (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (satisfies (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (error "Not Yet!"))
                        (otherwise (error "Not Yet!"))))
           (forall (case (op d)
                     (and (error "Not Yet!"))
                     (or (error "Not Yet!"))
                     (not (error "Not Yet!"))
                     (satisfies (error "Not Yet!"))
                     (forall (error "Not Yet!"))
                     (exists (error "Not Yet!"))
                     (fills (error "Not Yet!"))
                     (otherwise (error "Not Yet!"))))
           (exists (case (op d)
                     (and (error "Not Yet!"))
                     (or (error "Not Yet!"))
                     (not (error "Not Yet!"))
                     (satisfies (error "Not Yet!"))
                     (forall (error "Not Yet!"))
                     (exists (error "Not Yet!"))
                     (fills (error "Not Yet!"))
                     (otherwise (error "Not Yet!"))))
           (fills (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (if *autoepistemic-local-closed-world*
                                       (when (equivalent-property-p (arg1 c) (arg1 d))
                                         (disjoint-p (arg2 c) (arg2 d)))
                                     (values nil nil)))
                        (otherwise (error "Not Yet!"))))
           (otherwise (case (op d)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (satisfies (error "Not Yet!"))
                        (forall (error "Not Yet!"))
                        (exists (error "Not Yet!"))
                        (fills (error "Not Yet!"))
                        (otherwise (error "Not Yet!"))))))
        ((consp c)
         (case (op c)
           (and (let ((cf nil))
                  (some #'(lambda (cc)
                            (multiple-value-bind (val1 val2) (disjoint-p cc d)
                              (when val1 (return-from disjoint-p (values t t)))
                              (setq cf (or cf val2))))
                        (args c))
                  (values nil cf)))
           (or  (let ((cf t))
                  (if (every #'(lambda (cc)
                                 (multiple-value-bind (val1 val2) (disjoint-p cc d)
                                   (setq cf (and cf val2))))
                             (args c))
                      (values t t)
                    (values nil cf))))
           (not (error "Not Yet!"))
           (satisfies (error "Not Yet!"))
           (forall (error "Not Yet!"))
           (exists (error "Not Yet!"))
           (fills (error "Not Yet!"))))
        ((consp d)
         (case (op d)
           (and (let ((cf nil))
                  (some #'(lambda (dd)
                            (multiple-value-bind (val1 val2) (disjoint-p c dd)
                              (when val1 (return-from disjoint-p (values t t)))
                              (setq cf (or cf val2))))
                        (args d))
                  (values nil cf)))
           (or  (let ((cf t))
                  (if (every #'(lambda (dd)
                                 (multiple-value-bind (val1 val2) (disjoint-p c dd)
                                   (setq cf (and cf val2))))
                             (args d))
                      (values t t)
                    (values nil cf))))
           (not (error "Not Yet!"))
           (satisfies (error "Not Yet!"))
           (forall (error "Not Yet!"))
           (exists (error "Not Yet!"))
           (fills (error "Not Yet!"))
           (otherwise (some #'(lambda (dd) (disjoint-p c dd)) d))))
        (t (%disjoint-p c d))))

(defun %disjoint-p (c d)
  (cond ((and (slot-exists-p c 'disjoint-classes) (slot-boundp c 'disjoint-classes)
              (member d (slot-value c 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((and (slot-exists-p d 'disjoint-classes) (slot-boundp d 'disjoint-classes)
              (member c (slot-value d 'disjoint-classes) :test #'owl-equivalent-p))
         (values t t))
        ((and (strict-class-p c)
              (or (rdf-metaclass-p d)
                  (rdf-instance-p d)))
         (values t t))
        ((and (rdf-metaclass-p c)
              (or (strict-class-p d)
                  (rdf-instance-p d)))
         (values t t))
        ((and (rdf-instance-p c)
              (rdf-class-p d))
         (values t t))
        ((and (rdf-class-p c) (rdf-class-p d))
         (cond ((or (%clos-subtype-p c d) (%clos-subtype-p d c))
                (values nil t))  ; xsd datatype is also reasoned.
               ((check-instance-sharing c d)
                (values nil t))
               ;; rdf disjoint part
               ((or (and (eq c xsd:nonPositiveInteger) (eq d xsd:nonNegativeInteger))
                    (and (eq d xsd:nonPositiveInteger) (eq c xsd:nonNegativeInteger)))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:integer))
                         (%clos-subtype-p d (symbol-value 'xsd:nonPositiveInteger)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:integer))
                         (%clos-subtype-p c (symbol-value 'xsd:nonPositiveInteger)))
                    (and (%clos-subtype-p c (symbol-value 'xsd:integer))
                         (%clos-subtype-p d (symbol-value 'xsd:nonNegativeInteger)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:integer))
                         (%clos-subtype-p c (symbol-value 'xsd:nonNegativeInteger))))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:nonPositiveInteger))
                         (%clos-subtype-p d (symbol-value 'xsd:positiveInteger)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:nonPositiveInteger))
                         (%clos-subtype-p c (symbol-value 'xsd:positiveInteger))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:nonPositiveInteger))
                         (%clos-subtype-p d (symbol-value 'xsd:unsignedLong)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:nonPositiveInteger))
                         (%clos-subtype-p c (symbol-value 'xsd:unsignedLong))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:nonNegativeInteger))
                         (%clos-subtype-p d (symbol-value 'xsd:negativeInteger)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:nonNegativeInteger))
                         (%clos-subtype-p c (symbol-value 'xsd:negativeInteger))))
                (values t t))
               ;; implicit disjointness of datatype
               ((and (%clos-subtype-p c xsd:anySimpleType)
                     (%clos-subtype-p d xsd:anySimpleType))
                (loop for csub in (cons c (collect-all-subs c)) 
                    with dsubs = (cons d (collect-all-subs d))
                    do (loop for dsub in dsubs
                           when (or (%clos-subtype-p csub dsub) (%clos-subtype-p dsub csub))
                           do (return-from %disjoint-p (values nil t))))
                (values t t))
               ;; end of rdf disjoint part
               (t (let ((val2 t))
                    (loop for cc in (equivalent-classes-of c)
                        do (loop for dd in (equivalent-classes-of d)
                               do (multiple-value-bind (v1 v2) (%owl-disjoint-p cc dd)
                                    (cond (v1 (return-from %disjoint-p (values t t)))
                                          (t (setq val2 (and val2 v2)))))))
                    (values nil val2)))))
        ((and (rdf-instance-p c) (rdf-instance-p d))
         ;; for transitive property, instances have subsumption
         (if gx::*autoepistemic-local-closed-world*
             (cond ((or (subsumed-p c d) (subsumed-p d c))
                    (values nil t))
                   (t (values t t)))
           (values nil nil)))
        (t (typecase c
             (forall (typecase d
                       (forall (cond ((%disjoint-p (forall-filler c) (forall-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (exists (cond ((%disjoint-p (forall-filler c) (exists-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (fills (cond ((typep (fills-filler d) (forall-filler c))
                                         (values nil t))
                                        (t (values nil nil))))
                       (rdfs:Class (disjoint-p (forall-filler c) d))
                       (rdfs:Resource (error "Not Yet!"))
                       (t (values nil nil))))
             (exists (typecase d
                       (exists (cond ((%owl-disjoint-p (exists-filler c) (exists-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (forall (cond ((%disjoint-p (exists-filler c) (forall-filler d))
                                      (values t t))
                                     (t (values nil nil))))
                       (fills (cond ((typep (fills-filler d) (exists-filler c))
                                         (values nil t))
                                        (t (values nil nil))))
                       (rdfs:Class (cond ((disjoint-p (exists-filler c) d)
                                          (format t "~%disjoint ~S ~S" c d)
                                          (values t t))
                                         (t (values nil nil))))
                       (rdfs:Resource (error "Not Yet!"))
                       (t (values nil nil))))
             (fills (typecase d
                          (fills (disjoint-p (fills-filler c) (fills-filler d)))
                          (forall (cond ((typep (fills-filler c) (forall-filler d))
                                         (values nil t))
                                        (t (values nil nil))))
                          (exists (cond ((typep (fills-filler c) (exists-filler d))
                                         (values nil t))
                                        (t (values nil nil))))
                          (rdfs:Class (cond ((typep (fills-filler c) d)
                                             (values nil t))
                                            (t (values nil nil))))
                          (rdfs:Resource (values nil nil))
                          (t (values nil nil))))
             (rdfs:Class (typecase d
                           (forall (disjoint-p c (forall-filler d)))
                           (exists (disjoint-p c (exists-filler d)))
                           (fills (cond ((typep (fills-filler d) c)
                                             (values nil t))
                                            (t (values nil nil))))
                           (rdfs:Class (error "Cant happen!"))
                           (t (values nil nil))))
             (rdfs:Resource (typecase d
                              (forall (values nil nil))
                              (exists (values nil nil))
                              (fills (values nil nil))
                              (rdfs:Resource (error "Cant happen!"))
                              (t (values nil nil))))
             (t (typecase d
                  (forall (values nil nil))
                  (exists (values nil nil))
                  (fills (values nil nil))
                  (rdfs:Resource (values nil nil))
                  (t (values nil nil))))
             ))))
)

(cl:provide :owlequivalentdisjoint)
