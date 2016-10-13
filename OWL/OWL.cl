;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL Module
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
;;;
;; History
;; -------
;;

(cl:provide :owl)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(in-package :gx)

;;; <owl-equalp> checks the equality of objects in OWL, namely it returns t, if 
;;; # owl-equivalent-p holds, if args are property.
;;; # if args are restrictions, then if both of owl:onProperty values are equal and values are 
;;;   equal as owl:allValuesFrom or owl:someValuesFrom or owl:hasValue, 
;;;   and either both or neither cardinality restriction are also equal.
;;; # owl-same-p holds.
;;; # otherwise equal in the sense of rdf-graph equality.

(excl:without-redefinition-warnings
(defun owl-equalp (x y)
  "returns true if <x> and <y> is equal in semantics of RDF(S) superimposed by OWL. 
   This predicate checks <x> and <y> in classes as individuals."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y)) ; cl:strings, symbols, objects, and gx:uri. If both are equal, then equal.
        ((and (numberp x) (numberp y))     ; in case of cl:number, 
         (= x y))                          ; = tests in value space, e.g., 1 and 1.0 is equal
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (and (eq (class-of x) (class-of y))
              (rdf-equalp (value-of x) (value-of y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal. 
               ;; name may be a cons
               
               ;; owl part
               ((and (owl-restriction-p x) (owl-restriction-p y))
                (%owl-restriction-equal x y))
               ((and (owl-oneof-p x) (owl-oneof-p y))
                (%oneof-equivalent (slot-value x 'owl:oneOf) (slot-value y 'owl:oneOf)))
               ;; sameAs? See rdfp6, rdfp7
               ((member x (same-as-of y) :test #'(lambda (a b) (%owl-same-p a b))) t)
               ;; differentFrom?
               ((member x (different-from-of y) :test #'(lambda (a b) (%owl-same-p a b))) nil)
               ;; functional and inversefunctional
               ((and (owl-thing-p x) (owl-thing-p y)
                     (or (functional-property-equal-p x y)             ; rdfp1 by ter Horst
                         (inverse-functional-property-equal-p x y))))  ; rdfp2 by ter Horst
               ;((and (property-p x) (property-p y))
               ; (not (not (member x (equivalent-property-of y)))))
               ;; even though intersection and union, another slots must be checked for equality
               ;; therefore, graph-equality-checking must be done.
               ;; end of owl part
               
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names
               (t  ; if nonUNA, check subtree, even though different names or anonymous
                (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                  (declare (ignore graph))
                  result))))
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (owl-equalp (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (owl-equalp (symbol-value x) y))
        ((and (symbolp y) (object? y)) (owl-equalp x (symbol-value y)))
        ((and (uri-p x) (uri-p y) (uri= x y))) ; or go through next
        ((and (iri-p x) (iri-p y))                             ; uri-string different but
         (cond ((and *nonUNA* (iri-boundp x) (iri-boundp y))   ; if nonUNA and has value
                (rdf-equalp (iri-value x) (iri-value y)))      ; then check values
               (t nil)))                                    ; else different uri means different
        ((and (iri-p x) (iri-boundp x)) (owl-equalp (iri-value x) y))
        ((and (iri-p y) (iri-boundp y)) (owl-equalp x (iri-value y)))
        ((and (cl:typep x 'rdf:|inLang|) (cl:typep y 'rdf:|inLang|))
         ;; see, http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/#section-Graph-Literal
         (and (equalp (string (lang x)) (string (lang y))) (string= (content x) (content y))))
        ((and (consp x) (consp y))
         (and (owl-equalp (car x) (car y))
              (owl-equalp (cdr x) (cdr y))))))
)

#|
(owl-equalp vin:TableWine vin:DryWine) -> nil
|#

;;;
;;;; Subsumption 
;;;

;;;
;;; (subsumed-p vin:DryWhiteWine vin:WhiteNonSweetWine) -> t
;;;

;;;
;;; (subsumed-p food:Fruit food:EdibleThing) 
;;;

(defun %union-subsumed-p (c d)
  (let (cdisjuncts ddisjuncts)
    (or (and (setq cdisjuncts (union-of c))
             (every #'(lambda (csub) (subsumed-p-without-equivalency csub d)) cdisjuncts))
        (and (setq ddisjuncts (union-of d))
             (some #'(lambda (dsub) (subsumed-p-without-equivalency c dsub)) ddisjuncts)))))


(defun %intersection1-subsumed-p (var conjuncts d)
  "This function directly computes conjuncts instead of C's intersection. This is useful to compute 
   conjunction (and) without creating an intersection class."
  (let ((dintersections (intersection-of d)))
    (cond (dintersections                ; if C is subsumed by D's intersections
           (%intersection12-subsumed-p var conjuncts dintersections))
          (t (error "Not Yet!")))))
(defun %intersection12-subsumed-p (var conjuncts dintersections)
  (loop for c in conjuncts
      unless (mop:class-finalized-p c)
      do (mop:finalize-inheritance c))
  (let ((cpl (remove-duplicates (loop for c in conjuncts append (clos:class-precedence-list c)))))
    (and (every #'(lambda (ds) 
                    (some #'(lambda (cs) (subsumed-p cs ds))
                          (remove-if #'owl-restriction-p cpl)))
                (remove-if #'owl-restriction-p dintersections))
         (let ((drestrs (remove-if-not #'owl-restriction-p dintersections)))
           (unless drestrs (return-from %intersection12-subsumed-p t))
           (let ((props (remove-duplicates
                         (mapcar #'(lambda (dr) (name (onproperty-of dr))) drestrs))))
             (loop for prop in props
                 always 
                   (let* ((drs (remove-if-not  ; y's restriction
                                #'(lambda (dr) (eq prop (name (onproperty-of dr))))
                                drestrs))
                          (cslot (remove-duplicates
                                  (loop for c in conjuncts 
                                      when (find prop (mop:class-slots c)
                                                 :key #'mop:slot-definition-name)
                                      collect it)))
                          (cmax (slot-definition-maxcardinality cslot))
                          (cmin (slot-definition-mincardinality cslot))
                          (crs (mklist (mop:slot-definition-type cslot))))
                     (format t "~%crs: ~S cmax: ~S cmin: ~S~%drs: ~S" crs cmax cmin drs)
                     ;; crs includes every user defined restriction and range constraint.
                     ;; x: predecessor, y: successor
                     (let ((types (remove-if #'(lambda (cn)
                                                 (or (cl:typep cn 'forall)
                                                     (cl:typep cn 'exists)
                                                     (cl:typep cn 'fills)))
                                             crs))
                           (alls (loop for cn in crs
                                     when (cl:typep cn 'forall)
                                     collect (forall-filler cn)))
                           (exists (loop for cn in crs
                                       when (cl:typep cn 'exists)
                                       collect (exists-filler cn)))
                           (fillers (loop for cn in crs
                                        when (cl:typep cn 'fills)
                                        collect (fills-filler cn))))
                       (let ((models* (generate-models (or var (new-variable "gx")) prop cmax types alls exists fillers nil)))
                         (format t "~%models*:~S" models*)
                         ;; now we obtained C's models, then all of them satisfy D's constraint?
                         (satisfy-model var models* drs))))))))))

;; (subsumed-p vin:DryWine vin:TableWine)
;; (subsumed-p vin:DryWhiteWine vin:WhiteNonSweetWine)
#|
(defConcept Person)
(defConcept Doctor
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasMedicalBranch)
                                         (owl:someValuesFrom MedicalBranch))))
(defConcept Doctor* (rdfs:|subClassOf| Person))
(defProperty hasMedicalBranch (rdfs:|domain| Doctor*) (rdfs:|range| MedicalBranch))
(defConcept Employee
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasSalary)
                                         (owl:someValuesFrom Salary))))
(defConcept Employee* (rdfs:|subClassOf| Person))
(defProperty hasSalary (rdfs:|domain| Employee*) (rdfs:|range| Salary))
(defConcept Employer
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasEmployee)
                                         (owl:someValuesFrom Employee)))
  (owl:disjointWith Employee))
(defConcept Employer* (rdfs:|subClassOf| Person))
(defProperty hasEmployee (rdfs:|domain| Employer*) (rdfs:|range| Employee))

(defConcept DoctorAndEmployee
    (owl:intersectionOf Employee Doctor))
(defConcept DoctorAndEmployee* (rdf:|type| owl:Class)
    (rdfs:|subClassOf| Employee* Doctor*))

(subsumed-p DoctorAndEmployee Doctor)
(subsumed-p DoctorAndEmployee* Doctor*)
(subsumed-p DoctorAndEmployee Doctor*)
(subsumed-p DoctorAndEmployee* Doctor)

(defConcept DoctorAndEmployer
    (owl:intersectionOf Employer Doctor))
(defConcept DoctorSelfEmployed
    (owl:intersectionOf Doctor
                        (owl:Class (owl:complementOf Employee))
                        (owl:Class (owl:complementOf Employer))))

(subsumed-p DoctorAndEmployee Doctor)
(subsumed-p DoctorAndEmployee Employee)
(subsumed-p DoctorAndEmployee Doctor*)
(subsumed-p DoctorAndEmployee Employee*)
(subsumed-p DoctorAndEmployee DoctorAndEmployee*)
(subsumed-p DoctorAndEmployee* DoctorAndEmployee)

(subsumed-p DoctorSelfEmployed Doctor)
(disjoint-p DoctorSelfEmployed DoctorAndEmployee)
(disjoint-p DoctorSelfEmployed DoctorAndEmployer)
(disjoint-p DoctorAndEmployee DoctorAndEmployer)

(defConcept AlbeitDoctor* (rdf:|type| owl:Class)
  (rdfs:|subClassOf| Doctor))
(defProperty hasSalary (rdfs:|domain| AlbeitDoctor*) (rdfs:|range| Salary))

(subsumed-p AlbeitDoctor* DoctorAndEmployee)
(subsumed-p DoctorAndEmployee AlbeitDoctor*)

(defConcept AlbeitDoctor 
    (owl:intersectionOf Doctor
                        (owl:Restriction (owl:onProperty hasSalary)
                                         (owl:someValuesFrom Salary))))

(subsumed-p AlbeitDoctor DoctorAndEmployee*)
(subsumed-p DoctorAndEmployee* AlbeitDoctor)


(defConcept Parent
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Person))))
(defConcept ParentOfEmployee
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Employee))))
(defConcept ParentOfEmployer
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Employer))))
(defConcept ParentOfDoctor
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Doctor))))
(subsumed-p ParentOfEmployee Parent)
(subsumed-p ParentOfEmployer Parent)
(subsumed-p ParentOfDoctor Parent)
(subsumed-p ParentOfEmployer ParentOfDoctor)
(disjoint-p ParentOfEmployee ParentOfDoctor)

(defConcept ParentWhosChildAllDoctor
    (owl:intersectionOf Parent
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:allValuesFrom Doctor))))

(subsumed-p ParentWhosChildAllDoctor ParentOfDoctor)
(subsumed-p ParentWhosChildAllDoctor ParentOfEmployer)
(subsumed-p ParentOfEmployer ParentWhosChildAllDoctor)

(defConcept ParentOfDoctorAndEmployee
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom DoctorAndEmployee))))
(defConcept ParentOfDoctorAndEmployer
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom DoctorAndEmployer))))
(defConcept ParentOfDoctorSelfEmployed
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom DoctorSelfEmployed))))

(subsumed-p ParentOfDoctorAndEmployee ParentOfDoctor)
(subsumed-p ParentOfDoctorAndEmployee ParentOfEmployee)
(subsumed-p ParentOfDoctorAndEmployee ParentOfEmployer)

(defConcept IntersectOfParentsOfEmployeeAndParentsOfDoctor
    (owl:intersectionOf ParentOfEmployee ParentOfDoctor))
(defConcpet IntersectOfParentOfEmployeeAndParentOfDoctor
    (owl:intersectionOf ParentOfEmployee ParentOfEmployer))
(defConcept IntersectOfParentOfEmployeeParentOfEmployerParentOfDoctor
    (owl:intersectionOf ParentOfEmployee ParentOfEmployer ParentOfDoctor))
(defConcept IntersectOfParentOfEmployeeAndParentOfEmployerInheritParentOfDoctor
    (owl:intersectionOf ParentOfEmployee ParentOfEmployer)
  (rdfs:|subClassOf| ParentOfDoctor))

(subsumed-p IntersectOfParentsOfEmployeeAndParentsOfDoctor ParentOfDoctorAndEmployee) -> nil
(subsumed-p IntersectOfParentOfEmployeeParentOfEmployerParentOfDoctor ParentOfDoctorAndEmployee)
(defConcept ParentOfOneChildDoctorAndEmployee
    (owl:intersectionOf ParentOfEmployee ParentOfDoctor
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:cardinality 1))))

(subsumed-p ParentOfOneChildDoctorAndEmployee ParentOfDoctorAndEmployee)

(defConcept ParentOfTwoChildDoctorAndEmployee
    (owl:intersectionOf ParentOfEmployee ParentOfDoctor
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:cardinality 2))))

(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfDoctorAndEmployee)           -> nil
(subsumed-p ParentOfOneChildDoctorAndEmployee ParentOfTwoChildDoctorAndEmployee)
(subsumed-p ParentOfTwoChildDoctorAndEmployee IntersectOfParentsOfEmployeeAndParentsOfDoctor)
(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfDoctor)
(subsumed-p ParentOfTwoChildDoctorAndEmployee ParentOfEmployee)
|#

#|
(defConcept ParentOfEmployeeAndEmployer
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Employee))
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Employer))))  -> clash

(defConcept GrandParentOfDoctor
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom ParentOfDoctor))))
(defConcept GrandParentOfDoctorAndEmployee
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom ParentOfDoctorAndEmployee))))

(subsumed-p GrandParentOfDoctorAndEmployee GrandParentOfDoctor)

(defConcept IntersectionOfOneChildParentOfEmployeeAndParentOfEmployer
    (owl:intersectionOf ParentOfEmployee ParentOfEmployer
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:cardinality 1))))

(subsumed-p IntersectionOfOneChildParentOfEmployeeAndParentOfEmployer ParentOfEmployee)

(defConcept Lawyer (rdfs:|subClassOf| Person))
(defConcept PatentAttorney (rdfs:|subClassOf| Person))

(defConcept ParentOfLawyer
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Lawyer))))
(defConcept ParentOfAttorney
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Attorney))))

(defConcept ParentOfLawyerAndAttorney
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Lawyer))
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Attorney))))

(defConcept ParentOfLawyerAndAttorneyAndDoctor
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Lawyer))
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Attorney))
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:someValuesFrom Doctor))))

(subsumed-p ParentOfLawyer ParentOfLawyerAndAttorney)
(subsumed-p ParentOfLawyerAndAttorneyAndDoctor ParentOfDoctor)   --> nil
(subsumed-p ParentOfDoctor ParentOfLawyerAndAttorneyAndDoctor)
(defConcept GrandParentOfDoctor
    (owl:intersectionOf Person
                        (owl:Restriction (owl:onProperty hasChild)
                                         (owl:cardinality 1))))
|#

(defun %oneof1-subsumed-p (c d)
  ;; recursively loop while c is an oneof.
  (cond ((not (owl-oneof-p c)) (subsumed-p c d))
        ((%clos-subtype-p c d))
        ((every #'(lambda (ci) (typep ci d)) (slot-value c 'owl:oneOf)))
        (t (some #'(lambda (csuper) (%oneof1-subsumed-p csuper d))
                 (mop:class-direct-superclasses c)))))

(defun %oneof2-subsumed-p (c d)
  ;; c is not an oneof but d is an oneof, seldom happen.
  (declare (ignore c d))
  nil)

(defun oneof-subsumed-p (c d)
  ;; oneOf(a b c) < oneOf (a b c d)
  (if (subsetp (slot-value c 'owl:oneOf) (slot-value d 'owl:oneOf) :test #'%owl-same-p)
      (values t t)
    (values nil t)))

(defun collect-most-specific-concepts (class instance)
  "This function is used for shared-initialize."
  (let ((subs (mop:class-direct-subclasses class)))
    (setq subs     ; same as owl:Thing
          (remove-if #'(lambda (sub) (eql (complement-of sub) owl:Nothing)) subs))
    (setq subs (remove owl:Nothing subs))
    (setq subs (remove-if #'owl-oneof-p subs))
    (setq subs (remove-if-not #'(lambda (sub) (%typep-for-MSCs instance sub)) subs))
    (cond ((null subs) (list class))
          (t (most-specific-concepts-for-refining 
              (loop for sub in subs
                  append (collect-most-specific-concepts sub instance)))))))

;;.............................................................................................
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

(defun get-intersect-siblings (class intersections)
  "returns a set of subclasses of all member of <intersections>, removing duplicates and 
   ones that has same intersection as <class>."
  ;; %intersection-equivalent removes myself and the structural equal objects, 
  ;; e.g., DryWine vs. TableWine 
  (remove-if #'(lambda (sib) (%intersection-equivalent sib class))
             (remove-duplicates
              (mappend #'mop:class-direct-subclasses intersections))))

(defun get-union-spouses (class unions)
  "returns a set of superclases of all member of <unions>, removing duplicates and 
  ones that has same union as <class>."
  (remove-if #'(lambda (sp) (%union-equivalent sp class))
             ;; %union-equivalent remove myself and the structuralequal objects on union members
             (remove-duplicates
              ;; remove-if-not is necessary for (owl:unionOf owl:Nothing (owl:Class (owl:complementOf owl:Nothing)))
              (remove-if-not #'owl-class-p
                             (mappend #'mop:class-direct-superclasses unions)))))

(defun get-superclasses-siblings (class superclasses)
  (remove-if #'(lambda (sib) (and (owl-class-p sib)
                                  (owl-class-p class)
                                  (%intersection-equivalent sib class)))
             (remove class
                     (remove-duplicates
                      (mappend #'mop:class-direct-subclasses superclasses)))))

(defun get-subclasses-siblings (class subclasses)
  (remove-if #'(lambda (sib) (and (owl-class-p sib)
                                  (owl-class-p class)
                                  (%union-equivalent sib class)))
             (remove class
                     (remove-duplicates
                      (mappend #'mop:class-direct-superclasses subclasses)))))

;; refine-sibling-from-intersection causes DryWine subtype TableWine and 
;; TableWine subtype DryWine.
;; the calculation does not terminate.
(defun subtypep-for-refining (c1 c2)
  (or (%clos-subtype-p c1 c2)
      (%intersection-refining-p c1 c2)
      (%union-refining-p c1 c2)
      (%oneof-refining-p c1 c2)
      (%complement-refining-p c1 c2)))

(defun %intersection-refining-p (c1 c2)
  (let ((intersections nil))
    (when (and (owl-class-p c2) (setq intersections (intersection-of c2)))
      (let ((cpl (remove c1
                         (cond ((mop:class-finalized-p c1)
                                (clos:class-precedence-list c1))
                               (t (mop:compute-class-precedence-list c1)))))
            (c2supers (remove-if #'owl-restriction-p intersections))
            (c2restrictions (remove-if-not #'owl-restriction-p intersections)))
        (unless (and c2supers
                     (every #'(lambda (c2super) 
                                (some #'(lambda (c1super)
                                          (subtypep-for-refining c1super c2super))
                                      (remove-if #'owl-restriction-p cpl)))
                            c2supers))
          (return-from %intersection-refining-p nil))
        ;; when c2supers=null or subtyed for c2supers
        (cond ((null c2restrictions)
               (return-from %intersection-refining-p t))
              ((every #'(lambda (c2restriction)
                          (some #'(lambda (c1restriction)
                                    (restriction-subtypep c1restriction c2restriction))
                                (remove-if-not #'owl-restriction-p cpl)))
                      c2restrictions)))))))

(defun %union-refining-p (c1 c2)
  (let ((unions nil))
    (when (and (owl-class-p c2) (setq unions (union-of c2)))
      (cond ((some #'(lambda (u) (cl:subtypep c1 u))
                   unions)
             (error "Not Yet!"))
            (t nil))
      )))

(defun %oneof-refining-p (c1 c2)
  (when (and (owl-oneof-p c1) (owl-oneof-p c2))
    (subsetp (slot-value c1 'owl:oneOf) (slot-value c2 'owl:oneOf) :test #'%owl-same-p)))

(defun %complement-refining-p (c1 c2)
  (let ((complement1 ())
        (complement2 ()))
    (when (and (owl-class-p c1) (setq complement1 (complement-of c1))
               (owl-class-p c2) (setq complement2 (complement-of c2)))
      (subtypep-for-refining complement2 complement1))))

;;;
;;;; owl:complementOf
;;;

(defun shared-initialize-after-for-complementOf (class complements)
  (loop for complement in complements with result
      unless (eql class owl:Nothing)  ; owl:Nothing is complement to any object.
      do (cond ((setq result (check-instance-sharing class complement))
                (error 'complementof-condition-unsatiafiable
                  :format-control "~S has super-sub relation to ~S."
                  :format-arguments `(,(class-name class) ,(class-name result))))
               (t (cond ((slot-boundp class 'complement-class)
                         (cond ((subtypep complement (slot-value class 'complement-class))
                                (setf (slot-value class 'complement-class) complement))
                               ((error "Monotonicity Violation for ~S:complementOf=~S <- ~S"
                                  class (slot-value class 'complement-class) complement))))
                        (t (setf (slot-value class 'complement-class) complement)))  ; rule5
                  (cond ((slot-boundp complement 'complement-class)
                         (cond ((subtypep class (slot-value complement 'complement-class))
                                (setf (slot-value complement 'complement-class) class))
                               ((error "Monotonicity Violation for ~S:complementOf=~S <- ~S"
                                  complement (slot-value complement 'complement-class) class))))
                        (t (setf (slot-value complement 'complement-class) class)))  ; rule5
                  (pushnew complement (slot-value class 'disjoint-classes))          ; rule6
                  (pushnew class (slot-value complement 'disjoint-classes))))))      ; rule6

;;;
;;;; intersectionOf
;;;

;; ensure for intersections to be OWL classes.
(defun shared-initialize-after-for-intersectionOf (class intersections)
  (loop for super in (remove-if #'owl-restriction-p intersections)
      unless (owl-class-p super)
      do (warn "Change class by intersectionOf:~S rdf:type owl:Class" (name super))
        (change-class super (load-time-value owl:Class)))
  (let ((siblings (get-intersect-siblings class intersections)))
    ;; class and its structural equivalents are removed from siblings
    (loop for sib in siblings
        do (cond ((subsetp intersections (mop:class-direct-superclasses sib))
                  (let ((new-supers (refine-concept-by-intersection
                                     (most-specific-concepts-by-superclasses
                                      (cons class (mop:class-direct-superclasses sib))))))
                    (format t "~%Old1:~S" (mop:class-direct-superclasses sib))
                    (format t "~%New1:~S" new-supers)
                    (warn "~S is refined to a subclass of ~S by defining ~S's intersection."
                      sib class class)
                    (reinitialize-instance sib :direct-superclasses new-supers)))
                 ((every #'(lambda (sup) (cl:subtypep sib sup)) intersections)
                  (let ((new-supers (refine-concept-by-intersection
                                     (most-specific-concepts-by-superclasses
                                      (cons sib (mop:class-direct-superclasses class))))))
                    (format t "~%Old2:~S" (mop:class-direct-superclasses sib))
                    (format t "~%New2:~S" new-supers)
                    (warn "~S is refined to a subclass of ~S by defining ~S's intersection."
                      class sib class)
                    (reinitialize-instance class :direct-superclasses new-supers)))))))

(defun intersection-p (class)
  (and (cl:typep class owl:Class)
       (slot-boundp class 'owl:intersectionOf)))

(defun intersection-of (class)
  (and (cl:typep class owl:Class)
       (slot-boundp class 'owl:intersectionOf)
       (slot-value class 'owl:intersectionOf)))

;; rule14
(defun check-intersection-refining-for-subclasses (class superclasses)
  (let ((old-supers (mop:class-direct-superclasses class))
        (siblings (get-superclasses-siblings class superclasses)))
    (loop for sib in siblings
        unless (member sib old-supers)
        do (when (subtypep-for-refining class sib)
             (let ((new-supers
                    (refine-concept-by-intersection
                     (most-specific-concepts-by-superclasses (cons sib old-supers)))))
               (unless (set-equalp old-supers new-supers)
                 (warn "~S is refined to subclass of ~S." class sib)
                 (reinitialize-instance class :direct-superclasses new-supers)))))))
(excl:without-redefinition-warnings
(defun refine-concept-by-intersection (classes)
  "returns most specific concepts that include subs of <classes> as intersection's sub.
   Note that return values include initial values and newly added values."
  (cond ((null (cdr classes)) classes)
        (t (mapc #'(lambda (class)
                     (mapc #'(lambda (sub)
                               (cond ((and (cl:typep sub owl:Class)
                                           (intersection-of sub)
                                           (every #'(lambda (inte) (member inte classes))
                                                  (intersection-of sub)))
                                      ;; then refining
                                      (return-from refine-concept-by-intersection
                                        (refine-concept-by-intersection
                                         (most-specific-concepts-by-superclasses
                                          (cons sub classes)))))
                                     )) ; otherwise finally returns classes for mapc
                       (mop:class-direct-subclasses class)))
             classes))))
)
;;;
;;;; unionOf Shared Initialize After Action
;;; After defining a class with owl:unionOf slot, 
;;; # If the defined class <class> is a subclass of a superclass <sib> of a union member, 
;;;   <class> is inserted between the superclass <sib> and the union member.
;;; # If the defined class <class> is not a subclass of a superclass <sib> of a union member, 
;;;   <class> is a sibling of <sib>. 

(defun shared-initialize-after-for-unionOf (class unions)
  ;(format t "~%SHARED-INITIALIZE-AFTER-FOR-UNIONOF(~S ~S)" class unions)
  (loop for sub in (remove-if #'owl-restriction-p unions)
      unless (owl-class-p sub)
      do (warn "Change class by unionOf:~S rdf:type owl:Class" (name sub))
        (change-class sub (load-time-value (find-class 'owl:Class))))
  ;; reinitialize supers of unions
  (loop for sub in (remove-if #'owl-restriction-p unions)
      unless (cl:subtypep sub class)
      do (reinitialize-instance
          sub
          :direct-superclasses (most-specific-concepts-by-clos-supers
                                (cons class (mop:class-direct-superclasses sub)))))
  (let ((spouses (remove-if #'owl-restriction-p (get-union-spouses class unions))))
    ;; class and its rdf equivalents are removed from spouses
    (when spouses
      (let ((old-supers (mop:class-direct-superclasses class))
            (add-supers (remove-if-not #'(lambda (sp) (%union-subsumed-p class sp)) spouses)))
        (when add-supers
          ;(format t "~%Adding-supers-for-union:~S" add-supers)
          (let ((class-supers
                 (most-specific-concepts-by-clos-supers (append add-supers old-supers))))
            (unless (set-equalp class-supers old-supers)
              ;(format t "~%Supers of union class ~S: ~S -> ~S" class old-supers class-supers)
              (reinitialize-instance class :direct-superclasses class-supers))))))))

(defun check-union-refining-for-subclasses (class subclasses)
  (let ((old-supers (mop:class-direct-superclasses class))
        (siblings (get-subclasses-siblings class subclasses)))
    (loop for sib in siblings
        unless (member sib old-supers)
        do (when (subtypep-for-refining class sib)
             (let ((new-supers (most-specific-concepts (cons sib old-supers))))
               (unless (set-equalp old-supers new-supers)
                 (warn "~S is refined to subclass of ~S." class sib)
                 (reinitialize-instance class :direct-superclasses new-supers)))))))

(defmethod change-class :before ((instance owl:Thing) (new-class (eql owl:Class)) 
                                 &rest initargs)
  (declare (ignore initargs))
  (when (disjoint-p (class-of instance) new-class)
    (error 'disjointwith-condition-unsatiafiable
      :format-control "between ~S and ~S."
      :format-arguments `(,instance ,(class-name new-class)))))

(defmethod change-class :before ((instance owl:Thing) (new-class owl:Class) 
                                 &rest initargs)
  (declare (ignore initargs))
  (when (disjoint-p (class-of instance) new-class)
    (error 'disjointwith-condition-unsatiafiable
      :format-control "between ~S and ~S."
      :format-arguments `(,instance ,(class-name new-class)))))

;;;
;;;; Shared-initialize Before for owl:Class
;;;

(defmethod shared-initialize :before ((class owl:Class) slot-names &rest initargs
                                      &key (name nil))
  ;(format t "~%SHARED-INITIALIZE:BEFORE(owl:Class) ~S ~S ~S" class slot-names initargs)
  (let ((supers (append (mklist (getf initargs 'rdfs:|subClassOf|))
                        (getf initargs 'owl:intersectionOf))))
    (when supers
      (check-simple-disjoint-pair-p-in-supers name supers))
    (when (and (null slot-names) (not (null initargs))) ; reinitialize
      (loop for (role newval) on initargs by #'cddr with oldval
          when (and (or (eq role 'owl:intersectionOf) (eq role 'owl:unionOf))
                    (slot-exists-p class role)
                    (slot-boundp class role)
                    (setq oldval (slot-value class role))
                    ;; not set equal
                    (not (and (subsetp oldval newval :test #'owl-same-p)
                              (subsetp newval oldval :test #'owl-same-p))))
          do (warn "~S value ~S of ~S is doubly specified to ~S." role oldval class newval)))))

;;;
;;;; Shared-initialize After for owl:Class
;;;

;;; <update-instance-for-different-class> is called in <change-class>.
;;; Usually <change-class> is invoked by <ensure-class> without <initargs>.
;;; So, direct-superclasses is set by <reinitialize-instance> in <ensure-class>
;;; after <change-class>. The following routine is prepared only for direct 
;;; invocation of <change-class> by users.
(defmethod update-instance-for-different-class ((previous rdfs:|Class|) (current owl:Class)
                                                &rest initargs)
  "ensures owl:Thing as superclasses in OWL universe. This routine is effective
   only if a user designates :direct-superclasses but no inheritance of owl:Thing."
  (cond ((and (not (cl:typep previous owl:Class))     ; rdfsClass to owl:Class
              (getf initargs :direct-superclasses)    ; if supers but no owl:Thing
              (some #'(lambda (obj) (cl:subtypep obj owl:Thing))
                    (getf initargs :direct-superclasses)))
         (setf (getf initargs :direct-superclasses)
           (cons owl:Thing (getf initargs :direct-superclasses)))
         (apply #'call-next-method previous current initargs))
        (t (call-next-method))))

(defmethod shared-initialize :after ((class owl:Class) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (with-slots (rdfs:|subClassOf| owl:intersectionOf owl:unionOf owl:equivalentClass 
                                      owl:disjointWith owl:complementOf)
             class
           ;; see also ensure-meta-absts-using-class class for subclasses and intersections
           (when (and (slot-boundp class 'rdfs:|subClassOf|) rdfs:|subClassOf|)
             (check-intersection-refining-for-subclasses class (mklist rdfs:|subClassOf|))
             (when (mop:class-direct-subclasses class)
               (check-union-refining-for-subclasses class (mop:class-direct-subclasses class))))
           (when (and (slot-boundp class 'owl:intersectionOf) owl:intersectionOf)
             (shared-initialize-after-for-intersectionOf class owl:intersectionOf))
           (when (and (slot-boundp class 'owl:unionOf) owl:unionOf)
             (unless (eq class owl:Thing)
               (shared-initialize-after-for-unionOf class owl:unionOf)))
           (when (and (slot-boundp class 'owl:equivalentClass) owl:equivalentClass)
             (shared-initialize-after-for-equivalentClass class (mklist owl:equivalentClass)))
           (when (and (slot-boundp class 'owl:disjointWith) owl:disjointWith)
             (shared-initialize-after-for-disjointWith class (mklist owl:disjointWith)))
           (when (and (slot-boundp class 'owl:complementOf) owl:complementOf)
             (shared-initialize-after-for-complementOf class (mklist owl:complementOf))))
         (unless (owl-thing-p class)
           (reinitialize-instance
            class
            :direct-superclasses (list owl:Thing (mop:class-direct-superclasses class))))
         )))

;;;
;;;; Symmetric Property
;;;
(excl:without-redefinition-warnings
(defun symmetric-property-p (obj)
  "Is this <obj> an instance of owl:SymmetricProperty?"
  ;;this is same as '(cl:typep <obj> owl:SymmetricProperty)'
  (and (excl::standard-instance-p obj)
       (let ((class (class-of obj)))
         (cond ((eq class (load-time-value owl:SymmetricProperty)))
               ((mop:class-finalized-p class)
                (and (member (load-time-value owl:SymmetricProperty)
                                (mop:class-precedence-list class)
                                :test #'eq)
                     t))
               ((labels ((walk-partial-cpl (c)
                                           (let ((supers (mop:class-direct-superclasses c)))
                                             (when (member
                                                    (load-time-value owl:SymmetricProperty)
                                                    supers :test #'eq)
                                               (return-from symmetric-property-p t))
                                             (mapc #'walk-partial-cpl supers))))
                  (declare (dynamic-extent #'walk-partial-cpl))
                  (walk-partial-cpl class)
                  nil))))))
)

(excl:without-redefinition-warnings
(defun shared-initialize-before-in-OWL (instance slot-names initargs)
  ;; filler = oldval + newval, computed at shared-initialize:around(rdfs:Resource).
  (when (consp slot-names)
    (loop for slot-name in slot-names  ; when redefining slot value
        when (property? slot-name)
        do (when (functional-property-p (symbol-value slot-name))
             (let* ((oldval (slot-value instance slot-name))
                    (newval (set-difference (getf initargs slot-name) oldval)))
               (format t "~%~S to ~S on ~S in ~S" oldval newval slot-name instance)
               (loop for val in newval
                   do (cond ((some #'(lambda (ov) (definitely-owl-different-p val ov)) (mklist oldval))
                             (error 'sameas-condition-unsatiafiable
                               :format-control "~S is different from ~S."
                               :format-arguments `(,val ,oldval)))
                            (t (warn "Entailing same ~S to ~S by functional property ~S"
                                 val oldval slot-name)
                               (loop for ov in (mklist oldval)
                                   do (pushnew ov (slot-value val 'same-as))
                                     (pushnew val (slot-value ov 'same-as)))))))))))

(defmethod shared-initialize :after ((instance rdfs:|Resource|) slot-names &rest initargs)
  "book-keeping for reification"
  (let ((args (copy-list initargs)))
    (let ((changed (remf args :direct-slots)))
      (setq changed (or (remf args :direct-superclasses) changed))
      (cond ((and (null slot-names) (null args))  ; when change-class
             )
            ((and (consp slot-names) (null args)) ; when propagated
             )
            (t                                        ; first or redefinition
             (typecase instance
               (rdfs:|Literal| nil)
               (rdfs:|Datatype| nil)
               (rdf:|Statement| nil)
               (rdf:|List| nil)
               (t 
                (apply #'book-keeping-for-reification instance slot-names args)
                (let ((name (getf args :name)))
                  (when name
                    (when (nodeID? name)
                      (setf (slot-value instance 'excl::name) nil))
                    (export-as-QName name)
                    (setf (symbol-value name) instance)))
                         
                ;; functional property registers its inverse.
                ;; Then, vin:Delicate has ((vin:hasFlavor vin:WhitehallLanePrimavera) ... )
                (loop for funprop in (collect-owl-role-name-if #'functional-property-p instance)
                    as funprop-val = (and (slot-boundp instance funprop) (slot-value instance funprop))
                    when funprop-val
                    do ;(format t "~%Functional property ~S in ~S" funprop instance)
                      (loop for val in (mklist funprop-val)
                           ;when (owl-thing-p val)
                           do (pushnew (cons funprop instance) (slot-value val 'funprop-inverse)
                                       :test #'equal)))
                ;; rdfp1 ter Horst is deduced by reasoning.
                
                ;; inverse functional property also registers its inverse.
                (loop for inv-funprop in (collect-owl-role-name-if #'inverse-functional-property-p instance)
                    as inv-funprop-val = (and (slot-boundp instance inv-funprop) (slot-value instance inv-funprop))
                    when inv-funprop-val
                    do (loop for val in (mklist inv-funprop-val)
                           ;when (owl-thing-p val)
                           do (pushnew (cons inv-funprop instance) (slot-value val 'inverse-funprop-inverse)
                                       :test #'equal)))
                ;; symmetric property adds the subject to the object's slot. See rdfp3.
                (loop for symprop in (collect-owl-role-name-if #'symmetric-property-p instance) with symprop-val
                    when (setq symprop-val 
                               (and (slot-boundp instance symprop) (slot-value instance symprop)))
                    do 
                      (loop for val in (mklist symprop-val)
                           do (setf (slot-value val symprop) instance)))

                ;; transitive property registers its inverse. See rdfp4.
                (loop for trans-prop in (collect-owl-role-name-if #'transitive-property-p instance) with inv-transitive
                    when (setq inv-transitive
                               (and (slot-boundp instance trans-prop) (slot-value instance trans-prop)))
                    do (loop for invt in (mklist inv-transitive)
                           unless (slot-value invt 'gx::inverse-transitive)
                           do (reinitialize-instance invt :inverse-transitive `(,trans-prop ,instance))))
                )))))))
)

;;;
;;;; Shared-initialize After for owl:Thing
;;;

(defmethod shared-initialize :after ((instance owl:Thing) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when class changed
         )
        ((and (consp slot-names) (null initargs)) ; when class redefined, propagated
         )
        (t                                        ; first or redefinition
         (cond ((eq slot-names t)
                ;(format t "~%SHARED-INITIALIZE:AFTER(owl:Thing) first definition ~S~%  with ~S)" instance initargs)
                )
               (t ;(format t "~%SHARED-INITIALIZE:AFTER(owl:Thing) redefining ~S~%  slots ~S~%  with ~S)" instance slot-names initargs)
                ))
         (with-slots (owl:sameAs owl:differentFrom) instance
           ;; owl:sameAs makes sameAs groups among individuals.
           (when (slot-boundp instance 'owl:sameAs)
             (shared-initialize-after-for-sameAs instance (mklist owl:sameAs)))
           
           ;; owl:differentFrom makes pairwise different groups among individuals.
           (when (slot-boundp instance 'owl:differentFrom)
             (shared-initialize-after-for-differentFrom instance (mklist owl:differentFrom))))
         
         ;; functional property is moved to shared-initialize:after(rdfs:Resource)
         ;; inverse functional property is moved to shared-initialize:after(rdfs:Resource)
         ;; symmetric property is moved to shared-initialize:after(rdfs:Resource)
         ;; transitive property is moved to shared-initialize:after(rdfs:Resource)
         
         ;; satisfiability check for oneOf individual
         (let ((oneof (find-if #'owl-oneof-p  (mop:class-precedence-list (class-of instance)))))
           (when oneof
             ;(format t "~%SHARED-INITIALIZE :AFTER ~S ~S ~S" instance slot-names initargs)
             ;(format t "~%CLASS = ~S" (class-of instance))
             ;(format t "~%MCLASSES = ~S" (mclasses instance))
             (let ((ones (slot-value oneof 'owl:oneOf)))
               (cond ((member instance ones :test #'%owl-same-p) nil) ; nothing done
                     (t (let ((others (remove instance ones :test #'definitely-owl-different-p)))
                          (cond ((null others) (error 'oneof-condition-unsatiafiable
                                                 :format-control "~S for ~S of ~S"
                                                 :format-arguments `(,instance ,ones ,oneof)))
                                (t (warn "~S should be one of ~S." instance others)))))))
             ))
         
         ;; if oneof exists in classes, check its type.
         #|
         (let ((oneofclasses (remove-if-not #'(lambda (x) (cl:typep x 'OneOf)) (mclasses instance)))
               (notoneofclasses (remove-if #'(lambda (x) (cl:typep x 'OneOf)) (mclasses instance))))
           (cond ((and oneofclasses (null notoneofclasses))
                  (let ((newMSCs (set-difference oneofclasses oneofclasses
                                                 :test #'(lambda (subsumer subsumee)
                                                           (and (not (eql subsumer subsumee))
                                                                (subsumed-p subsumee subsumer))))))
                    (when newMSCs
                      ;(format t "~%newMSCs:~S" newMSCs)
                      (unless (set-equalp newMSCs oneofclasses)
                        (let ((notMSCs (set-difference oneofclasses newMSCs)))
                          (loop for new in newMSCs
                              do (reinitialize-instance new :direct-superclasses notMSCs)))
                        (cond ((null (cdr newMSCs))
                               (format t "~%Unshadowing for ~S to ~S" instance (car newMSCs))
                               (apply #'change-class instance (car newMSCs) initargs))
                              ((error "Not Yet!"))) ))))
                 ((null (cdr notoneofclasses))
                  (loop for oneofclass in oneofclasses with super = (car notoneofclasses)
                      when (every #'(lambda (one) (cl:typep one super))
                                  (slot-value oneofclass 'owl:oneOf))
                      do (unless (cl:subtypep oneofclass super)
                           (reinitialize-instance oneofclass :direct-superclasses `(,super)))))))
         |#
         ;; refine instance in OWL
         (typecase instance
           (owl:Restriction nil)
           (owl:Class
            (when (not (eql (class-of instance) owl:Class))
              (let ((*autoepistemic-local-closed-world* nil))
                (let ((MSCs (collect-most-specific-concepts (class-of instance) instance)))
                  (cond ((null MSCs) (error "Cant happen!"))
                        ((length=1 MSCs)
                         (cond ((eq (car MSCs) (class-of instance)))
                               ((no-twin-p (car MSCs) instance)
                                (assert (not (eq (car MSCs) owl:Nothing)))
                                (warn "Entailed in refining: ~S to ~S." instance (class-name (car MSCs)))
                                (apply #'change-class instance (car MSCs) initargs))
                               (t (warn "Twin found for ~S at ~S" instance (name (car MSCs)))
                                  ;(destroy instance)
                                  )))
                        (t (warn "~S might be refine with changing class to ~S" instance MSCs)
                           ))))))
           (owl:Thing
            (when (not (eql (class-of instance) owl:Thing))
              (let ((*autoepistemic-local-closed-world* nil))
                (let ((MSCs (collect-most-specific-concepts (class-of instance) instance)))
                  ;(format t "~%MSCs:~S" MSCs)
                  (cond ((null MSCs) (error "Cant happen!"))
                        ((length=1 MSCs)
                         (cond ((eq (car MSCs) (class-of instance)))
                               ((no-twin-p (car MSCs) instance)
                                (assert (not (eq (car MSCs) owl:Nothing)))
                                (warn "Entailed in refining: ~S to ~S." instance (class-name (car MSCs)))
                                (apply #'change-class instance (car MSCs) initargs))
                               (t (warn "Twin found for ~S at ~S" instance (name (car MSCs)))
                                  ;(destroy instance)
                                  )))
                        (t (warn "~S might be refine with changing class to ~S" instance MSCs)
                           ))))))))
        ))

;;; Warning: Entailed in refining: #<vin:WhiteWine vin:StGenevieveTexasWhite> 
;;; to vin:WhiteNonSweetWine.

;;; owl:equivalentClass is a subproperty of rdfs:subClassOf, so 
;;; the objects are automatically captured as subclasses in nature. 
;;; However, this characteristics is very tricky and bad for construction of stable ontology.
;;; Because rdfs:subClassOf is substantial relation, but equivalentClass is temporal one.
;;; We treat equivalentClass in inference instead of subproperty of rdfs:subClassOf 
;;; or superclass-subclass relation.

;; See rdfskernel
(defmethod make-this-supers ((class owl:Class) superclasses)
  "returns an append list of MSCs of restrictions and MSCs of non restrictions in <old-users> and <new-supers>."
  (let ((restrictions (remove-if-not #'owl-restriction-p superclasses))
        (supers (remove-if #'owl-restriction-p superclasses)))
    (when restrictions
      (setq restrictions (most-specific-concepts-for-restrictions restrictions)))
    (when supers
      (when (and (not (eq class owl:Thing))
                 (not (member owl:Thing supers)))
        (setq supers (cons owl:Thing supers)))       ; add default top
      (setq supers (most-specific-concepts-for-refining supers)))
    (append supers restrictions)))

(defun most-specific-concepts-for-restrictions (classes)
  (declare (optimize (speed 3) (safety 0)))
  (when classes
    (let ((l (remove-duplicates classes :test #'%owl-restriction-equal)))
      (let ((answer
             (set-difference l l
                             :test #'(lambda (abst spec)
                                       (cond ((%owl-restriction-equal abst spec) nil)
                                             ((and (equivalent-property-p
                                                    (onproperty-of abst) (onproperty-of spec))
                                                   (etypecase abst
                                                     (owl:hasValueRestriction
                                                      (typecase spec
                                                        (owl:hasValueRestriction 
                                                         (subsumed-p (slot-value spec 'owl:hasValue)
                                                                     (slot-value abst 'owl:hasValue)))
                                                        ))
                                                     (owl:allValuesFromRestriction
                                                      (typecase spec
                                                        (owl:allValuesFromRestriction 
                                                         (subsumed-p (slot-value spec 'owl:allValuesFrom)
                                                                     (slot-value abst 'owl:allValuesFrom)))))
                                                     (owl:someValuesFromRestriction
                                                      (typecase spec
                                                        (owl:someValuesFromRestriction 
                                                         (subsumed-p (slot-value spec 'owl:someValuesFrom)
                                                                     (slot-value abst 'owl:someValuesFrom)))))
                                                     (owl:cardinalityRestriction
                                                      (typecase spec
                                                        (owl:cardinalityRestriction
                                                         (flet ((get-card (obj prop)
                                                                          (when (slot-boundp obj prop)
                                                                            (slot-value obj prop))))
                                                           (let ((maxspec
                                                                  (or (get-card spec 'owl:cardinality)
                                                                      (get-card spec 'owl:maxCardinality)))
                                                                 (maxabst
                                                                  (or (get-card abst 'owl:cardinality)
                                                                      (get-card abst 'owl:maxCardinality)))
                                                                 (minspec
                                                                  (or (get-card spec 'owl:cardinality)
                                                                      (get-card spec 'owl:minCardinality)))
                                                                 (minabst
                                                                  (or (get-card abst 'owl:cardinality)
                                                                      (get-card abst 'owl:minCardinality))))
                                                             (and (or (null minabst) (null minspec)
                                                                      (> (value-of minspec) (value-of minabst)))
                                                                  (or (null maxabst) (null maxspec)
                                                                      (< (value-of maxspec) (value-of maxabst)))))))))))
                                              t))))))
        (assert answer)
        answer))))

;;;
;;;; Transitivity
;;;

(defun transitive-p (obj)
  (cond ((slot-value obj 'inverse-transitive) t)
        ((some #'(lambda (prop) (and (slot-boundp obj prop) (slot-value obj prop)))
               (collect-owl-role-name-if #'transitive-property-p obj)))))

(defun transitive-subp (property sub super &optional visited)
  (or (eql sub super)
      (if (member sub visited) nil
        (strict-transitive-subp property sub super visited))))
(defun strict-transitive-subp (property sub super visited)
  (let ((supers (and (slot-exists-p sub property)
                     (slot-boundp sub property)
                     (slot-value sub property))))
    (unless (listp supers) (setq supers (list supers)))
    (loop for subsSuper in supers
        thereis (transitive-subp property
                                 subsSuper
                                 super
                                 (cons sub visited)))))

(defun strict-transitive-superp (property super sub)
  (and (not (eql super sub)) (strict-transitive-subp property sub super nil)))
(defun strict-include-p (property super sub)
  (and (not (eql super sub)) (strict-transitive-subp property sub super nil)))

(defun most-specific-transitives (property transitives)
  (let ((l (remove-duplicates transitives)))    ; eql should be assured
    (set-difference l l :test #'(lambda (x y) (strict-transitive-superp property x y)))))

;;;
;;; Here after Bookkeeping Facilities
;;;

;;;
;;;; owl:AllDifferent
;;;

(defmethod shared-initialize :after ((instance owl:AllDifferent) slot-names &rest initargs)
  (declare (ignore slot-names))
  (let ((distincts (getf initargs 'owl:distinctMembers)))
    (unless (listp distincts) (setq distincts (list distincts)))
    (loop for distinct in distincts
        do (cond ((symbolp distinct)
                  (cond ((object? distinct)
                         (setq distinct (symbol-value distinct))
                         (unless (owl-thing-p distinct)
                           (warn "Change class by distinctMembers: ~S type owl:Thing." distinct)
                           (change-class distinct 'owl:Thing)))
                        (t (warn "Entail by distinctMembers: ~S type owl:Thing." distinct)
                           (make-instance 'owl:Thing `(:name ,distinct)))))
                 ((owl-thing-p distinct))
                 (t (warn "Change class by distinctMembers: ~S type owl:Thing." distinct)
                    (change-class distinct 'owl:Thing)
                    )))
    (loop for distinct1 in distincts
        do (loop for distinct2 in distincts
               unless (eq distinct1 distinct2)
               do (pushnew distinct1 (slot-value distinct2 'different-from))
                 (pushnew distinct2 (slot-value distinct1 'different-from))))))

;;
;;
;;

(defun no-twin-p (class instance)
  (or (anonymous-p instance)
      (every #'(lambda (ins)
                 (or (eq ins instance)
                     (anonymous-p ins)
                     (not (eql (name ins) (name instance)))))
             (class-direct-instances class))))

(defun %symbols2values (lst)
  (mapcar #'(lambda (x) (if (and (symbolp x) (boundp x)) (symbol-value x) x))
    lst))

;;;
;;;; owl:Restriction again
;;;
(excl:without-redefinition-warnings 
(defun owl-restriction-p (obj)
  "Is this <obj> an instance of owl:Restriction?"
  ;;this is same as '(cl:typep <obj> owl:Restriction)'
  (let ((class (class-of obj)))
    (cond ((eq class (load-time-value owl:Restriction)))
          ((not (mop:class-finalized-p class))
           (labels ((walk-partial-cpl (c)
                                      (let ((supers (mop:class-direct-superclasses c)))
                                        (when (member (load-time-value owl:Restriction)
                                                         supers
                                                         :test #'eq)
                                          (return-from owl-restriction-p t))
                                        (mapc #'walk-partial-cpl supers))))
             (declare (dynamic-extent #'walk-partial-cpl))
             (walk-partial-cpl class)
             nil))
          (t (and (member (load-time-value owl:Restriction)
                             (mop:class-precedence-list class)
                             :test #'eq)
                  t)))))
)
(defun restriction-subtypep (restriction1 restriction2)
  (and (eq (name (slot-value restriction1 'owl:onProperty))
           (name (slot-value restriction2 'owl:onProperty)))
       (etypecase restriction2
         (owl:hasValueRestriction 
          (etypecase restriction1
            (owl:hasValueRestriction
             (eql (slot-value restriction1 'owl:hasValue)
                  (slot-value restriction2 'owl:hasValue)))
            (owl:allValuesFromRestriction nil)
            (owl:someValuesFromRestriction nil)
            (owl:cardinalityRestriction nil)))
         (owl:allValuesFromRestriction nil)
         (owl:someValuesFromRestriction
          (etypecase restriction1
            (owl:hasValueRestriction nil)
            (owl:allValuesFromRestriction nil)
            (owl:someValuesFromRestriction
             (subtypep-for-refining
              (slot-value restriction1 'owl:someValuesFrom)
              (slot-value restriction2 'owl:someValuesFrom)))
            (owl:cardinalityRestriction nil)))
         (owl:cardinalityRestriction nil))))

;;;
;;;; owl:intersectionOf is translated to super/subtype relation.
;;;

(defmethod collect-all-subsumed-types ((class owl:Class))
  (cond ((null (intersection-of class)) (collect-all-subtypes class))
        (t (remove-duplicates 
            (loop for super in (remove-if #'owl-restriction-p (intersection-of class))
                append (remove-if-not #'(lambda (sub) (subsumed-p sub class))
                                      (collect-all-subtypes super)))))))

(defmethod collect-all-instances-of ((class owl:Class))
  (let ((*autoepistemic-local-closed-world* nil))
    (cond ((null (intersection-of class)) (call-next-method))
          (t (labels ((%all-instances-from (sub)
                                           (append 
                                            (remove-if-not #'(lambda (ins) (%typep ins class))
                                                           (class-direct-instances sub))
                                            (mappend #'%all-instances-from 
                                                     (mop:class-direct-subclasses sub)))))
               (remove-duplicates 
                (loop for super in (remove-if #'owl-restriction-p (intersection-of class))
                    append 
                      (loop for sib in (mop:class-direct-subclasses super)
                          append (%all-instances-from sib)))))))))

(defmethod all-instances-generator ((class owl:Class))
  (cond ((null (intersection-of class)) (call-next-method))
        (t (let ((pending-classes (intersection-of class))
                 (pending-instances nil))
             (flet ((generator ()
                      (loop
                        (when pending-instances
                          (return-from generator (pop pending-instances)))
                        (when (null pending-classes) (return-from generator nil))
                        (let ((next-class (pop pending-classes)))
                          (when (not (owl-restriction-p next-class)) ; next loop
                            (let ((instances 
                                   (remove-if-not #'(lambda (ins) (typep ins class))
                                                  (class-direct-instances next-class))))
                              (when instances (setf pending-instances instances))
                              (setf pending-classes
                                (append (mop:class-direct-subclasses next-class)
                                        pending-classes))))))))
               #'generator)))))

;;
;; (collect-all-instances-of vin:TexasWine)
;;

(defun owl-complement-p (c d)
  (cond ((eq c rdfs:|Resource|) (values nil nil))
        ((eq d rdfs:|Resource|) (values nil nil))
        ((not (slot-exists-p c 'equivalent-classes)) (values nil nil)) ; not OWL, return
        ((not (slot-exists-p d 'equivalent-classes)) (values nil nil)) ; not OWL, return
        ((values (some #'(lambda (cc)
                           (some #'(lambda (dd) (%owl-complement-p cc dd))
                                 (equivalent-classes-of d)))
                       (equivalent-classes-of c))
                 t))))

(defun %owl-complement-p (cc dd)
  (and (slot-exists-p dd 'complement-class)
       (slot-boundp dd 'complement-class)
       (equal cc (slot-value dd 'complement-class))))

;;
;;
;;

(defmethod mop:ensure-class-using-class ((class owl:Class) name &rest args)
  ;(format t "~%ENSURE-CLASS-USING-CLASS ~S ~S ~S" class name args)
  (assert (not (eq (car (mop:class-direct-superclasses owl:Thing)) owl:Thing)))
  (cond ((eq class owl:Thing) class)                              ; nothing done
        ((getf args :direct-superclasses) (call-next-method))     ; next
        (t (let ((initargs (copy-list args)))
             (setf (getf initargs :direct-superclasses) '(owl:Thing))
             (apply #'call-next-method class name initargs)))))

;;;
;;;;  hasValue Restriction sets up an initial value of the slot.
;;;
;; The following methods are called after owl:Class
(defmethod shared-initialize :after ((class owl:hasValueRestriction) slot-names &rest initargs)
  (when (member-if #'(lambda (x) (owl-oneof-p x)) (getf initargs :direct-superclasses))
    (error "Debug It!"))
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class 'owl:onProperty))
               (hasvalues (slot-value class 'owl:hasValue)))
           (let ((range (get-range property)))
             (when range
               (slot-value-range-check 'owl:hasValue hasvalues range)))
           (let* ((name (name property))
                  (slotd (find name (mop:class-direct-slots class) :key #'name))
               #|   (initfun (cond ((symbolp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name :boot)))
                                 ((rsc-object-p hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 ((stringp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 ((numberp hasvalues)
                                  (eval (excl::compute-initfunction-function
                                         hasvalues 'mop:slot-definition-initfunction
                                         (class-name class) name)))
                                 (t (error "Not Yet for ~S" hasvalues)))) |#
                  )
             ;; see update-onPropertyConstraints-for
             (cond (slotd (reinitialize-instance slotd
                                                 :name name 
                                                 :type (make-instance 'fills
                                                         :role name
                                                         :filler hasvalues
                                                         :subject-type class)
                                                 ;:initform hasvalues :initfunction initfun
                                                 ))
                   (t (case name
                        ((rdfs:|range|)
                         (push (make-instance 'gx::Property-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'fills
                                         :role name
                                         :filler hasvalues
                                         :subject-type class)
                                 :subject-type class
                                 ;:initform hasvalues :initfunction initfun
                                 )
                               (mop:class-direct-slots class)))
                        (otherwise
                         (push (make-instance 'OwlProperty-direct-slot-definition
                                 :documentation (format nil
                                                    "From hasValueRestriction ~S" class) 
                                 :name name :initargs `(,name)
                                 :type (make-instance 'fills
                                         :role name
                                         :filler hasvalues
                                         :subject-type class)
                                 :subject-type class
                                 ;:initform hasvalues :initfunction initfun
                                 )
                               (mop:class-direct-slots class)))))))))))

(defmethod shared-initialize :after
  ((class owl:someValuesFromRestriction) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class 'owl:onProperty))
               (somevalues (slot-value class 'owl:someValuesFrom)))
           (let* ((name (name property))
                  (slotd (find name (mop:class-direct-slots class) :key #'name)))
             (cond (slotd (reinitialize-instance slotd :name name
                                                 :type (make-instance 'exists
                                                         :role name
                                                         :filler somevalues
                                                         :subject-type class)))
                   (t (case name
                        ((rdfs:|range|)
                         (push (make-instance 'gx::Property-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'exists
                                         :role name
                                         :filler somevalues
                                         :subject-type class)
                                 :documentation "From someValuesFromRestriction as range" 
                                 :subject-type class)
                               (mop:class-direct-slots class)))
                        (otherwise
                         (push (make-instance 'OwlProperty-direct-slot-definition
                                 :name name :initargs `(,name)
                                 :type (make-instance 'exists
                                         :role name
                                         :filler somevalues
                                         :subject-type class)
                                 :documentation "From someValuesFromRestriction"
                                 :subject-type class)
                               (mop:class-direct-slots class)))))))))))

(defmethod shared-initialize :after
  ((class owl:allValuesFromRestriction) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class 'owl:onProperty))
               (allvalues (slot-value class 'owl:allValuesFrom)))
           (let ((name (name property))
                 ;(range (get-range property))
                 )
             #|
             (when range
               (cond ((owl-oneof-p allvalues)
                      (loop for one in (class-direct-instances allvalues)
                          unless (typep one range)
                          do (warn "oneOf + onProperty range entailment by ~S: ~S rdf:type ~S"
                               name one range)
                            (change-class one range)))
                     ((and (not (cl:typep allvalues owl:Restriction))
                           (not (subsumed-p allvalues range)))
                      (warn "onProperty range entailment by ~S: ~S rdfs:subClassOf ~S"
                        name allvalues range)
                      (reinitialize-instance
                       allvalues
                       :direct-superclasses 
                       (most-specific-concepts
                        (append (mklist range) (mop:class-direct-superclasses allvalues))))))) |#
             
             (let ((slotd (find name (mop:class-direct-slots class) :key #'name)))
               (cond (slotd (reinitialize-instance slotd
                                                   :name name
                                                   :type (make-instance 'forall
                                                           :role name
                                                           :filler allvalues
                                                           :subject-type class)))
                     (t (case name
                          ((rdfs:|range|)
                           (push (make-instance 'gx::Property-direct-slot-definition
                                   :name name :initargs `(,name)
                                   :type (make-instance 'forall
                                           :role name
                                           :filler allvalues
                                           :subject-type class)
                                   :documentation "From allValuesFromRestriction as range"
                                   :subject-type class)
                                 (mop:class-direct-slots class)))
                          (otherwise
                           (push (make-instance 'OwlProperty-direct-slot-definition
                                   :name name :initargs `(,name)
                                   :type (make-instance 'forall
                                           :role name
                                           :filler allvalues
                                           :subject-type class)
                                   :documentation "From allValuesFromRestriction" 
                                   :subject-type class)
                                 (mop:class-direct-slots class))))))))))))

(defmethod shared-initialize :after
  ((class owl:cardinalityRestriction) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t ;; first or redefinition
         (let ((property (slot-value class 'owl:onProperty)))
           (let* ((name (name property))
                  (maxcardinality (and (slot-boundp class 'owl:maxCardinality) 
                                       (slot-value class 'owl:maxCardinality)))
                  (mincardinality (and (slot-boundp class 'owl:minCardinality) 
                                       (slot-value class 'owl:minCardinality)))
                  (cardinality (and (slot-boundp class 'owl:cardinality) 
                                    (slot-value class 'owl:cardinality)))
                  (slotd (find name (mop:class-direct-slots class) :key #'name)))
             (when (datatype-p (class-of cardinality))
               (setq cardinality (value-of cardinality)))
             (assert (or (null cardinality) (integerp cardinality)))
             (cond (slotd 
                    (reinitialize-instance slotd 
                                           :name name  
                                           :maxcardinality cardinality 
                                           :mincardinality cardinality))
                   (t 
                    (when (datatype-p (class-of maxcardinality))
                      (setq maxcardinality (value-of maxcardinality)))
                    (when (datatype-p (class-of mincardinality))
                      (setq mincardinality (value-of mincardinality)))
                    (push (make-instance 'OwlProperty-direct-slot-definition
                            :documentation (format nil
                                               "From cardinalityRestriction ~S" class) 
                            :name name :initargs `(,name)
                            :maxcardinality (or maxcardinality cardinality)
                            :mincardinality (or mincardinality cardinality))
                          (mop:class-direct-slots class)))))))))

;;;
;;;; For owl:equivalentProperty
;;;
(excl:without-redefinition-warnings
(defun equivalentProperty-maintain (instance slot-names &rest initargs)
  (declare (ignore slot-names))
  (when (getf initargs 'owl:equivalentProperty)
    (let ((equivs (mklist (slot-value instance 'owl:equivalentProperty))))
      (let ((equivalent-props (equivalent-property-of instance)))
        (cond ((null equivalent-props) (setq equivalent-props (list instance)))
              (t (setq equivalent-props (most-specific-properties (append equivalent-props equivs)))
                 (mapc #'(lambda (p) (setf (slot-value p 'equivalent-property) equivalent-props))
                   equivalent-props)))))))

(defun equivalent-property-p (x y)
  "Are <x> and <y> equivalent property in semantics of OWL?"
  (cond ((equal x y))
        ((and (iri-p x) (iri-p y))
         (uri= x y))
        ((iri-p x)
         (equivalent-property-p (%uri2symbol x) y))
        ((iri-p y)
         (equivalent-property-p x (%uri2symbol y)))
        ((and (symbolp x) (object? x))
         (equivalent-property-p (symbol-value x) y))
        ((and (symbolp y) (object? y))
         (equivalent-property-p x (symbol-value y)))
        ((and (property-p x) (property-p y)
              (not (not (member x (equivalent-property-of y))))))
        ((and (owl-thing-p x) (owl-thing-p y)
              (or ;; rdfp1 ter Horst
                  (functional-property-equal-p x y)
                  ;; rdfp2 ter Horst
                  (inverse-functional-property-equal-p x y))))
        ))
)

(defun most-specific-properties (properties)
  (setq properties (remove-duplicates properties :test #'eql))
  (warn "Equivalent properties:~S" properties)
  properties)

;;;
;;;; owl:inverseOf
;;;
(excl:without-redefinition-warnings
(defun %get-inverse-prop (prop)              ; See rdfp8ax, rdfp8bx
  (when (cl:typep prop owl:ObjectProperty)
    (or (and (slot-boundp prop 'owl:inverseOf) (slot-value prop 'owl:inverseOf))
        (slot-value prop 'inverse-inverse-of))))
)
;;;
;;;; Property in OWL
;;;

(defmethod shared-initialize :after ((instance owl:ObjectProperty) slot-names &rest initargs)
  ; instance = ub:memberOf
  (declare (ignore slot-names))
  (cond ((getf initargs 'owl:inverseOf)
         (let ((inv (slot-value instance 'owl:inverseOf)))
           ; inv = ub:member
           (assert (cl:typep inv owl:ObjectProperty))
           (assert (or (null (slot-value inv 'inverse-inverse-of))
                       (eql instance (slot-value inv 'inverse-inverse-of))))
           ;(format t "~%REINITIALIZE ~S :inverse-inverse-of ~S" inv instance)
           (reinitialize-instance inv :inverse-inverse-of instance)
           (let ((inv-domain (and (slot-boundp inv 'rdfs:|domain|) (slot-value inv 'rdfs:|domain|)))
                 ; inv-domain = ub:Organization
                 (inv-range (and (slot-boundp inv 'rdfs:|range|) (slot-value inv 'rdfs:|range|))))
             ; inv-range = ub:Person
             (when (or inv-range inv-domain)
               ;(format t "~%REINITIALIZE ~S rdfs:domain ~S rdfs:range ~S" instance inv-range (or inv-domain t))
               (reinitialize-instance instance 'rdfs:|domain| inv-range 'rdfs:|range| (or inv-domain t))
               ; slot ub:memberOf for ub:Person
               (mop:finalize-inheritance inv-range)
               ))))))

;; rule8 by seiji
(defmethod shared-initialize :after ((instance owl:SymmetricProperty) slot-names &rest initargs)
  (declare (ignore slot-names))
  (when (or (getf initargs 'rdfs:|domain|) (getf initargs 'rdfs:|range|))
    (let ((domain (domain-value instance))
          (range (range-value instance)))
      (when (and domain range)
        (let ((equivs (union (mklist domain) (mklist range))))
          (loop for cls in equivs
              do (unless (owl-class-p cls)
                   (warn "~S rdfs:type owl:Class by owl:SymmetricProperty entailment." cls)
                   (change-class cls owl:Class))
                (setf (slot-value cls 'equivalent-classes) equivs)
                ))))))

(excl:without-redefinition-warnings
(defmethod domain-value ((property rdf:|Property|))
  (flet ((get-dom (p) (and (slot-boundp p 'rdfs:|domain|) (slot-value p 'rdfs:|domain|))))
    (mkatom (mappend #'(lambda (p) (mklist (get-dom p))) (equivalent-property-of property)))))
)

(defmethod domain-value ((property owl:ObjectProperty))
  ;; in change-class from (rdf:Property vin:locatedIn) to owl:TransitiveProperty
  ;; this function is called from update-instance-for-different-class/shared-initialize:around(rdf:Property)
  ;; but owl:inverseOf and inverse-inverse-of slot value is unbound before shared-initialization.
  ;; rule12 by seiji
  (flet ((%get-inv (p) (and (slot-boundp p 'owl:inverseOf) (slot-value p 'owl:inverseOf)))
         (%get-inv-inv (p) (and (slot-boundp p 'inverse-inverse-of) (slot-value property 'inverse-inverse-of)))
         (get-dom (p) (and (slot-boundp p 'rdfs:|domain|) (slot-value p 'rdfs:|domain|)))
         (get-ran (p) (and (slot-boundp p 'rdfs:|range|) (slot-value p 'rdfs:|range|))))
    (let* ((inv (or (%get-inv property) (%get-inv-inv property)))
           (inv-range (and inv
                           (mappend #'(lambda (p) (mklist (get-ran p)))
                                    (equivalent-property-of inv))))   ; rule12b
           (domain (mappend #'(lambda (p) (mklist (get-dom p)))
                            (equivalent-property-of property))))      ; rule11a
      (mkatom (append domain inv-range)))))

(excl:without-redefinition-warnings
(defmethod range-value ((property rdf:|Property|))
  (flet ((get-ran (p) (and (slot-boundp p 'rdfs:|range|) (slot-value p 'rdfs:|range|))))
    (mkatom (mappend #'(lambda (p) (mklist (get-ran p))) (equivalent-property-of property)))))
)

(defmethod range-value ((property owl:ObjectProperty))
  (flet ((%get-inv (p) (and (slot-boundp p 'owl:inverseOf) (slot-value p 'owl:inverseOf)))
         (%get-inv-inv (p) (and (slot-boundp p 'inverse-inverse-of) (slot-value property 'inverse-inverse-of)))
         (get-dom (p) (and (slot-boundp p 'rdfs:|domain|) (slot-value p 'rdfs:|domain|)))
         (get-ran (p) (and (slot-boundp p 'rdfs:|range|) (slot-value p 'rdfs:|range|))))
    (let* ((inv (or (%get-inv property) (%get-inv-inv property)))
           (inv-domain (and inv
                            (mappend #'(lambda (p) (mklist (get-dom p)))
                                     (equivalent-property-of inv))))  ; rule12a
           (range (mappend #'(lambda (p) (mklist (get-ran p)))
                           (equivalent-property-of property))))       ; rule11b
      (mkatom (most-specific-concepts (append range inv-domain))))))

;;
;; HasValue Restriction Violation
;;
#|
(defmethod (setf mop:slot-value-using-class) :before
  (value (class owl:Class) (object owl:Thing) (slotd gx::OwlProperty-effective-slot-definition))
  ;(format t "~%Setf Slot-value-using-class:before with ~S to ~S ~S" value object slotd)
  (when (slot-value slotd 'excl::initform)
    (when (not (owl-equalp value (slot-value slotd 'excl::initform)))
      (when (and (slot-value slotd 'maxcardinality) (<= (slot-value slotd 'maxcardinality) 1)
                 (not (functional-property? (name slotd))))
        (error "owl:hasValue cardinality violation: ~S.~S <- ~S" object (name slotd) value)))))
|#
;;;
;;; We construct every inference upon subsumption-basis.
;;; Subsumption is infered by structural subsumption algorithms, here.

(defun union-of (class)
  (and (slot-exists-p class 'owl:unionOf)
       (slot-boundp class 'owl:unionOf)
       (slot-value class 'owl:unionOf)))

(defun complement-of (class)
  (and (slot-exists-p class 'owl:complementOf)
       (slot-boundp class 'owl:complementOf)
       (slot-value class 'owl:complementOf)))

(defun onproperty-of (restriction)
  (and (slot-exists-p restriction 'owl:onProperty)
       (slot-boundp restriction 'owl:onProperty)
       (slot-value restriction 'owl:onProperty)))

;;;
;;;; Membership in OWL
;;;
;;; The following is taken from http://www.w3.org/TR/owl-ref/#equivalentProperty-def.
;;;
;;; "Property equivalence is not the same as property equality. Equivalent properties have the same "values" 
;;; (i.e., the same property extension), but may have different intensional meaning (i.e., denote different concepts). 
;;; Property equality should be expressed with the owl:sameAs construct. As this requires that properties are 
;;; treated as individuals, such axioms are only allowed in OWL Full."
;;;
;;; Since we consider the domain and range constraint and OWL onPorperty restrictions are intensional data of properties.
;;; we do not treat here the equivalent property.
;;; We do not implement sameAs functions for properties.

(excl:without-redefinition-warnings
(defun %%typep (object type)
  "<object> and <type> is an object in RDF universe."
  (declare (optimize (speed 3) (safety 0)))        
  (let ((equivs (append 
                 (and (slot-exists-p type 'equivalent-classes)
                      (slot-boundp type 'equivalent-classes)
                      (slot-value type 'equivalent-classes))
                 (and (slot-exists-p type 'same-as)
                      (slot-boundp type 'same-as)
                      (slot-value type 'same-as))))) ; OWL Full
    (if (null equivs) (%typep-without-type-equivalents object type) ; no equiv def
      (let ((value2 t))
        (loop for c in equivs                                ; equivs include type
            do (multiple-value-bind (val1 val2)
                   (%typep-without-type-equivalents object c)
                 (when val1 ; some equiv satisfies condition
                   (return-from %%typep (values t t)))
                 (setq value2 (and value2 val2))))
        (values nil value2)))))
)

(defun %typep-without-type-equivalents (object type)
  "This is sub-subfunction for <typep>. This is only invoked in <%%typep>.
   This function tests type relation for each member of owl:sameAs group."
  (if (owl-oneof-p type)
      (cond ((member object (slot-value type 'owl:oneOf) :test #'%owl-same-p)
             (values t t))
            (t (values nil t)))
    (let ((sames (and (slot-exists-p object 'same-as)
                      (slot-boundp object 'same-as)
                      (slot-value object 'same-as))))
      (cond (sames
             (let ((value2 t))
               (loop for same in sames
                   do (multiple-value-bind (val1 val2)
                          (typep-without-sames-and-equivalents-in-owl same type)
                        (when val1
                          (return-from %typep-without-type-equivalents (values t t)))
                        (setq value2 (and value2 val2))))
               (values nil value2)))
            (t (typep-without-sames-and-equivalents-in-owl object type))))))

(defun typep-without-sames-and-equivalents-in-owl (object type)
  "<object> and <type> are in RDF universe and no direct class-instance relations.
   <type> may be a complex concept."
  (cond ((intersection-of type)
         (owl-intersection-type-p object (intersection-of type)))
        ((union-of type)
         (owl-union-type-p object (union-of type)))
        ((complement-of type)
         (owl-complement-type-p object (complement-of type)))
        (*autoepistemic-local-closed-world*
         (values nil t))
        (t (values nil nil))))

(defun owl-intersection-type-p (object intersections)
  "when every element in <intersections> satisfies a type of <object> then true.
   If any element does not satisfy obviously, then imediately returns with false.
   Otherwise, finally the result for all of <intersections> is true or unknown."
  (let ((supers (remove-if #'owl-restriction-p intersections))
        (restrictions (remove-if-not #'owl-restriction-p intersections))
        (known t))
    (loop for sup in supers
        do (multiple-value-bind (v1 v2) (%typep object sup)
             (when (not v1)
               (cond (v2             ; <nil, t> implies false then imediately returns
                      (return-from owl-intersection-type-p (values nil t)))
                     (t              ; <nil, nil> implies unknown
                      (setq known nil))))))
    ;; true or unknown here
    (loop for r in restrictions
        as prop = (name (onproperty-of r))
        as slotvalues = (mklist (get-value object (onproperty-of r)))
        do ;(format t "~%prop:~S~%slotvalues:~S" prop slotvalues)
          (etypecase r
            (owl:allValuesFromRestriction          ; rdfp16 by ter Horst
             (cond ((null slotvalues)              ; by Seiji, allValues restriction in intersection must exist definitely.
                    (return-from owl-intersection-type-p (values nil t)))
                   (t (let ((rst (slot-value r 'owl:allValuesFrom)))
                        (loop for v in slotvalues
                            do (multiple-value-bind (v1 v2) (%typep v rst)
                                 (when (not v1)
                                   (cond (v2 (return-from owl-intersection-type-p (values nil t)))
                                         (t (setq known nil))))))))))
            (owl:someValuesFromRestriction          ; rdfp15 by ter Horst
             (cond ((null slotvalues)
                    (if *autoepistemic-local-closed-world*
                        (if (y-or-n-p "No value for ~S in ~S.~%Create it?" prop object)
                            (setf (slot-value object prop) 
                              (addInstance (slot-value r 'owl:someValuesFrom) nil))
                          (return-from owl-intersection-type-p (values nil t)))
                      (return-from owl-intersection-type-p (values nil nil))))
                   (t (let ((rst (slot-value r 'owl:someValuesFrom)))
                        (cond ((some #'(lambda (v)
                                         (multiple-value-bind (v1 v2) (%typep v rst)
                                           (when (not v1)
                                             (setq known (and known v2)))
                                           v1))   ; then imediately exits this mapping fun
                                     slotvalues)) ; and nothing done
                              ; not satisfied
                              (*autoepistemic-local-closed-world*    ; local world
                               (if (y-or-n-p "No value for ~S in ~S.~%Create it?" prop object)
                                   (setf (slot-value object prop) 
                                     (addInstance (slot-value r 'owl:someValuesFrom) nil))
                                 (return-from owl-intersection-type-p (values nil t))))
                              (t (return-from owl-intersection-type-p (values nil nil))))))))
            (owl:hasValueRestriction              ; rdfp14a by ter Horst
             (cond ((null slotvalues)
                    (if *autoepistemic-local-closed-world*
                        (if (error "check it")
                            ;(y-or-n-p "No value for ~S in ~S.~%Add it?" prop object)
                            (setf (slot-value object prop) (slot-value r 'owl:hasValue))
                          (return-from owl-intersection-type-p (values nil t)))
                      (return-from owl-intersection-type-p (values nil nil))))
                   (t (let ((rst (slot-value r 'owl:hasValue)))
                        ;(format t "~%rst:~S" rst)
                        (cond ((some #'(lambda (v)
                                         (or (owl-same-p v rst)             ; seiji 2010 Sep.
                                             (some #'(lambda (cc)
                                                       (some #'(lambda (dd)
                                                                 (transitive-property-subsumed-p cc dd))
                                                             (same-as-of rst)))
                                                   (same-as-of v))))
                                     slotvalues)
                               (setq known (and known t))
                               t) ; and nothing done
                              ; not satisfied
                              (*autoepistemic-local-closed-world*    ; local world
                               (if (error "check it")
                                   ;(y-or-n-p "No value for ~S in ~S.~%Add it?" prop object)
                                   (setf (slot-value object prop) (slot-value r 'owl:hasValue))
                                 (return-from owl-intersection-type-p (values nil t))))
                              (t (return-from owl-intersection-type-p (values nil nil))))))))
            (owl:cardinalityRestriction
             (let ((maxr (cond ((slot-boundp r 'owl:cardinality)
                                (slot-value r 'owl:cardinality))
                               ((slot-boundp r 'owl:maxCardinality)
                                (slot-value r 'owl:maxCardinality))))
                   (minr (cond ((slot-boundp r 'owl:cardinality)
                                (slot-value r 'owl:cardinality))
                               ((slot-boundp r 'owl:minCardinality)
                                (slot-value r 'owl:minCardinality)))))
               (when (and maxr (cl:typep maxr rdf:|XMLLiteral|))
                 (setq maxr (value-of maxr)))
               (when (and minr (cl:typep minr rdf:|XMLLiteral|))
                 (setq minr (value-of minr)))
               (unless (and (or (null minr) (>= (length slotvalues) minr))
                            (or (null maxr) (<= (length slotvalues) maxr)))
                 (return-from owl-intersection-type-p (values nil t))))))
        finally (return (cond (known (values t t))
                              (t (values nil nil)))))))
;;
;; (typep vin:StGenevieveTexasWhite vin:TexasWine)        -> tt
;; (typep vin:SaucelitoCanyonZinfandel1998 vin:TableWine) -> tt
;; (typep vin:MariettaOldVinesRed vin:DryWine)            -> tt
;;

(defun owl-union-type-p (object unions)
  (let ((subs (remove-if #'owl-restriction-p unions))
        (restrictions (remove-if-not #'owl-restriction-p unions))
        (known t))
    (assert (null restrictions))
    (loop for sub in subs
        do (multiple-value-bind (v1 v2) (%typep object sub)
             (cond (v1 (return-from owl-union-type-p (values t t)))
                   (t (setq known (and known v2)))))
        finally (return (values nil known)))))

(defun owl-complement-type-p (object complement)
  (multiple-value-bind (v1 v2) (%typep object complement)
    (cond (v1 (values nil t))
          (v2 (warn "Check ~S rdf:type (complement-of ~S)" object complement)
              (values t t))
          (t (values nil nil)))))

;;;
;;;; slot-value
;;;
;;; Cardinality check

(defmethod cardinality-ok-p (value (slotd OwlProperty-effective-slot-definition))
  "simply checks <value> with cardinality constraint without models and returns true if no problems.
   It is expected that elements in <value> will be unified if false is returned."
  (let ((maxcard (slot-definition-maxcardinality slotd))
        (differs
         (if (listp value)
             (remove-duplicates value :test #'definitely-owl-same-p)
           (list value))))
    (let ((len (length differs)))
      (or (null maxcard)
          (< len maxcard)
          (= len maxcard)
          (cond ((<= len 1))
                (t nil))))))
#|

;;
;; (typep ub:Department10.University0.FullProfessor5 ub:Chair) => t
;;

(defmethod (setf mop:slot-value-using-class)
    ((value Property-direct-slot-definition) (class rdfs:|Class|) (object owl:Restriction) slotd)
  ;(format t "~%Setf Slot-value-using-class with ~S to ~S ~S" value object slotd)
  (error "Bingo")
  (let ((slot-name (mop:slot-definition-name slotd))
        (prop-name (mop:slot-definition-name value)))
    (assert (eq slot-name 'excl::direct-slots))
    (cond ((not (slot-boundp object slot-name))
           (funcall #'call-next-method (list value) class object slotd))
          (t (let ((direct-slotds (remove prop-name (slot-value object slot-name)
                                          :key #'mop:slot-definition-name)))
               (cond ((null direct-slotds)
                      (funcall #'call-next-method
                               (list value) class object slotd))
                     (t (funcall #'call-next-method
                                 (cons value direct-slotds) class object slotd))))))))

;;;
;;;; OWL Untils
;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setq *system-properties* (list-all-properties t))
  )

(eval-when (:execute :load-toplevel)
  (setq *referenced-resources* nil)
)

(defmethod superclasses-of ((object owl:Class))
  (mappend #'equivalent-classes-of
           (mappend #'mop:class-direct-superclasses (equivalent-classes-of object))))
(defmethod subclasses-of ((object owl:Class))
  (mappend #'equivalent-classes-of
           (mappend #'mop:class-direct-subclasses (equivalent-classes-of object))))

;;
;; Additional Useful Axioms
;;
#+:slot-value-for-metaclass
(defConcept owl:DatatypeProperty (rdf:|type| rdfs:|Class|)
  (rdfs:|subClassOf| (owl:Restriction (owl:onProperty rdfs:|range|)
                                    (owl:allValuesFrom rdfs:|Datatype|))))
#+:slot-value-for-metaclass
(defConcept owl:ObjectProperty (rdf:|type| rdfs:|Class|)
  (rdfs:|subClassOf| (owl:Restriction (owl:onProperty rdfs:|range|)
                                    (owl:allValuesFrom owl:Class))))
|#

