;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; OWL Kernel Module
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
;; 2010.11.15    File created and kernel parts are moved from OWL module
;;
;;; ==================================================================================
;;; Loading this file onto RDF modules creats the OWL universe as part of the RDF universe.
;;; 
;;; ----------------------------------------------------------------------------------
;;;
;;;                                           ............................
;;;                                           :        :                 :
;;; rdf-node --------------------rdfs:Class --:-----owl:Class --- owl:Restriction
;;;     :                     /      : :......:     /
;;;     :               ...../.......:             /
;;;     :               :   /        :            /
;;;     :               :  /  +------:-- owl:Thing
;;;     :               : /  /       :
;;;  gnode --- rdfs:Resource --- rdf:Property
;;;
;;;  <--, /     super/sub class relation, the direction of super is right to left.
;;;  ..., :     class/instance relation, the direction of class is upward and left to right.
;;; ----------------------------------------------------------------------------------
;;; 
;;;; Compatibility between RDF and OWL Universes in SWCLOS
;;; W3C specifies two styles on the compatibility between RDF universe and OWL universe.  
;;; One is called OWL Full style in which rdf:Class is identical to owl:Class, rdfs:Resource is 
;;; identical to owl:Thing, and rdf:Property is identical to owl:ObjectProperty. Another is 
;;; called DL style and in which the semantics does not follow RDF semantics. The class 
;;; extensions between owl:Thing, owl:Class, and owl:ObjectProperty are mutually disjoint. 
;;; See, http://www.w3.org/TR/owl-semantics/rdfs.html.
;;;
;;; In SWCLOS implementation, owl:Thing is a subclass of rdfs:Resource, owl:Class is a subclass 
;;; of rdfs:Class, and owl:ObjectProperty is a subclass of rdf:Property. Thus, OWL universe is 
;;; included in RDF universe, and the semantics of RDF is realized in OWL unvierse due to the 
;;; CLOS Object-Oriented ususal manner. 
;;;
;;;; OWL Full Semantics (classes are also instances).
;;; In OWL Full, every class can be an instance of metaclass(es). This is naturally realized in 
;;; SWCLOS with owl:Class subclassing to rdfs:Class (eventually to cl:standard-class). In 
;;; addition, we set owl:Class as subclass of owl:Thing (eventually to rdfs:Resource). Thus, 
;;; classes in OWL (the extension of owl:Class) inherit the slots of rdf:Resource, rdfs:label, 
;;; rdfs:comment, rdf:type, etc. from rdfs:Resource.

(cl:provide :owlkernel)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(in-package :gx)

(export '(owl-same-p disjoint-p  
          owl-class-p owl-thing-p owl-oneof-p))

;;;
;;;; OWL Property Slot Definition
;;;
(excl:without-redefinition-warnings
(defparameter *default-slot-definition-class* 'OwlProperty-direct-slot-definition
  "Every slot of which name corresponds to RDF property is defined as an instance of 
Property-direct-slot-definition. This variable is set to symbol 
gx::Property-direct-slot-definition in RDFS module and set to 
OwlProperty-direct-slot-definition in OWL module.")
)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (mop:finalize-inheritance (find-class *default-slot-definition-class*))
  (mop:finalize-inheritance (find-class 'OwlProperty-effective-slot-definition))
  )

;;;
;;;; owl:Class and owl:Thing with Houskeeping Slots
;;;
;;; Note that the following programming style is useful to use SWCLOS as APIs for applications 
;;; that extend semantics of RDF(S) or OWL.
;;;
;;; Before loading RDF(S) or OWL knowledge into SWCLOS, you can define the housekeeping or 
;;; application specific methods at CLOS objects and classes in the ontology of application, 
;;; then load the ontology in RDF(S) or OWL. 
;;;
;;; Adding function of SWCLOS in RDF semantics adds new definition without any collisions onto 
;;; CLOS slot definitions and methods. As a result, you can obtain ontological knowledge and 
;;; application methods around ontologies.
;;;
;;; In case of no extension of OWL semantics, you can define application methods after loading 
;;; OWL ontologies.

;;;
;;; We add two new slots into resource objects, <funprop-inverse> that is the inverse pointer of 
;;; a property of owl:FunctionProperty and <inverse-funprop-inverse> that is the inverse pointer 
;;; of a property of owl:InverseFunctionProperty.
;;;
(eval-when (:execute :load-toplevel :compile-toplevel)
  (reinitialize-instance
   rdfs:|Resource|
   :direct-slots
   `((:name funprop-inverse :initform nil :initfunction ,(load-time-value #'excl::false)
            :initargs (:funprop-inverse))
     (:name inverse-funprop-inverse :initform nil 
            :initfunction ,(load-time-value #'excl::false)
            :initargs (:inverse-funprop-inverse))))
  )

(defclass owl:|Class| (rdfs:|Class|)
  ((complement-class :initarg :complement-class)
   (disjoint-classes :initarg :disjoint-classes :initform ())
   (equivalent-classes :initarg :equivalent-classes :initform ()))
  (:metaclass rdfs:|Class|)
  (:documentation "The meta class in OWL universe. This class is a subclass and instance of 
rdfs:Class."))

(defclass owl:|Thing| (rdfs:|Resource|)
  ((different-from  :initarg :different-from :initform ())
   (same-as         :initarg :same-as :initform ())
   (inverse-transitive :initarg :inverse-transitive :initform ()))
  (:metaclass owl:|Class|)
  (:documentation "The top class in OWL universe. This class is a subclass of rdfs:Resource 
and instance of owl:Class."))

(defun %equivalent-classes-of (c)
  "returns nil if no definition on owl:equivalentClass."
  (declare (inline))
  (and (slot-exists-p c 'equivalent-classes)
       (slot-value c 'equivalent-classes)))

(defun equivalent-classes-of (c)
  "returns a list of equivalences to <c>. Note that this function 
   returns one element list of <c>, when no equivalences defined."
  (declare (inline))
  (or (and (slot-exists-p c 'equivalent-classes)
           (slot-value c 'equivalent-classes))
      (list c)))

(defun owl-class-p (obj)
  "Is this <obj> an instance of owl:Class?
   Note that owl:Class and owl:Restriction is not owl class."
  ;;this is same as '(cl:typep <obj> owl:Class)'
  (declare (inline))
  (and (excl::standard-instance-p obj)
       (%owl-class-subtype-p (class-of obj))))
(defun %owl-class-subtype-p (class)
  "If you are sure that <class> is a metaobject of CLOS, use this instead of 
   (cl:subtypep <class> owl:Class)."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class (load-time-value owl:|Class|)))
        ((mop:class-finalized-p class)
         (and (member (load-time-value owl:|Class|)
                         (mop:class-precedence-list class)
                         :test #'eq)
              t))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member (load-time-value (find-class 'owl:|Class|))
                                       supers
                                       :test #'eq)
                        (return-from %owl-class-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))

(defun owl-class? (symbol)
  "Is this bound value to <symbol> is owl class?"
  (declare (inline))
  (and (symbolp symbol) (boundp symbol) (owl-class-p (symbol-value symbol))))

(defun %same-as-of (x)
  "returns nil if no definition on owl:sameAs."
  (declare (inline))
  (and (slot-exists-p x 'same-as)
       (slot-boundp x 'same-as)
       (slot-value x 'same-as)))
(excl:without-redefinition-warnings
(defun same-as-of (x)
  "returns a list of sames as <x>. Note that this function 
   returns one element list of <x>, when no same individuals defined."
  (declare (inline))
  (or (and (slot-exists-p x 'same-as)
           (slot-boundp x 'same-as)
           (slot-value x 'same-as))
      (list x)))
)

(defvar owl:|Nothing|)

(excl:without-redefinition-warnings
(defun owl-thing-p (obj)
  "Is this <obj> an instance of owl:Thing?
   Note that owl:Class and owl:Thing is not owl thing."
  ;;this is same as '(cl:typep <obj> owl:Thing)'
  (declare (inline))
  (and (excl::standard-instance-p obj)
       (%owl-thing-subclass-p (class-of obj))
       (not (eq (name obj) 'owl:|Nothing|))))
(defun %owl-thing-subclass-p (class)
  "If you are sure that <class> is a metaobject of CLOS, use this instead of 
  (cl:subtypep <class> owl:Thing)."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class (load-time-value owl:|Thing|)))
        ((mop:class-finalized-p class)
         (and (member (load-time-value owl:|Thing|)
                         (mop:class-precedence-list class)
                         :test #'eq)
              t))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member (load-time-value owl:|Thing|)
                                       supers
                                       :test #'eq)
                        (return-from %owl-thing-subclass-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))
)
;;;
;;; Note the domain & range of owl:equivalentProperty is rdf:Property rather than 
;;; owl:ObjectProperty.
;;;

;;;
;;;; Object Property for owl:inverseOf Book-keeping
;;;

(defclass owl:|ObjectProperty| (rdf:|Property|)
  ((inverse-inverse-of :initarg :inverse-inverse-of :initform ())
   )
  (:metaclass rdfs:|Class|)
  (:documentation "This class defines an inverse slot of the inverse-of property."))

(defun equivalent-property-of (c)
  (declare (inline))
  ;; rule9, rule10
  (or (slot-value c 'equivalent-property) (list c)))

(defmethod change-class :after ((instance rdf:|Property|) (new-class rdfs:|Class|) &rest initargs)
  "In case that <new-class> is owl:ObjectProperty, the domain of <instance> is retrieved and 
   slot definitions named its domain for <instance> is changed to an instance of 
   <OwlProperty-direct-slot-definition>."
  (case (name new-class)
    (owl:|ObjectProperty|
     (loop for domain in (mklist (and (slot-boundp instance 'rdfs:|domain|)
                                      (slot-value instance 'rdfs:|domain|)))
         do (loop for slotd in (mop:class-direct-slots domain)
                when (eq (name instance) (mop:slot-definition-name slotd))
                do (unless (cl:typep slotd 'OwlProperty-direct-slot-definition)
                     (change-class slotd (find-class 'OwlProperty-direct-slot-definition))))))))

;;;
;;;; Restriction Subclasses
;;;

(defclass owl:|Restriction| (rdfs:|Resource|)
  ()
  (:metaclass rdfs:|Class|)
  (:documentation "owl:Restriction is a metaclass a subclass of owl:Class in OWL."))

(defmethod excl::default-direct-superclasses ((class owl:|Restriction|))
  "The default direct superclass of restrictions is rdfs:|Resource|."
  (list (load-time-value (find-class 'rdfs:|Resource|))))

(defclass owl:|allValuesFromRestriction| (owl:|Restriction|) () (:metaclass rdfs:|Class|)
  (:documentation "A class for value restrictions is a subclass of owl:Restriction."))
(defmethod print-object ((obj owl:|allValuesFromRestriction|) stream)
  (cond ((and (slot-exists-p obj 'excl::name)
              (slot-boundp obj 'excl::name)
              (slot-value obj 'excl::name))
         (call-next-method))
        ((and (slot-exists-p obj 'owl:|onProperty|)
              (name (slot-value obj 'owl:|onProperty|))
              (slot-exists-p obj 'owl:|allValuesFrom|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "  ~S.~S"
             (name (slot-value obj 'owl:|onProperty|))
             (or (name (slot-value obj 'owl:|allValuesFrom|))
                 (get-form (slot-value obj 'owl:|allValuesFrom|))))))
        (t (call-next-method))))

(defmethod name ((object owl:|allValuesFromRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons 'owl:|allValuesFromRestriction|
                 (mapcar #'name (mklist (slot-value object 'owl:|allValuesFrom|)))))))

(defclass owl:|someValuesFromRestriction| (owl:|Restriction|) () (:metaclass rdfs:|Class|))
(defmethod print-object ((obj owl:|someValuesFromRestriction|) stream)
  (cond ((and (slot-exists-p obj 'owl:|onProperty|)
              (name (slot-value obj 'owl:|onProperty|))
              (slot-exists-p obj 'owl:|someValuesFrom|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "  ~S.~S"
             (name (slot-value obj 'owl:|onProperty|))
             (or (name (slot-value obj 'owl:|someValuesFrom|))
                 :anonymous))))
        (t (call-next-method))))

(defmethod name ((object owl:|someValuesFromRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons 'owl:|someValuesFromRestriction|
                 (mapcar #'name (mklist (slot-value object 'owl:|someValuesFrom|)))))))

(defclass owl:|hasValueRestriction| (owl:|Restriction|) () (:metaclass rdfs:|Class|))
(defmethod print-object ((obj owl:|hasValueRestriction|) stream)
  (cond ((and (slot-exists-p obj 'owl:|onProperty|)
              (name (slot-value obj 'owl:|onProperty|))
              (slot-exists-p obj 'owl:|hasValue|))
         (print-unreadable-object (obj stream :type nil)
           (format stream "~S  {~S}"
             (name (slot-value obj 'owl:|onProperty|))
             (cond ((slot-boundp obj 'owl:|hasValue|)
                    (let ((x (slot-value obj 'owl:|hasValue|)))
                      (cond ((eq x t) t)
                            ((eq x nil) nil)
                            ((excl::standard-instance-p x)
                             (cond ((name x))
                                   ((datatype-p (class-of x)) x)
                                   (t :anonymous)))
                            (t x))))
                   (t :unbound)))))
        (t (call-next-method))))

(defmethod name ((object owl:|hasValueRestriction|))
  (cond ((and (slot-boundp object 'excl::name) (slot-value object 'excl::name)))
        (t (cons 'owl:|hasValueRestriction|
                 (mapcar #'name (mklist (slot-value object 'owl:|hasValue|)))))))

(defclass owl:|cardinalityRestriction| (owl:|Restriction|) () (:metaclass rdfs:|Class|))
(defmethod print-object ((obj owl:|cardinalityRestriction|) stream)
  (cond ((slot-value obj 'owl:|onProperty|)
         (print-unreadable-object (obj stream :type nil)
           (prin1 (name (slot-value obj 'owl:|onProperty|)) stream)
           (and (slot-boundp obj 'owl:|minCardinality|)
                (format stream "   ~S" (slot-value obj 'owl:|minCardinality|)))
           (and (slot-boundp obj 'owl:|cardinality|)
                (format stream "= ~S" (slot-value obj 'owl:|cardinality|)))
           (and (slot-boundp obj 'owl:|maxCardinality|)
                (format stream "   ~S" (slot-value obj 'owl:|maxCardinality|)))))
        (t (call-next-method))))

(defun owl-cardinality-p (x) (cl:typep x owl:|cardinalityRestriction|))

;;;
;;;; OneOf class
;;;
;;; Note that oneOf elements may be in the RDF universe.
;;; Note that the domain of owl:oneOf is rdfs:Class. Namely, 
;;; The owl:oneOf slot definition is attached to rdfs:Class.
(excl:without-redefinition-warnings
(defmethod print-object ((obj rdfs:|Class|) stream)
  (cond ((and (slot-boundp obj 'excl::name)
              (slot-value obj 'excl::name))
         (print-unreadable-object (obj stream :type t)
           (prin1 (slot-value obj 'excl::name) stream)))
        ((slot-boundp obj 'owl:|oneOf|)
         (let ((ones (slot-value obj 'owl:|oneOf|)))
           (print-unreadable-object (obj stream :type t)
             (when (and (slot-boundp obj 'excl::name)
                        (slot-value obj 'excl::name))
               (prin1 (slot-value obj 'excl::name) stream))
             (princ #\{ stream)
             (prin1 (cond ((delay-p (car ones)) :delayed)
                          ((anonymous-p (car ones)) nil)
                          ((name (car ones))))
                    stream)
             (mapc #'(lambda (one)
                       (princ #\Space stream)
                       (prin1 (cond ((delay-p one) :delayed)
                                    ((anonymous-p one) nil)
                                    ((name one)))
                              stream))
               (cdr ones))
             (princ #\} stream))))
        (t (print-unreadable-object (obj stream :type t)
             (prin1 :unbound stream)))))
)

(excl:without-redefinition-warnings
(defun owl-oneof-p (x)
  "Is this <x> an object that holds owl:oneOf data?"
  (and (rdf-class-p x)
       (slot-exists-p x 'owl:|oneOf|)
       (slot-boundp x 'owl:|oneOf|)
       ))
)

;;;
;;;; How to Calculate a Type Value in Effective Slot Definition in OWL
;;;
;;; After the calculation of initargs as standard slot definition and rdf property slot 
;;; definition, the type option is recalculated, if OWL module is loaded. Because, it may 
;;; include existential restriction for slot value, and it must be treated in OWL semantics.
;;;
;;; The form of type option is one of the followings.
;;; ----------------------------------------------------------------------------------
;;; <fval> ::= t | <class-meta-object> | (and <fval> ...) |
;;;            (forall <slot-name> <fval>) | (exists <slot-name> <fval>) |
;;;            (fills <slot-name> <fval>)
;;;            (<= <slot-name> <fval>)  | (>= <slot-name> <fval>)  |
;;;            (= <slot-name> <fval>)
;;; ----------------------------------------------------------------------------------
;;; Here, <class-meta-object> is a class object which comes from direct slot definition in RDF,
;;; (and <fval> ...) is computed by <excl::compute-effective-slot-definition-initargs> method 
;;; at standard slot definition, (forall <slot-name> <fval>) originates from owl:allValuesFrom 
;;; constraint and (exists <slot-name> <fval>) originates from owl:someValuesFrom constraint. 
;;; (fills <slot-name> <fval>) represents owl:hasValueRestriction. 
;;;
;;; On the other hand, (\<= <slot-name> <fval>) is from owl:minCardinality, 
;;; (>= <slot-name> <fval>) is from owl:maxCardinality, and (= <slot-name> <fval>) is from 
;;; owl:cardinality.
;;;
;;; To compute the most specific concepts which include the above special <fval> forms, 
;;; <most-specific-concepts-for-slotd-type>, <strict-supertype-p-for-slotd-type> and 
;;; <strict-subtype-p-for-slotd-type> are defined, which specifically process these <fval> 
;;; with <subsumed-p> and <owl-equivalent-p>. 

(excl:without-redefinition-warnings
(defmethod excl::compute-effective-slot-definition-initargs ((class rdfs:|Class|) direct-slotds)
  (declare (optimize (speed 3) (safety 0)))
  (let ((initargs (call-next-method)))
    (let ((type (getf initargs ':type)))
      (when (consp type)
        (cond ((eq (car type) 'and)
               ; this 'and' comes from standard routine in ACL
               (check-simple-disjoint-pair-p-in-slot-types class (cdr type))
               ;(setq type (most-specific-concepts-for-slotd-type (cdr type)))
               ;(setq type (compute-effective-slot-definition-type type))
               )
              ((error "Cant happen!")))
        (setf (getf initargs ':type) type)))
    (%compute-effective-slot-definition-initargs 
     class (class-name class) (mop:slot-definition-name (car direct-slotds)) direct-slotds initargs)))
(defun %compute-effective-slot-definition-initargs (class class-name slot-name direct-slotds initargs)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((member-if #'owl-property-direct-slotd-p direct-slotds)
         (setq initargs `(:subject-type ,class ,@initargs))
         (loop for slotd in direct-slotds
             with maxc and minc and initform and initfunc
             as slotd-maxc = (and (slot-exists-p slotd 'maxcardinality)
                                  (slot-definition-maxcardinality slotd))
             and
               slotd-minc = (and (slot-exists-p slotd 'mincardinality)
                                 (slot-definition-mincardinality slotd))
             and
               slotd-initfunc = (mop:slot-definition-initfunction slotd) and
               slotd-initform = (mop:slot-definition-initform slotd)
             do
               ;; get minimum maxc over slotds
               ;(describe slotd)
               ;(format t "~%Maxc:~S Slotd-maxc:~S" maxc slotd-maxc)
               ;(format t "~%Minc:~S Slotd-minc:~S" minc slotd-minc)
               (cond ((and maxc slotd-maxc) (setq maxc (min slotd-maxc maxc)))
                     (slotd-maxc (setq maxc slotd-maxc)))
               ;; get maximum minc over slotds
               (cond ((and minc slotd-minc) (setq minc (max slotd-minc minc)))
                     (slotd-minc (setq minc slotd-minc)))
               (when (and slotd-initfunc (property? slot-name) (find-class 'owl:|TransitiveProperty| nil))
                 (cond ((cl:typep (symbol-value slot-name) (find-class 'owl:|TransitiveProperty|))
                        (cond ((and initform slotd-initform)
                               (cond ((and (consp initform) (consp slotd-initform))
                                      (error "Not Yet!"))
                                     ((consp initform)
                                      (cond ((some
                                              #'(lambda (x)
                                                  (transitive-subp slot-name slotd-initform x))
                                              initform))
                                            (t (setq initform
                                                     (cons slotd-initform initform))
                                               (setq initfunc
                                                     (cons slotd-initfunc initfunc)))))
                                     ((consp slotd-initform)
                                      (cond ((some #'(lambda (x)
                                                       (transitive-subp slot-name initform x))
                                                   slotd-initform))
                                            (t (setq initform
                                                     (cons initform slotd-initform))
                                               (setq initfunc
                                                     (cons initfunc slotd-initfunc)))))
                                     ((transitive-subp slot-name initform slotd-initform))
                                     ((transitive-subp slot-name slotd-initform initform)
                                      (setq initform slotd-initform)
                                      (setq initfunc slotd-initfunc))
                                     (t (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                              (slotd-initform ; null initform
                               (setq initform slotd-initform)
                               (setq initfunc slotd-initfunc))))
                       (t (cond ((and initform slotd-initform)
                                 (cond ((and (listp initform) (listp slotd-initform))
                                        (setq initform
                                              (union slotd-initform initform :test #'equal))
                                        (setq initfunc
                                              (union slotd-initfunc initfunc :test #'equal)))
                                       ((and (listp initform)
                                             (not (member slotd-initform initform
                                                          :test #'equal)))
                                        (setq initform (cons slotd-initform initform))
                                        (setq initfunc (cons slotd-initfunc initfunc)))
                                       ((and (listp slotd-initform)
                                             (not (member initform slotd-initform 
                                                          :test #'equal)))
                                        (setq initform
                                              (append slotd-initform (list initform)))
                                        (setq initfunc
                                              (append slotd-initfunc (list initfunc))))
                                       ((not (equal initform slotd-initform))
                                        (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                                (slotd-initform ; null initform
                                 (setq initform slotd-initform)
                                 (setq initfunc slotd-initfunc))))))
             finally (progn
                       (setf (getf initargs ':initform) initform)
                       (setf (getf initargs ':initfunction)
                         (cond ((consp initfunc)
                                (eval `(excl::named-function
                                        (mop:slot-definition-initfunction 
                                         ,class-name ,(gentemp (string slot-name)))
                                        (lambda () (mapcar #'funcall ',initfunc)))))
                               (t initfunc)))
                       (setf (getf initargs ':maxcardinality) maxc)
                       (setf (getf initargs ':mincardinality) minc)
                       (assert (or (null maxc) (null minc) (<= minc maxc)) ()
                               "Unsatisfiability by cardinality for ~S ~S"
                               class-name slot-name)
                       ;(format t "~%Computing Effective Slots ... ~S ~S ~S" class-name slot-name direct-slotds)
                       ;(format t "~%                              ~S" initargs)
                       (return initargs))))
        ((member-if #'property-direct-slotd-p direct-slotds)
         ;; if a slotd is property slotd, add subject-type option.
         (setq initargs `(:subject-type ,class ,@initargs))
         (loop for slotd in direct-slotds
             with initform and initfunc
             as 
               slotd-initfunc = (mop:slot-definition-initfunction slotd) and
               slotd-initform = (mop:slot-definition-initform slotd)
             do
               (when (and slotd-initfunc (property? slot-name) (find-class 'owl:|TransitiveProperty| nil))
                 (cond ((cl:typep (symbol-value slot-name) (find-class 'owl:|TransitiveProperty|))
                        (cond ((and initform slotd-initform)
                               (cond ((and (consp initform) (consp slotd-initform))
                                      (error "Not Yet!"))
                                     ((consp initform)
                                      (cond ((some
                                              #'(lambda (x)
                                                  (transitive-subp slot-name slotd-initform x))
                                              initform))
                                            (t (setq initform
                                                     (cons slotd-initform initform))
                                               (setq initfunc
                                                     (cons slotd-initfunc initfunc)))))
                                     ((consp slotd-initform)
                                      (cond ((some #'(lambda (x)
                                                       (transitive-subp slot-name initform x))
                                                   slotd-initform))
                                            (t (setq initform
                                                     (cons initform slotd-initform))
                                               (setq initfunc
                                                     (cons initfunc slotd-initfunc)))))
                                     ((transitive-subp slot-name initform slotd-initform))
                                     ((transitive-subp slot-name slotd-initform initform)
                                      (setq initform slotd-initform)
                                      (setq initfunc slotd-initfunc))
                                     (t (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                              (slotd-initform ; null initform
                               (setq initform slotd-initform)
                               (setq initfunc slotd-initfunc))))
                       (t (cond ((and initform slotd-initform)
                                 (cond ((and (listp initform) (listp slotd-initform))
                                        (setq initform
                                              (union slotd-initform initform :test #'equal))
                                        (setq initfunc
                                              (union slotd-initfunc initfunc :test #'equal)))
                                       ((and (listp initform)
                                             (not (member slotd-initform initform
                                                          :test #'equal)))
                                        (setq initform (cons slotd-initform initform))
                                        (setq initfunc (cons slotd-initfunc initfunc)))
                                       ((and (listp slotd-initform)
                                             (not (member initform slotd-initform 
                                                          :test #'equal)))
                                        (setq initform
                                              (append slotd-initform (list initform)))
                                        (setq initfunc
                                              (append slotd-initfunc (list initfunc))))
                                       ((not (equal initform slotd-initform))
                                        (setq initform (list slotd-initform initform))
                                        (setq initfunc (list slotd-initfunc initfunc)))))
                                (slotd-initform ; null initform
                                 (setq initform slotd-initform)
                                 (setq initfunc slotd-initfunc))))))
             finally (progn
                       (setf (getf initargs ':initform) initform)
                       (setf (getf initargs ':initfunction)
                         (cond ((consp initfunc)
                                (eval `(excl::named-function
                                        (mop:slot-definition-initfunction 
                                         ,class-name ,(gentemp (string slot-name)))
                                        (lambda () (mapcar #'funcall ',initfunc)))))
                               (t initfunc)))
                       ;(format t "~%Computing Effective Slots ... ~S ~S ~S" class-name slot-name direct-slotds)
                       ;(format t "~%                              ~S" initargs)
                       (return initargs))))
        (t initargs)))
)
;;;
;;; If <initargs> in making an effective-slot-definition includes :maxcardinality or 
;;; :mincardinality keyword, the slot-definition must be OwlProperty-direct-slot-definition.
(excl:without-redefinition-warnings
(defmethod mop:effective-slot-definition-class ((class rdfs:|Class|) &rest initargs)
  "This method calls next method if there is no :maxcardinality keyword in <initargs>."
  (cond ((member :maxcardinality initargs)
         (find-class 'OwlProperty-effective-slot-definition))
        ((member :subject-type initargs)
         (find-class 'gx::Property-effective-slot-definition))
        (t (call-next-method))))

(defun type-option-check-with-cardinality (instance filler slotd oldval)
  (unless slotd ; when slot is an ordinal slot.
    (return-from type-option-check-with-cardinality nil))
  (let ((type (mop:slot-definition-type slotd))
        (name (mop:slot-definition-name slotd))
        (maxc (and (slot-exists-p slotd 'maxcardinality)
                   (slot-value slotd 'maxcardinality)))
        (minc (and (slot-exists-p slotd 'mincardinality)
                   (slot-value slotd 'mincardinality))))
    (typecase type
      (null (error "Cant happen!"))
      (cons (case (op type)
              (and (mapc #'(lambda (type)
                             (satisfy-filler instance name filler type maxc minc oldval slotd))
                     (cdr type)))
              (or (error "Not Yet!"))
              (not (error "Not Yet!"))
              (otherwise ;; conjunction of type options
               (mapc #'(lambda (type) 
                         (satisfy-filler instance name filler type maxc minc oldval slotd))
                 type))))
      (symbol (case type
                ((t) (cond ((null oldval) (cardinality-ok-p filler slotd))
                           ((subsumed-p filler oldval) (cardinality-ok-p filler slotd))
                           (t (satisfy-filler instance name filler type maxc minc oldval slotd))))
                (otherwise (cond ((object? type)
                                  (setf (slot-value slotd 'excl::type)
                                    (symbol-value type))
                                  (type-option-check-with-cardinality instance filler slotd oldval))
                                 (t (error "Cant happen!"))))))
      (t (satisfy-filler instance name filler type maxc minc oldval slotd)))))

(defun satisfy-filler (x R y type maxc minc oldval slotd)
  (declare (ignore minc))
  (let ((values (remove-duplicates (append (mklist y) (mklist oldval)) :test #'owl-same-p)))
    (flet ((closed-p () (and maxc (> (length values) maxc)))
           (closing-p () (and maxc (>= (length values) maxc)))
           (exacts () (and maxc (= (length values) maxc)))
           (range-satisfy (filler)
                          (cond ((typep filler type) t) ; nothing done
                                ((eq (type-of filler) '|rdfs:Resource|)
                                 (change-class filler type))
                                ((eq (type-of filler) 'rdfs:|Resource|)
                                 (change-class filler type))
                                ((subsumed-p type (class-of filler))
                                 (warn "Range entail of ~S: change class of ~S to ~S." R filler type)
                                 (change-class filler type))
                                ((or (numberp filler) (stringp filler))
                                 (error 'range-condition-unsatisfiable
                                   :format-control "~S of ~S is disjoint to ~S in range entailment"
                                   :format-arguments (list (type-of filler) filler type)))
                                ((disjoint-p (class-of filler) type)
                                 (error 'range-condition-unsatisfiable
                                   :format-control "~S of ~S is disjoint to ~S in range entailment"
                                   :format-arguments (list (type-of filler) filler type)))
                                (t ;; shadowing
                                 (warn "range entail of ~S: change class of ~S to ~S." R filler type)
                                 (change-class filler type))))
           (forall-satisfy (filler)
                           (cond ((typep filler (forall-filler type)) t) ; nothing done
                                 ((eq (type-of filler) '|rdfs:Resource|)
                                  (change-class filler (forall-filler type)))
                                 ((eq (type-of filler) 'rdfs:|Resource|)
                                  (change-class filler (forall-filler type)))
                                 ((subsumed-p (forall-filler type) (class-of filler))
                                  (warn "allValuesFrom entailment: change class ~S to ~S."
                                    filler (forall-filler type))
                                  (change-class filler (forall-filler type)))
                                 ((or (numberp filler) (stringp filler))
                                  (error 'forall-condition-unsatisfiable
                                    :format-control "~S of ~S is disjoint to ~S in ~S."
                                    :format-arguments (list (type-of filler) filler (forall-filler type)
                                                            (mop:class-direct-subclasses (slot-subject-type type)))))
                                 ((disjoint-p (class-of filler) (forall-filler type))
                                  (error 'forall-condition-unsatisfiable
                                    :format-control "~S of ~S is disjoint to ~S in ~S."
                                    :format-arguments (list (type-of filler) filler (forall-filler type)
                                                            (mop:class-direct-subclasses (slot-subject-type type)))))
                                 (t ;; shadowing
                                  (warn "owl:allValuesFrom entail of ~S: change class of ~S to ~S." R filler (forall-filler type))
                                  (change-class filler (forall-filler type)))))
           (exists-satisfy (filler)
                           (cond ((typep filler (exists-filler type)) t) ; satisfied
                                 ((disjoint-p (class-of filler) (exists-filler type))
                                  (error "Disjoint class ~S of ~S to ~S in someValuesFrom."
                                    (type-of filler) filler (exists-filler type)))
                                 ((eq (class-of x) (slot-definition-subject-type slotd))
                                  ;; instance and this role filler is defined for this class 
                                  ;; and someValuesFromRestriction defined to this class.
                                  ;; So, filler may be defined under this restriction, may be.
                                  (warn "someValuesFrom entailment: class of ~S is changed to ~S." filler (class-name (exists-filler type)))
                                  (change-class filler (exists-filler type)))
                                 (t (if (y-or-n-p "someValuesFrom entailment: you may or may not type ~S to ~S.~%Do it?" 
                                                     filler (exists-filler type))
                                        (change-class filler (exists-filler type))
                                      nil)))) ; then others may satisfy
           (fills-satisfy (filler)
                              (cond ((owl-same-p filler (fills-filler type)) t)  ; nothing done
                                    ((definitely-owl-different-p filler (fills-filler type))
                                     (error "hasValue clash: ~S different from ~S."
                                       filler (fills-filler type)))
                                    (t (if (y-or-n-p "hasValue entailment: you may or may not define ~S same as ~S.~%Do it?" 
                                                     filler (fills-filler type))
                                           ;; push filler into same group
                                           (shared-initialize-after-for-sameAs
                                            filler (same-as-of (fills-filler type)))
                                         nil)))))
      (cond ((and maxc (minusp maxc))
             (error "Clash with max cardinality for ~S ~S." x R))
            ((and maxc (zerop maxc) x)
             (error "Clash with max cardinality for ~S ~S ~S." x R y))
            ((closed-p)
             (error "Clash with max cardinality for ~S ~S ~S ~S." x R y maxc))
            ((and (closing-p) (not (exacts)))
             (cond ((atom y)
                    (cond ((some #'(lambda (old) (owl-same-p y old)) (mklist oldval))
                           (warn "~S sames in ~S: nothing done." y oldval))
                          (t (error "Max cardinality constraint violation ~S ~S ~S into ~S"
                               x R y oldval))))
                   ((null oldval)
                    (cond ((= maxc 1)
                           (warn "Cardinality entailment: All of ~S must be same!" y)
                           (map nil #'(lambda (s)
                                        (cond ((owl-thing-p s)
                                               (setf (slot-value s 'same-as) y))
                                              (t (error 'cardinality-constraint-condition-unsatiafiable
                                                   :format-control "~S is not an OWL thing."
                                                   :format-arguments s))))
                             y))
                          ((> maxc 1)
                           (error "Cardinality entailment: ~S is given for value of ~S of ~S.~%   but max cardinality ~S~%   Some of them must be same!"
                             y R x maxc))))
                   (t (error "Not Yet!"))))
            (t (typecase type
                 (null nil)
                 (rdfs:|Class| (cond ((atom y) (range-satisfy y))
                                   (t (mapc #'(lambda (fil) (range-satisfy fil)) y))))
                 (forall (cond ((atom y) (forall-satisfy y))
                               (t (mapc #'(lambda (fil) (forall-satisfy fil)) y))))
                 (exists (cond ((atom y) (exists-satisfy y))
                               ((some #'(lambda (fil) (exists-satisfy fil)) y))
                               (t )))
                 (fills (cond ((atom y) (fills-satisfy y))
                                  (t (mapc #'(lambda (fil) (fills-satisfy fil)) y))))
                 (t (cond ((atom y) (range-satisfy y))
                          (t (mapc #'(lambda (fil) (range-satisfy fil)) y))))))))))
) ; end of excl:without-redefinition-warnings

(defProperty owl:|complementOf|)
(defConcept owl:|Ontology|)
(defProperty owl:|unionOf|)

;;;
;;;; Default Super Class in OWL
;;;
;;; If <v> is typed to owl:Class, then <v> is subtyped to owl:Thing.
;;;
;;; In order to set up the anonymous class for (owl:Class (owl:complementOf owl:Nothing)) of owl:Thing 
;;; (owl:Class owl:Thing (rdfs:label "Thing") (owl:unionOf owl:Nothing (owl:Class (owl:complementOf owl:Nothing))))
;;; this rule must be defined before reading the OWL file.

;; rule1a and rule1b by Seiji
(defmethod excl::default-direct-superclasses ((class owl:|Class|))
  (list (load-time-value (find-class 'owl:|Thing|))))

(defmethod make-instance :around ((class (eql owl:|Class|)) &rest initargs)
  (cond ((notany #'(lambda (cls)
                     (cl:subtypep cls (load-time-value owl:|Thing|)))
                 (getf initargs :direct-superclasses))
         (setf (getf initargs :direct-superclasses)
           (append (getf initargs :direct-superclasses)
                   (list (load-time-value owl:|Thing|))))
         (apply #'call-next-method class initargs))
        (t (call-next-method)))
  )

(defmethod change-class :around ((from rdfs:|Class|) (to owl:|Thing|) &rest initargs)
  "This is happen when <from> is class in RDF and it is changed into OWL."
  (when (eq from to)
    (error "Changing class of ~S to itself, membership loop happend!" from)
    (return-from change-class from))
  (format t "~%Changing(rdfs:Class owl:Thing) ~S to ~S" from to)
  (apply #'change-class from (find-class 'owl:|Class|) initargs)
  ;; then from is typed to owl:Thing through owl:Class.
  )

;;; ==================================================================================
;;;; Here OWL rdf file is read.
;;;

(defProperty owl::imports)          ; just for suppression of entailment warning
(defProperty owl::versionInfo)      ; just for suppression of entailment warning
(defProperty owl::priorVersion)     ; just for suppression of entailment warning

(eval-when (:load-toplevel)
  (read-rdf-file #'addRdfXml "OWL:OWL.rdf"))

;;; ==================================================================================
;;;
;;; Note that 
;;; (rdfs:|Class| owl:FunctionalProperty (rdfs:|subClassOf| rdf:|Property|))
;;; (rdfs:|Class| owl:InverseFunctionalProperty  (rdfs:|subClassOf| owl:ObjectProperty))
;;;
;;; Then, we add owl:Functional&InverseFunctionalProperty
;;;
(addClass `(,rdfs:|Class|) '|owl:FunctionalProperty|
          `(,(symbol-value 'owl:|FunctionalProperty|) ,(symbol-value 'owl:|ObjectProperty|)))
(addClass `(,rdfs:|Class|) 'owl::Functional&InverseFunctionalProperty
          `(,(symbol-value '|owl:FunctionalProperty|) ,(symbol-value 'owl:|InverseFunctionalProperty|)))

;;;; We add some new axioms for OWL.
;;;

;;; For OWL Full, an owl class also inherit owl:|Thing|
(addClass `(,(class-of owl:|Class|)) 'owl:|Class| `(,owl:|Thing|) ())
#|
(apply #'mop:ensure-class-using-class (find-class 'shadowed-class) 'shadowed-class
       :direct-superclasses `(,owl:|Class|)
       :metaclass (class-of (find-class 'shadowed-class))
       ())
|#
(reinitialize-instance
 (symbol-value 'owl:|allValuesFrom|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|allValuesFromRestriction|)))
(reinitialize-instance
 (symbol-value 'owl:|someValuesFrom|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|someValuesFromRestriction|)))
(reinitialize-instance 
 (symbol-value 'owl:|hasValue|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|hasValueRestriction|)))
(reinitialize-instance 
 (symbol-value 'owl:|minCardinality|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|cardinalityRestriction|)))
(reinitialize-instance 
 (symbol-value 'owl:|maxCardinality|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|cardinalityRestriction|)))
(reinitialize-instance 
 (symbol-value 'owl:|cardinality|)
 'rdfs:|domain| (load-time-value (symbol-value 'owl:|cardinalityRestriction|)))
(eval-when (:execute :load-toplevel)
  (let ((slots
         (remove 'owl:|allValuesFrom|
                 (remove 'owl:|hasValue|
                         (remove 'owl:|someValuesFrom|
                                 (remove 'owl:|minCardinality|
                                         (remove 'owl:|maxCardinality|
                                                 (remove 'owl:|cardinality|
                                                         (mop:class-direct-slots owl:|Restriction|)
                                                         :key #'name)
                                                 :key #'name)
                                         :key #'name)
                                 :key #'name)
                         :key #'name)
                 :key #'name)))
    (slot-makunbound owl:|Restriction| 'excl::direct-slots)
    (setf (mop:class-direct-slots owl:|Restriction|) slots)
    )
  )

(reinitialize-instance (load-time-value (symbol-value 'owl:|Thing|))
                       :complement-class (load-time-value (symbol-value 'owl:|Nothing|))
                       :different-from (list (load-time-value (symbol-value 'owl:|Nothing|)))
                       :disjoint-classes (list (load-time-value (symbol-value 'owl:|Nothing|))))

(reinitialize-instance (load-time-value (symbol-value 'owl:|Nothing|))
                       :complement-class (load-time-value (symbol-value 'owl:|Thing|))
                       :different-from (list (load-time-value (symbol-value 'owl:|Thing|)))
                       :disjoint-classes (list (load-time-value (symbol-value 'owl:|Thing|))))

;;;
;;;; %addForm for OWL
;;; Calling sequence: %addForm (<type> <slots> <role>)
;;; When <type> is an undefined symbol as resource,
;;; # If <type> is a symbol rdf:Description and <role> is a symbol owl:intersectionOf, 
;;;   owl:unionOf, then owl:Class is used for <type>. See, rule2a and rule2b.
;;; # If <role> is a symbol owl:distinctMembers or owl:oneOf, then owl:Thing is used for <type>. 
;;;   See. rule3.

(excl:without-redefinition-warnings
(defun get-range-constraint-from (role)
  "This is same as one in RDFS module except owl:intersectionOf, owl:unionOf returns owl:Class, 
   and owl:oneOf returns owl:Thing instead of rdf:List."
  (case role
    ((nil) nil)
    ((t) nil)
    ((owl:|intersectionOf| owl:|unionOf|)
     ;; The range of these properties is rdf:List, but the range constraint should be owl:Class 
     ;; for list elements.
     owl:|Class|)
    ((owl:|oneOf|) rdfs:|Resource|)
    ((owl:|distinctMembers|) owl:|Thing|)
    (otherwise 
     (when (boundp role)
       (let ((range (get-range (symbol-value role))))
         (if (eql range rdf:|List|) rdfs:|Resource| range))))))
)

;;
;; Magic change-class, I don't know why, but this is needed absolutely.
;;

(defmethod change-class :before ((from cl:class) (to cl:class) &rest initargs) ;bug3166
  (declare (ignore initargs))		;bug3166
  (unless (mop:class-finalized-p to)	;bug3254
    (mop:finalize-inheritance to))
  (format nil "~S" (mop:class-prototype to))    ; this is magic code.
  (unless (excl::validate-metaclass-change from (mop:class-prototype to))
    (excl::.program-error "validate-metaclass-change forbids changing the class of ~s to ~s"
                          from to)))

(defmethod change-class :after ((class rdfs:|Class|) (new-class (eql owl:|Class|))  &rest initargs)
  (declare (ignore initargs))
  (unless (cl:subtypep class (load-time-value owl:|Thing|))
    (reinitialize-instance class :direct-superclasses `(,(load-time-value owl:|Thing|)))))

(defmethod change-class :after ((class rdfs:|Class|) (new-class owl:|Class|)  &rest initargs)
  (declare (ignore initargs))
  (unless (cl:subtypep class (load-time-value owl:|Thing|))
    (reinitialize-instance class :direct-superclasses `(,(load-time-value owl:|Thing|)))))

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;
