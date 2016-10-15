;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Slot Definition Module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2009 Seiji Koide
;;;
;; History
;; -------
;; 2008.11.05    File created and slot definition parts are moved here from RDFBoot.
;;; ==================================================================================

(cl:provide :slotdef)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
) ; end of eval-when

(defpackage :gx
  (:export collect-prop-names-from)
  )

(in-package :gx)

;;;; Slot definitions for SWCLOS
;;;
;;; The standard slot definition in CLOS (mop:standard-slot-definition) is also a CLOS object 
;;; and includes the information for slot creation of instances, i.e., initargs, type, 
;;; documentation and so on. Hereafter, we call a slot in slot definition object an option of 
;;; slot. So, we say, for example, the standard slot definition has an initargs option, a type 
;;; option, and documentation option, and so on. 
;;;
;;; The slot with respect to rdf property is an instance of <Property-direct-slot-definition>, 
;;; which is a specialized class of <mop:standard-direct-slot-definition>. In addition to CLOS 
;;; native options, <Property-direct-slot-definition> has a <subject-type> option, and the slot 
;;; with respect to owl specific property is an instance of <OwlProperty-direct-slot-definition>, 
;;; which is a specialized class of <Property-direct-slot-definition>. It has a <maxcardinality> 
;;; and a <mincardinality> option. 
;;;
;;; The <type> option of CLOS native options is used as the constraint for the slot value type, 
;;; and <maxcardinality> and <mincardinality> options are used as cardinality constraints for a 
;;; number of slot values. 
;;;
;;; The <subject-type> option in <Property-direct-slot-definition> is used as a inverse link to a 
;;; subject class to which the slot definition itself is attached. Note that a 
;;; <subject/predicate/object> triple in RDF is realized as instance-object/slot-name/slot-value 
;;; in CLOS. Thus, the specialized slot-definition <Property-effective-slot-definition> and 
;;; <OwlProperty-effective-slot-definition> in SWCLOS represents a set of triples such that the 
;;; <subject> is typed to <subject-type> and <predicate> is the same as <slot-name>. Therefore, 
;;; all triples <subject/predicate/object> can be retrieved by collecting all slot values through 
;;; this inverse link from these slot-definitions using slot-value function. Note that all slot 
;;; definitions on a property is booked into the property resource objects.

(defparameter *default-slot-definition-class* 'gx::Property-direct-slot-definition
  "Symbol 'Property-direct-slot-definition' is set to this parameter in RDF. This value is overwritten 
by OWL module. This value directs the default class for slot definition. See 
<mop:direct-slot-definition-class> method in RDFboot module.")

(defclass gx::Property-direct-slot-definition (mop:standard-direct-slot-definition)
  ((subject-type :initarg :subject-type :accessor slot-definition-subject-type))
  (:documentation "defines a subject-type option."))
(defclass gx::Property-effective-slot-definition (mop:standard-effective-slot-definition)
  ((subject-type :initarg :subject-type :accessor slot-definition-subject-type))
  (:documentation "An instance of this class has a subject-type option in which a class of 
subject in triple is stored."))

(defclass OwlProperty-direct-slot-definition (gx::Property-direct-slot-definition)
  ((maxcardinality :initarg :maxcardinality :initform () 
                   :accessor slot-definition-maxcardinality)
   (mincardinality :initarg :mincardinality :initform () 
                   :accessor slot-definition-mincardinality))
  (:documentation "defines a max and min cardinality options."))
(defclass OwlProperty-effective-slot-definition (gx::Property-effective-slot-definition)
  ((maxcardinality :initarg :maxcardinality :initform () 
                   :accessor slot-definition-maxcardinality)
   (mincardinality :initarg :mincardinality :initform () 
                   :accessor slot-definition-mincardinality))
  (:documentation "An instance of this class has a max and min cardinality options, 
which work as the constraint for settable number of values."))

;;;
;;;; Slot Predicates
;;;

(defun property-direct-slotd-p (slotd)
  "returns true if <slotd> is an instance of Property-direct-slot-definition."
  (declare (inline))
  (or (eql (class-name (class-of slotd)) 'gx::Property-direct-slot-definition)
      (cl:typep slotd 'gx::Property-direct-slot-definition)))

(defun property-effective-slotd-p (slotd)
  "returns true if <slotd> is an instance of Property-effective-slot-definition."
  (declare (inline))
  (or (eq (class-name (class-of slotd)) 'gx::Property-effective-slot-definition)
      (cl:typep slotd 'gx::Property-effective-slot-definition)))

(defun owl-property-direct-slotd-p (slotd)
  "returns true if <slotd> is an instance of OwlProperty-direct-slot-definition."
  (declare (inline))
  (or (eql (class-name (class-of slotd)) 'OwlProperty-direct-slot-definition)
      (cl:typep slotd 'OwlProperty-direct-slot-definition)))

(defun owl-property-effective-slotd-p (slotd)
  "returns true if <slotd> is an instance of OwlProperty-effective-slot-definition."
  (declare (inline))
  (or (eq (class-name (class-of slotd)) 'OwlProperty-effective-slot-definition)
      (cl:typep slotd 'OwlProperty-effective-slot-definition)))

(defun collect-prop-names-from (class)
  "collect direct and inherited property (slot) names on this class and returns a list of them."
  (mapcar #'mop:slot-definition-name
    (remove-if-not #'property-effective-slotd-p
                   (if (mop:class-finalized-p class)
                       (mop:class-slots class)
                     (mop:compute-slots class)))))

;;;
;;;; How a Type Value is set in Direct Slot Definition
;;;
;;; There are four ways in RDF and two in OWL to suppy the type option in initargs parameter for 
;;; shared-initialize method of <Property-direct-slot-definition>, i.e., 
;;; # When a property is defined with domain constraint, <add-direct-slots-to-domain> function adds 
;;;   a direct slot definition into the domain class. In this case, the range constraint of property 
;;;   is retrieved from the property object and supplied for type option in the slot definition.
;;;   See <add-direct-slots-to-domain>.
;;; # When an RDF instance or class is newly made or reinitialized, <ensure-class-slotds> is invoked 
;;;   for the class of the object to ensure class slot definitions. In this case, the range constraint 
;;;   of property is retrieved from the property object and supplied for type option in the slot 
;;;   definition. See <ensure-class-slotds>.
;;; # When change-class is invoked, if the instance to be changed has a slot value and the new class 
;;;   has no definition on the slot, the old slot definition is copied into the new class. 
;;;   See <change-class>:before(rdfs:Resource rdfs:Class). 
;;; # When <put-value>(rdfs:Resource rdf:Property) is invoked but the subject inherits no slot definition, 
;;;   the slot definition is added to the class of subject. See <put-value>.
;;; # When an instance of owl:allValuesFromRestriction is made or redefined, the constraint is transfered 
;;;   into the type option in the slot definition that is attached to the restriction with slot name 
;;;   from onProperty value. See <shared-initialize>:after(owl:allValuesFromRestriction).
;;; # When an instance of owl:someValuesFromRestriction is made or redefined, the constraint is transfered 
;;;   into the type option in the slot definition that is attached to the restriction with slot name 
;;;   from onProperty value. See <shared-initialize>:after(owl:someValuesFromRestriction).
;;;
#|
;; To get a newest option value of type in current situation, use <mop:slot-definition-type>. 
;; It is specialized for <Property-direct-slot-definition> and <Property-effective-slot-definition> 
;; in order to activate a get daemon. 
;; See also <mop:slot-definition-type> before method in GxType module.
|#
#|
(defmethod shared-initialize :after ((slotd gx::Property-direct-slot-definition) slot-names
                                      &key (name nil) (type t typep))
  "The <type> value is set in the primary method of this method.
   However, this after method makes sure the most specific concepts are set among given value <type> 
   in <initargs> and the value retrieved from range constraints of property <name>."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq slot-names t) ; when first
         (cond ((eq name (mop:slot-definition-name slotd))
                ;; definition of property itself, so cannot invoke get-range.
                ;; the type option must be set up at an upper method before this after method invoking, 
                ;; if it is supplied in initargs.
                ;; see ensure-class-slotds
                )
               ((property? name)
                (let ((range (or (get-range (symbol-value name)) t)))
                  (error "Bingo! Please delete this error invocation.")
                  (format t "~%Range in shared-initialize: ~S" range)
                  (when (consp range)
                    (case (car range)
                      ((forall exists fills) nil)
                      ((and or not) nil)
                      (otherwise (setq range (cons 'and range)))))
                  (setf (slot-value slotd 'excl::type)
                    (cond ((cl:subtypep type range) type)
                          ((cl:subtypep range type) range)
                          ((and (consp range) (eq (car range) 'and)
                                (consp type)  (eq (car type) 'and))
                           `(and ,@(cdr range) ,@(cdr type)))
                          ((and (consp range) (eq (car range) 'and))
                           `(and ,@(cdr range) ,type))
                          ((and (consp type) (eq (car type) 'and))
                           `(and ,range ,@(cdr type)))
                          (t `(and ,range ,type))))
                  (format t "~%TYPE in direct-slotd:~S input:~S range:~S"
                    (slot-value slotd 'excl::type) type range)))))
        ((and typep (consp slot-names))
         (error "Cant happen?"))))
|#
;;;
;;;; How to Compute a Type Value in Effective Slot Definition
;;;
;;; See the same comment as above in RDFboot module.
;;;
;;; Note that rdf property name is also slot definition name, and every property object in CLOS keeps effective 
;;; slot definition objects for housekeeping. 
;;; When an effective-slot-definition object is created, the name of slotd is a property name, and this slotd 
;;; is stored into slotd slot of the property. The following method do it.

;; This is also invoked for OwlProperty-direct-slot-definition
(defmethod shared-initialize :after ((slotd gx::Property-effective-slot-definition)  slot-names
                                      &key (name nil))
  "When the first definition of <slotd>, updates the value of slotds slot in the property. Namely, 
   the old relevant slotd in slotds of the property is removed, and this <slotd> is added into 
   the slotds of the property."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq slot-names t) ; when first
         ;(assert (eq name (mop:slot-definition-name slotd))) ; name is always name
         (cond ((and (boundp name) (cl:typep (symbol-value name) 'rdf:|Property|)) ; instead of (property? name)
                (let ((prop (symbol-value name)))
                  (let ((slotds (slot-value prop 'slotds))
                        (subject-type (slot-definition-subject-type slotd)))
                    (setf (slot-value prop 'slotds)
                      (cons slotd (remove subject-type slotds :key #'slot-definition-subject-type))
                      ;; this remove form is requisite against wasteful data, do not touch.
                      ;; maybe old slotd is included in slotds with non-eql 
                      ))))))))

;;;
;;;; Property Predicates
;;;

(defun property-p (x)
  "returns true if <x> is an instance of rdf property."
  (declare (inline))
  (cl:typep x 'rdf:|Property|))

(defun property? (name)
  "returns true if <name> is an rdf property name"
  (declare (inline))
  (case name
    ((rdfs:|subClassOf| rdfs:|label| rdfs:|comment| rdfs:|isDefinedBy| rdfs:|domain| rdfs:|range| rdfs:|subPropertyOf|)
     t)
    (otherwise 
     (and (boundp name) (cl:typep (symbol-value name) 'rdf:|Property|)))))

(defmethod name ((object mop:standard-direct-slot-definition))
  "returns a name of <object>, if it is named, otherwise nil."
  (mop:slot-definition-name object))

(defmethod name ((object mop:standard-effective-slot-definition))
  "returns a name of <object>, if it is named, otherwise nil."
  (mop:slot-definition-name object))

;;;
;;;; Slot Definition for owl:oneOf 
;;;
;;; Note that the slot definition for owl:oneOf is an instance of <gx::Property-effective-slot-definition>
;;; rather than <OwlProperty-direct-slot-definition>. It depends on method <mop:direct-slot-definition-class> 
;;; and rdfs:Class.

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Sep-04-2009
;;;
