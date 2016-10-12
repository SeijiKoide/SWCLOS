;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdfs Kernel module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007 Seiji Koide
;;
;; History
;; -------
;; 2009.10.16    collect-all-instances is renamed to collect-all-instances-of.
;; 2009.10.16    instance-p is renamed to rdf-instance-p.
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2009.01.07    Rdfs vocabulary part is separated to RdfsObjects module.
;; 2009.01.07    domain and range part is separated to DomainRange module.
;; 2008.12.11    resource-p is renamed to rdf-object-p.
;; 2007.11.20    finalize-inheritance protocol is moved to ForwardRef
;; 2007.11.08    GxType file is separated from rdfskernel
;; 2005.03.08    boot file is separated from rdfskernel
;; 2004.12.22    booting sequence is drastically changed
;; 2004.12.17    property direct-instances are created
;; 2004.12.16    shadowed-class is created
;; 2004.07.28    Rdfs file is split out into RdfsKernel and RdfsCore.
;; 2004.03.13    name is provieded for ID and rdfs:label is set up as just label for print out.
;; 2004.01.14    rdf parser is placed in rdf module and package declarations are moved there.
;; 2004.01.09    parse-rdf for XML parser is prepared.
;; 2003.11.29    Utilities are separated to utils.cl file.
;; 2002.08.15    File created
;;; ==================================================================================

(cl:provide :rdfskernel)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclosutils)
  (require :swclospackages)
  (require :namespace)
  (require :rdfboot)
  ;(require :gxtype)
  ;(require :rdfsobjects)
  ;(require :domainrange)
) ; end of eval-when

(in-package :gx)

(export 'subproperty)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (uri-namedspace-package (set-uri-namedspace "http://www.w3.org/2001/XMLSchema#"))
    (find-package :xsd))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :xsd) t)))
    (find-package :xsd))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :rdf) t)))
    (find-package :rdf))
  (setf (uri-namedspace-package (set-uri-namedspace (documentation (find-package :rdfs) t)))
    (find-package :rdfs))
  )

;;;; MOP Programming Layer out of Three Layers in RDF(S) and OWL Definition
;;;
;;; There are three layers for resource definition, i.e., 
;;; (i) top level macro layer, 
;;; (ii) intermediate function/method layer, and 
;;; (iii) MOP programming layer.
;;; The top level macro layer, which includes three macros, <defConcept>, <defProperty>, and <defIndividual>,
;;; allows users to input RDF(S) and OWL entities in S-expression just in the same feeling as defining classes 
;;; or structures in lisp. 
;;; The intermediate layer composed of several functions and methods mediates inputs at the top level to the 
;;; MOP programming layer. 
;;; The MOP programming layer constitutes a bunch of MOP methods that are programmed in order to tailor 
;;; the CLOS orignal functionality to RDF(S) and OWL semantics using the Meta-Object Protocol (MOP).
;;;
;;; This file includes (iii) the MOP programming layer. 
;;; The other layers are contained in Rdfs Core module.
;;;
;;;; Method Invocation Orders and their Procedures
;;;
;;; The procedure of make-instance is as follows.
;;; # make-instance:around ((eql owl:Class)), see leanOWL file
;;;   - make sure this class has owl:Thing as superclass
;;; # make-instance:around (owl:Class), see leanOWL file
;;;   - make sure this class has owl:Thing as superclass
;;; # make-instance:around (rdfs:Class), see RdfsCore file
;;;   - ensure slot definitions for this <class>, then domain constraints are also taken care
;;; # make-instance:around (rdfsClass), see RdfsCore file
;;;   - ensure slot definitions for this <class>, then domain constraints are also taken care
;;; There is no initialize-instance in this implementation.
;;; 
;;; The procedure of reinitialize-instance is as follows.
;;; # reinitialize-instance:around (rdfs:Class)
;;;   - remove direct-slots initarg and its value, if its value is nil.
;;; # reinitialize-instance:before (rdfs:Resource)
;;;   - after ensuring slot definitions at the class of instance, change-class if more special type(s) is derived from domain constraints
;;;
;;; The procedure of shared-initialize is as follows.
;;; # shared-initialize:around (rdf:Property)
;;;   - if both old and new domain/range exist, rewrite initargs with their MSC
;;; # shared-initialize:around (rdfs:Class)
;;;   - assure the MSG for direct-superclasses, if they exist in initargs
;;;   - in redefinition, prepare all old slot difinitions and merge them with new definitions in initargs, 
;;; # shared-initialize :around (rdfsClass)
;;; # shared-initialize:after(gnode)
;;;   - maintain mclasses slot and direct-instances slot
;;; # shared-initialize:after (rdfs:Resource)
;;;   - book keeping for reification
;;; # shared-initialize:after (owl:Class)
;;;   - subClassOf
;;;   - intersectionOf
;;;   - unionOf
;;;   - equivalentClass
;;;   - disjointWith
;;;   - complementOf
;;; # shared-initialize:after (owl:Thing)
;;;   - owl:sameAs
;;;   - owl:differentFrom
;;;   - functional property maintenance
;;;   - symmetric property maintenance
;;;   - transitive property maintenance
;;;   - oneOf check
;;;   - refine instance in OWL
;;; # shared-initialize:after (OneOf)
;;; # shared-initialize:after(rdf:Property)
;;;   - super/sub property book-keeping
;;;   - equivalent property maintain
;;;   - define this property slot to domain, if suplied
;;;   - domain checking and range checking to every related extensions.
;;;
;;; The rule rdfs8 indicates the default top class in triple should be rdfs:Resource.
;;; The rule rdfs13 indicates the default data type in triple should be rdfs:Datatype.
;;; The following methods assure rdfs8 and rdfs13 rules.

(defmethod excl::default-direct-superclasses ((class rdfs:Class))
  "Rdfs8 rule is implemented at this method."
  (list (load-time-value (find-class 'rdfs:Resource))))

(defmethod excl::default-direct-superclasses ((class rdfs:Datatype))
  "Rdfs13 rule is implemented at this method."
  (list (load-time-value (find-class 'rdfs:Literal))))

(defmethod make-instance :around ((class (eql rdfs:Class)) &rest initargs)
  (cond ((notany #'(lambda (cls) (cl:subtypep cls (load-time-value rdfs:Resource)))
                 (getf initargs :direct-superclasses))
         (setf (getf initargs :direct-superclasses)
           (append (getf initargs :direct-superclasses) (list (load-time-value rdfs:Resource))))
         (apply #'call-next-method class initargs))
        (t (call-next-method)))
  )

(defmethod make-instance :around ((class (eql rdfs:Datatype)) &rest initargs)
  (cond ((notany #'(lambda (cls) (cl:subtypep cls (load-time-value rdfs:Literal)))
                 (getf initargs :direct-superclasses))
         (setf (getf initargs :direct-superclasses)
           (append (getf initargs :direct-superclasses) (list (load-time-value rdfs:Literal))))
         (apply #'call-next-method class initargs))
        (t (call-next-method)))
  )

;;
;; Asyclic Property Terminology Error
;;

(define-condition asyclic-property-termilogy-error (error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "asyclic property terminology: ~A" 
         (apply #'format nil fcont args)))))
  )

;;
;; Metamodeling Error
;;

(define-condition metamodeling-error (error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "metamodeling error: ~A" 
         (apply #'format nil fcont args)))))
  )

;;;
;;; Class Precedence List
;;;

;(defmethod mop:compute-class-precedence-list :after ((root class))
;  )
;;;
;;;; Shared-initialize for Property
;;;
;;; # <shared-initialize>:around(rdf:Property)
;;;   - When redefining a property, if both olddomains and newdomains are exists, 
;;;   the most specific concepts (MSC) are set for domain initargs, and if both oldranges and newrages are exists, 
;;;   the most specific concepts (MSC) are set for range initargs. Otherwise nothing done for initargs. The next method 
;;;   processes initargs regularly.
;;; # <shared-initialize>:after (rdf:Property)
;;;   - When rdfs:subPropertyOf is supplied, the inverse relation is registered for book-keeping.
;;;   - When owl:equivalentPropety is supplied, the equivalent property group is maintained.
;;;   - When rdfs:domain is supplied, the direct-slot-definition for this property is added to the directed domain class.
;;;   - When rdfs:domain is supplied, the domain constraints are checked for every subject of the extension of this property.
;;;   - When rdfs:range is supplied, the range constraints are checked for every object of the extension of this property.
#|
(defmethod shared-initialize :around ((instance rdf:Property) slot-names &rest initargs)
  "In order to affect new definition of rdfs:subPropertyOf to inherited domain and range constraints
in addition to the effect of direct definitions of domain and range in <initargs>, <get-domain> and 
<get-range> are used and before-value and after-value are compared. If there is any difference, 
<domain-check-for-instance> and <slot-value-range-check> are applied to all the subjects and objects 
of this <instance> property."
  (cond ((and (null slot-names) (null initargs))  ; when change-class
         (call-next-method))
        ((and (consp slot-names) (null initargs)) ; when propagated
         (call-next-method))
        ((eq slot-names t)  ; first
         (multiple-value-prog1 (call-next-method)
           (let ((newdomain (get-domain instance)) ; this includes new definition for new super property.
                 (newrange (get-range instance))  ; this includes new definition for new super property.
                 (prop-name (name instance)))
             (when (or newdomain newrange)
               (loop for slotd in (slot-value instance 'slotds)
                   do (loop for individual
                          in (collect-all-instances-of (slot-definition-subject-type slotd))
                          do ;; propagate the domain and range effect for adding this property
                            ;(format t "~%NewDomain:~S NewRange : ~S" newdomain newrange)
                            (when (not (eql instance individual))
                              (warn "Propergating domain constraint of ~S to individuals ..." prop-name)
                              (domain-check-for-instance individual newdomain))
                            (when (not (eql instance individual))
                              (warn "Propergating range constraint of ~S to individuals ..." prop-name)
                              (let ((values (and (slot-exists-p individual prop-name) ;seiji 2006/2/17
                                                 (slot-boundp individual prop-name)
                                                 (slot-value individual prop-name))))
                                (when values
                                  (slot-value-range-check prop-name values newrange))))))))))
        (initargs ;; redefining, or adding domain and/or range, or adding owl:inverseOf/inverse-inverseOf from update-instance
         ;(format t "~%SHARED-INITIALIZE:AROUND(rdf:Property) redefining ~S~%  slots ~S~%  with ~S" instance slot-names initargs)
         (let ((olddomain (get-domain instance))
               (oldrange (get-range instance)))
           (multiple-value-prog1 (call-next-method)
             (let ((newdomain (get-domain instance)) ; this includes new definition for new super property.
                   (newrange (get-range instance))  ; this includes new definition for new super property.
                   (prop-name (name instance)))
               (when (or (and newdomain (or (null olddomain) (not (set-equalp (mklist newdomain) (mklist olddomain)))))
                         (and newrange  (or (null oldrange)  (not (set-equalp (mklist newrange)  (mklist oldrange))))))
                 ;(format t "~%  Checking new domains ~S or ranges ~S concstraints ..." newdomain newrange)
                 (loop for slotd in (slot-value instance 'slotds)
                     do (loop for individual
                            in (collect-all-instances-of (slot-definition-subject-type slotd))
                            do ;; propagate the domain and range effect for adding this property
                              ;(format t "~%  NewDomain:~S NewRange:~S" newdomain newrange)
                              (when (not (eql instance individual))
                                (warn "Propergating domain constraint of ~S to individuals ..." prop-name)
                                (domain-check-for-instance individual newdomain))
                              (when (not (eql instance individual))
                                (warn "Propergating range constraint of ~S to individuals ..." prop-name)
                                (let ((values (and (slot-exists-p individual prop-name) ;seiji 2006/2/17
                                                   (slot-boundp individual prop-name)
                                                   (slot-value individual prop-name))))
                                  (when values 
                                    (slot-value-range-check prop-name values newrange)))))))
               ))))))
|#

(defmethod shared-initialize :after ((instance rdf:Property) slot-names &rest initargs)
  "After regular processing, property specific procedure is processed here, i.e., book-keeping for 
   super/sub relation maintenance, the equivalent property group maintencne, adding slot definition to 
   the domain class, and finally constraint propergation of domain and range constraints."
  ;(format t "SHARED-INTIALIZE:AFTER((~S rdf:Property) ~S &rest ~S)~%" instance slot-names initargs)
  (cond ((and (null slot-names) (null initargs))  ; when change-class
         )
        ((and (consp slot-names) (null initargs)) ; when propagated
         )
        (t  ; first or redefining, or when undate-instance-fore-different-class with added slot
         (apply #'book-keeping-super/sub-property instance slot-names initargs)
         (apply #'equivalentProperty-maintain instance slot-names initargs)
         (when (getf initargs 'rdfs:domain)
           (add-direct-slots-to-domain instance (getf initargs 'rdfs:domain)))
         (cond (initargs ; first and reinitialize
                (let ((newrange (getf initargs 'rdfs:range))
                      (prop-name (name instance)))
                  (declare (ignore newrange prop-name))
                  #|
           (when (or newdomain newrange)
             (loop for slotd in (slot-value instance 'slotds)
                 do (loop for individual
                        in (collect-all-instances-of (slot-definition-subject-type slotd))
                        as values = (and (slot-exists-p individual prop-name) ;seiji 2006/2/17
                                         (slot-boundp individual prop-name)
                                         (slot-value individual prop-name))
                        do ;; propagate the domain and range effect for adding this property
                          ;(format t "~%Newdomain:~S Newrange:~S Values:~S" newdomain newrange values)
                          (when (and newdomain (not (eql instance individual)))
                            (warn "Propergating domain constraint of ~S to individuals ..." (name instance))
                            (domain-check-for-instance individual (get-domain instance)))
                          (when (and newrange values (not (eql instance individual)))
                            (warn "Propergating range constraint of ~S to individuals ..." (name instance))
                            (slot-value-range-check values (get-range instance)))))) |#
                  ))
               )
         ;(apply #'delete-direct-slots-from-domain instance slot-names initargs)
         ;(apply #'update-onPropertyConstraints-for instance slot-names initargs)
         )))

;(defun update-onPropertyConstraints-for (instance slot-names &rest initargs)
;  (declare (ignore instance slot-names initargs))
;  nil)

(defun book-keeping-super/sub-property (property slot-names &rest initargs)
  "puts this property into the subproperty slot of super property."
  (declare (ignore slot-names))
  ;; inverse relation of rdfs:subPropertyOf
  (loop for super in (mklist (getf initargs 'rdfs:subPropertyOf))
      do (when (or (eql (name super) (name property))
                   (subproperty-p super property))
           (error 'asyclic-property-termilogy-error
             :format-control "<~S, ~S>"
             :format-arguments (list property super)))
        (etypecase super
          ;(symbol
          ; (cond ((property? super)
          ;        (pushnew property (slot-value (symbol-value super) 'subproperty)))
          ;       (t (error "Cant happen! The super property must be instantiated at upper level."))))
          (rdf:Property 
           (pushnew property (slot-value super 'subproperty)))
          )))

(defun equivalentProperty-maintain (instance slot-names &rest initargs)
  "This is hook for OWL module. See <equivalentProperty-maintain> in OWL module."
  (declare (ignore instance slot-names initargs))
  nil)

;;;
;;;; Slot Attaching Functions
;;;

(defun add-direct-slots-to-domain (instance newdomain)
  "add the direct slot definitions to new domains from <initargs> for this property definition."
  (loop for domain in (mklist newdomain)
      when (and (rsc-object-p domain)
                (not (associated-p instance domain)))
      do (let* ((slot-name (name instance))
                (range (get-range instance))
                (prop-forms
                 (cond ((null range)
                        `(:name ,slot-name :initargs (,slot-name)
                                :documentation "By domain definition of this property"
                                :subject-type ,domain))
                       ((eq range rdf:List)
                        ;; in case of rdf:List, it should be transparent
                        `(:name ,slot-name :initargs (,slot-name) :type t
                                :documentation "By domain definition of this property"
                                :subject-type ,domain))
                       ((atom range)
                        `(:name ,slot-name :initargs (,slot-name) :type ,range
                                :documentation "By domain definition of this property"
                                :subject-type ,domain))
                       (t ;; list
                        (case (car range)
                          ((forall exists fills) nil)
                          ((and or not) nil)
                          (otherwise (setq range (cons 'and range))))
                        ;(format t "~%Range in ADD-DIRECT-SLOTS-TO-DOMAIN: ~S" range)
                        (assert (or (atom range) (not (member nil range))))
                        `(:name ,slot-name :initargs (,slot-name) :type ,range
                                :documentation "By domain definition of this property"
                                :subject-type ,domain)))))
           ;(format t "~%Adding slotDef ~S~% to domain ~S for ~S" prop-forms domain instance)
           ;; shared-initialize for :direct-slots are customized to only add but not delete 
           ;; slots
           (reinitialize-instance domain :direct-slots `(,prop-forms)))))

(defun associated-p (role domain)
  "does this <domain> has directly the slot definition of this <role>?"
  (cond ((and (symbolp domain) (object? domain))
         (associated-p role (symbol-value domain)))
        ((rsc-object-p domain)
         (if (member role (mop:class-direct-slots domain)
                        :key #'mop:slot-definition-name)
             t nil))))
#|
(defun delete-direct-slots-from-domain (instance slot-names &rest initargs)
  "delete the direct slot definitions from old domains from <initargs> for this property 
   definition."
  (declare (ignore slot-names))
  (let ((deletes (remove-if #'(lambda (domain)
                                 (member (name domain) (getf initargs 'rdfs:domain)))
                            (mklist (domain-value instance)))))
    ;; Seiji Seiji Seiji Seiji
    (declare (ignore deletes))
    #|
    (loop for domain in deletes
        when (and (rsc-object-p domain)
                  (associated-p instance domain))
        do (let ((direct-slotds (mop:class-direct-slots domain)))
             (slot-unbound domain 'excl::direct-slots)
             (setf (mop:class-direct-slots domain)
               (remove (name instance) direct-slotds :key #'mop:slot-
                  (range (get-range instance))
                  (prop-forms
                   (cond (range
                          (cond ((eq range rdf:List)
                                 ;; in case of rdf:List, it should be transparent
                                 `(:name ,slot-name :initargs (,slot-name) :type t
                                         :documentation "By domain definition of this property"
                                         :subject-type ,domain))
                                (t ;; range may be a list
                                 (setq range (cond ((atom range) range)
                                                   ((eq (car range) 'or) range)
                                                   (t (cons 'and range))))
                                 `(:name ,slot-name :initargs (,slot-name) :type ,range
                                         :documentation "By domain definition of this property"
                                         :subject-type ,domain))))
                         (t `(:name ,slot-name :initargs (,slot-name)
                                    :documentation "By domain definition of this property"
                                    :subject-type ,domain)))))
             ;(format t "~%Adding slot ~S to domain ~S for ~S" prop-forms domain instance)
             ;; shared-initialize for :direct-slots are customized to only add but not delete slots
             (reinitialize-instance domain :direct-slots `(,prop-forms)))))
|#
    ))
|#

;;;
;;;; Shared Initialize for rdfs:Resource
;;;
;;; When any setting value in initargs is already set in the existing slot, it is eliminated from 
;;; initargs. This is for supressing meaningless redefining messages.

(defmethod shared-initialize :around ((instance rdfs:Resource) slot-names &rest initargs)
  ;(format t "~%SHARED-INITIALIZE:AROUND(rdfs:Resource) ~S ~S ~S" instance slot-names initargs)
  (cond ((and (null slot-names) (not (null initargs)))
         (let ((args
                (loop for (initarg val) on initargs by #'cddr
                    append (cond ((or (not (property? initarg))                ; if not property
                                      (not (slot-exists-p instance initarg))   ; or no slot
                                      (not (slot-boundp instance initarg)))    ; or no slot value
                                  (list initarg val))
                                 ((and (rdf-class-p instance)
                                       (not (mop:class-finalized-p instance)))
                                  (list initarg val))
                                 (t (list initarg
                                          (compute-slot-value val
                                                              (slot-value instance initarg)
                                                              (find initarg (mop:class-slots (class-of instance))
                                                                    :key #'mop:slot-definition-name)
                                                              instance)))))))
           (if (equal args initargs) (call-next-method)
             (apply #'call-next-method instance slot-names args))))
        (t (call-next-method))))

;;;
;;;; Shared Initialize for rdfs:Class
;;;
;;; Note that even if you want to add more abstract concept as superclass, 
;;; you cannot do it when system knows the MSC concept that is more specific than your indication.

(defmethod shared-initialize :around ((class rdfs:Class) slot-names &rest initargs)
  "When initialization rewrite direct-superclasses with MSCs of direct-superclasses in 
   <initargs>. When reinitialization rewrite direct-superclasses with MSCs of old superclasses 
   and new superclasses, then, old direct slot definitions are recovered into def-form and 
   merged with new definition."
  ;(format t "~%SHARED-INITIALIZE:AROUND(rdfs:Class) ~S ~S ~S" class slot-names initargs)
  (cond ((and (null slot-names) (null initargs))  ; when change-class
         (call-next-method))
        ((consp slot-names)         ; when propagated from update-instance-for-xxxxxx-class
         (call-next-method))
        ((eq slot-names t)                        ; when the first definition
         ;(format t "~%SHARED-INITIALIZE:AROUND(rdfs:Class) first definition ~S~%  with ~S" class initargs)
         (cond ((getf initargs :direct-superclasses)
                ;(setq initargs (copy-list initargs))
                (setf (getf initargs :direct-superclasses)
                  (make-this-supers class (getf initargs :direct-superclasses)))
                (apply #'call-next-method class slot-names initargs))
               (t (call-next-method))))
        ((and (null (getf initargs :direct-superclasses))
              (null (getf initargs :direct-slots)))
         ;; reinitialization but no supers and no slot definitions
         (call-next-method))
        (t ;; when reinitializing with supers or slot definitions
         ;(format t "~%SHARED-INITIALIZE:AROUND(rdfs:Class) reinitialize ~S~%  new slots ~S~%  with ~S" class slot-names initargs)
         (let ((oldsupers (mop:class-direct-superclasses class))
               (newsupers (getf initargs :direct-superclasses)))
           (when newsupers
             (setq newsupers (make-this-supers class (append oldsupers newsupers)))
             (setq initargs (copy-list initargs))
             (cond ((set-equalp oldsupers newsupers)
                    (remf initargs :direct-superclasses))
                   (t (setf (getf initargs :direct-superclasses) newsupers))))
           ;; the existance of direct-superclasses in initargs means change of superclasses in after method.
           (cond ((null (getf initargs :direct-slots))
                  (remf initargs :direct-slots)
                  (apply #'call-next-method class slot-names initargs)) ; no need of maintenance for direct slots
                 (t (let ((old-args (make-initargs-from-slotds (mop:class-direct-slots class)))
                          (new-args (getf initargs :direct-slots)))
                      ;(format t "~%oldargs:~S" old-args)
                      ;(format t "~%newargs:~S" new-args)
                      (let ((mergedargs
                             (loop for oldarg in old-args
                                 collect (cond ((find (getf oldarg :name) new-args
                                                      :key #'(lambda (arg) (getf arg :name))))
                                               (t oldarg)))))
                        (cond ((null mergedargs)
                               (setq mergedargs new-args))
                              (t (setq mergedargs
                                       (append mergedargs
                                               (loop for newarg in new-args
                                                   unless (find (getf newarg :name) old-args
                                                                :key #'(lambda (arg) (getf arg :name)))
                                                   collect newarg)))))
                        ;(format t "~%mergedargs:~S" mergedargs)
                        (setf (getf initargs :direct-slots) mergedargs)
                        (apply #'call-next-method class slot-names initargs)))))))))

(defun make-initargs-from-slotds (slotds)
  (mapcar #'make-initarg-from-slotd slotds))
(defun make-initarg-from-slotd (slotd)
  (loop for facetd in (mop:class-slots (class-of slotd)) with name
      when (and (slot-boundp slotd (setq name (mop:slot-definition-name facetd)))
                (cond ((eq name 'excl::initform) (mop:slot-definition-initform slotd))
                      ((eq name 'excl::initfunction) (mop:slot-definition-initfunction slotd))
                      ((eq name 'excl::readers) (slot-value slotd name))
                      ((eq name 'excl::writers) (slot-value slotd name))
                      ((eq name 'common-lisp:type) (not (eq t (slot-value slotd name))))
                      ((eq name 'documentation) (slot-value slotd name))
                      ((eq name 'excl::fixed-index) nil)
                      (t t)))
      append (cond ((eq name 'excl::name) `(:name ,(mop:slot-definition-name slotd)))
                   ((eq name 'excl::initargs) `(:initargs ,(mop:slot-definition-initargs slotd)))
                   ((eq name 'excl::initform) `(:initform ,(mop:slot-definition-initform slotd)))
                   ((eq name 'excl::initfunction) `(:initfunction ,(mop:slot-definition-initfunction slotd)))
                   ((eq name 'excl::readers) `(:readers ,(slot-value slotd name)))
                   ((eq name 'excl::writers) `(:writers ,(slot-value slotd name)))
                   ((eq name 'common-lisp:type) `(:type ,(mop:slot-definition-type slotd)))
                   ((eq name 'excl::allocation) `(:allocation ,(excl::slotd-allocation slotd)))
                   ((eq name 'documentation) `(:documentation ,(slot-value slotd name)))
                   (t `(,(car (slot-value facetd 'excl::initargs)) ,(slot-value slotd name))))))

(defmethod make-this-supers ((class rdfs:Class) superclasses)
  "returns MSCs of <old-supers> and <new-supers>."
  (cond ((or (eq class rdfs:Resource) (eq class |rdfs:Resource|))
         (most-specific-concepts superclasses))
        ((null superclasses) |rdfs:Resource|)
        (t (most-specific-concepts superclasses))))

(defun collect-props-from-initargs (initargs)
  (loop for args on initargs by #'cddr
      as role = (car args)
      when (and (not (eq role 'rdf:about))
                (not (eq role 'rdf:ID))
                (not (eq role 'xml:lang))
                (not (keywordp role)))
      collect role))

(defun aggregate-args (initargs)
  (let* ((roles (collect-props-from-initargs initargs))
         (vals (loop for role in roles
                   collect (loop for (prop val) on initargs by #'cddr
                               when (eq role prop)
                               append (cond ((keywordp role) (list val))
                                            ((listp val) val)
                                            (t (list val)))))))
    (loop for role in roles
        for val in vals
        append (list role (cond ((keywordp role) (car val))
                                   ((and (property? role)
                                         (eq (get-range (symbol-value role)) rdf:List))
                                    val)
                                   ((null (cdr val)) (car val))
                                   (t val))))))

(defmethod shared-initialize :before ((class rdfs:Class) slot-names
                                      &key (direct-superclasses nil direct-superclasses-p))
  "checks C subclassof D and D subclassof C, that implies equality."
  (cond ((eq slot-names t)) ; nothing done
        ((null direct-superclasses-p)) ; nothing done
        ((null direct-superclasses))   ; nothing done
        ((set-eq direct-superclasses (mop:class-direct-subclasses class))
         (error 'cyclic-super/subclasses-error
           :format-control "~S and ~S should be rewrite as equivalent"
           :format-arguments `(,class ,direct-superclasses)))))

(defmethod shared-initialize :before ((instance rdfs:Resource) slot-names &rest initargs)
  ;(format t "~%SHARED-INITIALIZE:BEFORE(rdfs:Resource) ~S ~S ~S" instance slot-names initargs)
  (when initargs
    (shared-initialize-before-in-RDF instance slot-names initargs)
    (shared-initialize-before-in-OWL instance slot-names initargs)))
(defun shared-initialize-before-in-RDF (instance slot-names initargs)
  ;(format t "~%shared-initialize-before-in-RDF(~S ~S ~S)" instance slot-names initargs)
  (let ((class (class-of instance)))
    (cond ((eq slot-names t)  ; new definition
           (loop for (role filler) on initargs by #'cddr
               when (property? role)
               do (type-option-check-with-cardinality
                   instance
                   filler
                   (find role (mop:class-slots class) :key #'mop:slot-definition-name)
                   nil)))
          ((null slot-names)  ; reinitialize
           (loop for (role filler) on initargs by #'cddr
               when (property? role)
               do ;; filler = oldval + newval, computed at shared-initialize:around(rdfs:Resource).
                 (let ((oldval (and (slot-exists-p instance role)  ; when ordinal property
                                    (slot-boundp instance role)
                                    (slot-value instance role))))
                   (cond ((eq role 'owl:oneOf)
                          (when oldval
                            (let ((diff (set-difference filler oldval :test #'owl-same-p)))
                              (when diff
                                (warn "~S is added to ~S value ~S of ~S." diff role oldval instance)))))
                         (t (type-option-check-with-cardinality
                             instance
                             filler
                             (find role (mop:class-slots class) :key #'mop:slot-definition-name)
                             oldval)))))))))
(defun shared-initialize-before-in-OWL (instance slot-names initargs)
  (declare (ignore instance slot-names initargs))
  )

(defun type-option-check-with-cardinality (instance filler slotd oldval)
  "See also owlkernel module."
  (unless slotd ; when slot is an ordinal slot.
    (return-from type-option-check-with-cardinality nil))
  (let ((type (mop:slot-definition-type slotd))
        (name (mop:slot-definition-name slotd)))
    (typecase type
      (null nil)
      (cons (case (op type)
              (and (mapc #'(lambda (type)
                             (satisfy-filler instance name filler type oldval slotd))
                     (cdr type)))
              (or (error "Not Yet!"))
              (not (error "Cant happen!"))
              (otherwise ;; conjunction of type options
               (mapc #'(lambda (type)
                         (satisfy-filler instance name filler type oldval slotd))
                 type))))
      (symbol (case type
                ((t) nil)  ; nothing done in RDF, See OWL module
                (otherwise (cond ((object? type)
                                  (setf (slot-value slotd 'excl::type)
                                    (symbol-value type))
                                  (type-option-check-with-cardinality instance filler slotd oldval))
                                 (t (error "Cant happen!"))))))
      (rdfs:Class (cond ((consp filler)
                         (loop for fil in filler
                             unless (typep fil type)
                             do (unless (eq (class-of fil) (load-time-value (symbol-value '|rdfs:Resource|)))
                                  (warn "Range entail of ~S: change class of ~S to ~S." (name slotd) fil type))
                               (change-class fil type)))
                        ((typep filler type) nil)
                        (t (unless (eq (class-of filler) (load-time-value (symbol-value '|rdfs:Resource|)))
                             (warn "Range entail of ~S: change class of ~S to ~S." (name slotd) filler type))
                           (change-class filler type))))
      (otherwise (error "Cant happen!")))))

(defun satisfy-filler (x R y type oldval slotd)
  (declare (ignore x oldval slotd))
  (flet ((range-satisfy (filler)
                        (cond ((typep filler type) t) ; nothing done
                              ((eq (type-of filler) '|rdfs:Resource|)
                               (change-class filler type))
                              ((eq (type-of filler) 'rdfs:Resource)
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
         )
    (typecase type
      (null nil)
      (rdfs:Class (cond ((atom y) (range-satisfy y))
                        (t (mapc #'(lambda (fil) (range-satisfy fil)) y))))
      (t (cond ((atom y) (range-satisfy y))
               (t (mapc #'(lambda (fil) (range-satisfy fil)) y)))))))

(defmethod shared-initialize :after ((class rdfs:Class) slot-names &rest initargs)
  (declare (ignore slot-names))
  "checks C subclassof D and D subclassof C, that implies equality.
Checks the residual mclasses of all instances of <class>."
  (when (set-equalp (mop:class-direct-superclasses class)
                    (mop:class-direct-subclasses class))
    (error 'cyclic-super/subclasses-error
      :format-control "~S and ~S should be rewrite as equivalent"
      :format-arguments `(,class ,(mop:class-direct-superclasses class))))
  ;(format t "~%SharedInitialize:after(~S ~S ~S)" class slot-names initargs)
  (let ((supers (getf initargs :direct-superclasses)))
    (when supers             ; direct-superclasses are changed then propagate the effects
      (loop for sub in (mop:class-direct-subclasses class) ; there are some subclasses
          as sub-supers = (mop:class-direct-superclasses sub)
          do (let ((new-sub-supers (make-this-supers sub sub-supers)))
               (unless (set-equalp sub-supers new-sub-supers)
                 ;(format t "~%Propagated superclass change of ~S:~%  ~S -> ~S" sub sub-supers new-sub-supers)
                 (reinitialize-instance sub :direct-superclasses new-sub-supers))))))
  
  (let ((supers (mklist (getf initargs 'rdfs:subClassOf))))
    (when supers
      ;; there might be a shadow class that is unshadowable in subclasses
      (loop for sub in (mop:class-direct-subclasses class)
          do (update-instance-for-unshadowing sub class supers))))
  )

(defun check-shadowed-class-and-propagate-to-subs (class)
  (when (shadowed-class-p class)
    (error "DEBUG!")
    (let ((new-supers (most-specific-concepts-by-superclasses (mop:class-direct-superclasses class))))
      (format t "~%NewSupers:~S for ~S" new-supers class)
      (when (length=1 new-supers)
        (loop for ins in (class-direct-instances class)
            do (change-class ins (car new-supers))))))
  (loop for sub in (mop:class-direct-subclasses class)
      do (check-shadowed-class-and-propagate-to-subs sub)))

(defun update-instance-for-unshadowing (shadow? class supers)
  "updates mclasses of all instances of class, if mclasses includes shadowed-classes that is unshadowable."
  (when (shadowed-class-p shadow?)
    ;; after reinitialize-instance the cpl is adjusted coherently.
    ;; then you cannot use most-specific-concepts routine
    ;(format t "~%Supers:~S~%ShadowsSupers:~S" supers (mop:class-direct-superclasses shadow?))
    (let* ((shadows-supers (mop:class-direct-superclasses shadow?))
           (revised-supers (set-difference shadows-supers supers)))
      (when (length=1 revised-supers)
        (assert (eq class (car revised-supers)))
        ;; change class of instances to class
        (loop for ins in (class-direct-instances shadow?)
            do ;(format t "~%Changing shadows instance ~S to ~S for unshadowing" ins class)
              (change-class ins class) ; upgrade instance
              ;; then is it possible to propagate unshadowing?
              (loop for sub in (mop:class-direct-subclasses shadow?)
                  do (update-instance-for-unshadowing sub shadow? revised-supers)))))))

#|
;; propagate the change of superclasses of this class to subclasses
(defmethod shared-initialize :after ((class rdfs:Class) slot-names &rest initargs)
  (assert (not (eq (car (mop:class-direct-superclasses class)) class)))
  (cond ((and (null slot-names) (null initargs))  ; when metaclass changed
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        (t                                        ; first or redefinition
         (unless (datatype-p class)
           (loop for sub in (mop:class-direct-subclasses class)
               unless (datatype-p sub)
                 ;; update the sub's supers
               do (let* ((oldsupers (mop:class-direct-superclasses sub))
                         (newsupers (most-specific-concepts-by-superclasses oldsupers)))
                    (unless (set-equalp newsupers oldsupers)
                      (reinitialize-instance sub :direct-superclasses newsupers))))))))
|#
(defmethod shared-initialize :after ((class rdfs:Datatype) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when change-class
         )
        ((and (consp slot-names) (null initargs)) ; when metaclass redefined, propagated
         )
        ((eq slot-names t)   ;; first definition
         (let* ((datatype (name class))
                (fname `(excl::deftype-expander ,datatype)))
           (cond ((fboundp fname) nil)        ; lisp type defined
                 ((fboundp datatype)          ; if type name has function
                  (symbol-function datatype)
                  `(deftype ,datatype (satisfies ,datatype)))
                 (t (warn "Datatype ~S is defined. Please define lisp type with same name."
                      (name class))))))
        (t ;; redefinition
         (case (name class)
           (rdf:XMLLiteral nil)
           (otherwise
            (let* ((datatype (name class))
                   (fname `(excl::deftype-expander ,datatype)))
              (cond ((fboundp fname) nil)        ; lisp type defined
                    ((fboundp datatype)          ; if type name has function
                     (symbol-function datatype)
                     `(deftype ,datatype (satisfies ,datatype)))
                    (t (warn "Datatype ~S is defined. Please define lisp type with same name."
                         (name class))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;

;;
;; Change Class
;;
;; SWCLOS allows ones to change predefined CLOS objects to RDF objects.
;; This is very powerful to combine usual CLOS functionalities and RDF semantics.
;; CLOS slots are often used for bookkeeping or special functions in domain application.
;; RDF properties obey RDF semantics and are used for ontology modeling.
;; If an instance is an usual CLOS class, single classing is ruled out.
;; If an instance is RDF object, multiple classing is allowed.

(defmethod change-class :around ((instance rdfs:Resource) (new-class cons) &rest initargs)
  (let ((classes (cond ((eq (car new-class) 'and) (cdr new-class))
                       (t new-class))))
    (cond ((length=1 classes)
           (apply #'change-class instance (car classes) initargs))
          (t (setq classes (most-specific-concepts-by-superclasses classes))
             (cond ((length=1 classes)
                    (apply #'change-class instance (car classes) initargs))
                   (t (let ((shadow (make-shadow (class-of instance) classes)))
                        (cond ((eql (class-of instance) shadow) instance)
                              (t (warn "Multiple classing with ~S for ~S" classes instance)
                                 (apply #'change-class instance shadow initargs))))))))))

;(defmethod change-class :before ((instance rdfs:Class) (new-class rdfs:Class)  &rest initargs)
;  (declare (ignore initargs))
;  (unless (mop:class-finalized-p instance) (mop:finalize-inheritance instance)))

(defmethod change-class :before ((instance rdfs:Resource) (new-class standard-class)
                                 &rest initargs)
  (declare (ignore initargs))
  (unless (shadowed-class-p instance)
    (unless (rdf-class-p new-class)
      (error "You cannot role down an RDF class to a CLOS class."))))

(defmethod change-class :before ((instance rdfs:Resource) (new-class rdfs:Class) &rest initargs)
  "If <instance> has a slot value and <new-class> has no slot definitions on it,
   then add the slot definitions into <new-class>."
  ;(format t "~%CHANGE-CLASS:before(~S ~S ~S)" instance new-class initargs)
  (unless (and (cl:subtypep (class-of instance) new-class) (null initargs))
    (unless (mop:class-finalized-p new-class) (mop:finalize-inheritance new-class))
    (let* ((new-class-slotds (mop:class-slots new-class))
           (old-class (class-of instance))
           (old-class-slotds (mop:class-slots old-class))
           (added (loop for slotd in old-class-slotds with role
                      when (and (property-effective-slotd-p slotd)
                                (slot-boundp instance (setq role (mop:slot-definition-name slotd)))
                                (slot-value instance role)
                                (not (find role new-class-slotds :key #'mop:slot-definition-name :test #'eq)))
                      collect (make-initarg-from-slotd slotd))))
      (when added
        (reinitialize-instance new-class :direct-slots added)))))

(defmethod change-class ((instance rdfs:Resource) (new-class rdfs:Class) &rest initargs)
  (let ((old-class (class-of instance)))
    (when (eq instance rdfs:Class) (error "Bingo, Check it!"))
    ;(when (eq old-class new-class) (return-from change-class instance))
    ;(format t "~%Changing class of ~S to ~S~%   with ~S" instance new-class initargs)
    (cond ((and (shadowed-class-p old-class) (not (shadowed-class-p new-class)))
           ;(format t "~%Changing shadowed class of ~S to ~S~%   with initargs:~S" instance new-class initargs)
           (call-next-method))
          ((cl:subtypep old-class new-class)
           (error "Cant happen!")
           (apply #'call-next-method instance old-class initargs))
          ((and (cl:typep old-class 'shadowed-class) (cl:typep new-class 'shadowed-class))
           ;(format t "~&Changing the shadow class of ~S to another shadow class ~S with initargs ~S" instance new-class initargs)
           (let ((old-supers (mop:class-direct-superclasses old-class))
                 (new-supers (mop:class-direct-superclasses new-class)))
             (cond ((subsetp old-supers new-supers)
                    (apply #'call-next-method instance new-class initargs))
                   ((subsetp new-supers old-supers)
                    (apply #'call-next-method instance new-class old-class))
                   (t (let ((shadow (make-shadow old-class (append new-supers old-supers))))
                        (apply #'call-next-method instance shadow initargs))))))
          ((cl:typep old-class 'shadowed-class)
           (let ((shadow (make-shadow old-class (cons new-class (mclasses instance)))))
             (apply #'call-next-method instance shadow initargs)))
          ((cl:typep new-class 'shadowed-class)
           (let ((shadow (make-shadow new-class (append (mop:class-direct-superclasses new-class) (mclasses instance)))))
             (apply #'call-next-method instance shadow initargs)))
          ;((cl:typep new-class 'shadowed-class)
          ; (warn "Attempting to refine ~S to ~S" instance new-class) ; by smh
          ; (call-next-method)  ; by smh
          ; )
          ((cl:typep instance 'shadowed-class)
           (warn "Attempting to refine ~S to ~S" instance new-class) ; by smh
           (call-next-method)  ; by smh
           )
          ((cl:subtypep new-class old-class) (call-next-method))
          ((and (eql (class-of instance) |rdfs:Resource|)
                (cl:subtypep new-class rdfs:Resource))
           (call-next-method))
          ((and (rdf-metaclass-p new-class) (rdf-instance-p instance))
           (cond ((eq (class-of instance) (find-class '|rdfs:Resource|))
                  (call-next-method))
                 (t (error "Not Yet!"))))
          (t (warn "~S is additionally classified to ~S." instance new-class)
             ;; note that shadow may not be a shadow class.
             (let ((shadow (make-shadow old-class (list new-class old-class) )))
               (apply #'call-next-method instance shadow initargs))))
    ))

(defmethod change-class :after ((instance rdfs:Resource) (new-class rdfs:Class) &rest initargs)
  (declare (ignore initargs))
  ;(format t "~%Change class :after (~S rdfs:Resource) (~S rdfs:Class)" instance new-class)
  (let ((old-class (class-of instance)))
    ;(format t "~%   old-class:~S direct-instances:~S" old-class (class-direct-instances old-class))
    (when (null (class-direct-instances old-class))
      (loop for slotd in (mop:class-direct-slots old-class)
          do (let ((name (mop:slot-definition-name slotd)))
               (let ((eslotd (find name (mop:class-slots old-class)
                                   :key #'mop:slot-definition-name)))
                 (when (and (cl:typep eslotd 'Property-effective-slot-definition)
                            (slot-boundp eslotd 'subject-type)
                            (eq old-class (slot-definition-subject-type eslotd)))
                   (cond ((eq old-class |rdfs:Resource|)
                          (error "Bingo!")
                          (setf (mop:class-direct-slots old-class)
                            (remove slotd (mop:class-direct-slots old-class)))
                          (let* ((prop (symbol-value name))
                                 (slotd (find old-class (slot-value prop 'slotds)
                                              :key #'slot-definition-subject-type)))
                            (setf (slot-value prop 'slotds)
                              (remove slotd (slot-value prop 'slotds)))))))))))))

;;
;;
;;

;; otherwise suppress the invocation of update-instance-for-different-class (standard-class)
;; I guess this is needed for Allegro CLOS bug that change-class before invokes update-instance-for-different-class

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; slot-value
;;
;; The semantics on slot value is very different between CLOS and RDF.
;; Slots are only added, must not deleted.
;; Symbol value is changed to the resource.
;; Range checking is carried out.
(defun compute-slot-value (value oldval slotd instance)
  (declare (optimize (speed 3) (safety 0)))
  (let ((prop (mop:slot-definition-name slotd)))
    (flet ((ignore-slot-value
            ()
            (warn "Ignored:~S for ~S in ~S." value prop instance)
            oldval)
           (slot-value-with-cardinality-check
            ()
            (cond ((cardinality-ok-p value slotd) value)
                  (t (warn "Ignored:~S ~S ~S." instance prop value)
                     oldval))))
      (when value
        (setq value (slot-value-range-check prop value (mop:slot-definition-type slotd)))
        (assert value))
      (cond ((null oldval) (slot-value-with-cardinality-check)) ; value
            ((equal value oldval) oldval) ; no message and return oldval
            ((equalp value oldval)        ; nothing done but message it
             (ignore-slot-value))         ; return oldval
            ;; the same is added into slot
            ((and (atom value) (atom oldval) ; not equal
                  (progn
                    (cond ((and (rsc-object-p value) (rsc-object-p oldval))
                           (setq value (mkatom (most-specific-concepts (list value oldval)))))
                          (t (setq value (list value oldval))))
                    (slot-value-with-cardinality-check))))
            ((and (atom value) ; oldval cons
                  (progn
                    (cond ((rsc-object-p value)
                           (setq value (mkatom (most-specific-concepts (cons value oldval)))))
                          ((member value oldval :test #'equalp)
                           (setq value (ignore-slot-value)))
                          (t (setq value (cons value oldval))))
                    (slot-value-with-cardinality-check))))
            ((and (atom oldval) ; value cons
                  (progn
                    (cond ((rsc-object-p oldval)
                           (setq value (mkatom (most-specific-concepts (append value (list oldval))))))
                          ((member oldval value :test #'equalp)) ; value
                          (t (setq value (append value (list oldval)))))
                    (slot-value-with-cardinality-check))))
            ((and (setq value (mkatom (most-specific-concepts (append value oldval))))
                  (slot-value-with-cardinality-check)))
            ;; violate cardinality constraint
            (t (error "Unification among ~S required, but NOT YET!")))
      )))
#|
;; This method is needed for method inheritance.
(defmethod (setf mop:slot-value-using-class)
    (value (class rdfs:Class) (object rdfs:Resource) slotd)
  (declare (ignore value slotd))
  ;(format t "~%Setf Slot-value-using-class4 with ~S ~S ~S" value object slotd)
  (call-next-method))

(defmethod (setf mop:slot-value-using-class)
    (value (class rdfs:Class) (object rdfs:Resource) 
           (slotd gx::Property-effective-slot-definition))
  (declare (optimize (speed 3) (safety 0)))
  ;(format t "~%Setf Slot-value-using-class in IR with ~S ~S ~S" value object slotd)
  (let ((slot-name (mop:slot-definition-name slotd))
        (filler nil))
    (flet ((ignore-slot-value ()
                              (warn "Ignored:~S ~S ~S." object slot-name value)
                              (slot-value object slot-name)))
      (flet ((slot-value-with-cardinality-check ()
                                                (cond ((cardinality-ok-p value slotd)
                                                       (call-next-method value class object slotd))
                                                      (t (warn "Ignored:~S ~S ~S." object slot-name value)
                                                         (slot-value object slot-name)))))
        (when value
          (setq value (slot-value-range-check slot-name value (mop:slot-definition-type slotd)))
          (assert value))
        (cond ((not (slot-boundp object slot-name))
               (slot-value-with-cardinality-check))
              ((null (setq filler (slot-value object slot-name)))
               (slot-value-with-cardinality-check))
              ((%owl-same-p value filler) ; nothing done
               (ignore-slot-value))
              ((and (atom value) (atom filler))
               (setq value (list value filler))
               (slot-value-with-cardinality-check))
              ((atom value)
               (cond ((not (member value filler :test #'%owl-same-p))
                      (setq value (cons value filler))
                      (slot-value-with-cardinality-check))
                     ((eq slot-name 'rdf:type)) ; ignore but no message
                     (t (ignore-slot-value))))
              ((atom filler)
               (cond ((not (member filler value :test #'%owl-same-p))
                      (setq value (cons filler value))
                      (slot-value-with-cardinality-check))
                     ((eq slot-name 'rdf:type)) ; ignore but no message                     
                     (t (ignore-slot-value))))
              (t (let ((union (union value filler :test #'%owl-same-p)))
                   (cond ((every #'rdf-instance-p union)
                          (setq value (remove-duplicates union :test #'%owl-same-p)))
                         ((every #'rdf-class-p union)
                          (setq value (most-specific-concepts union)))
                         ((error "Cant happen! Debug!"))))
                 (slot-value-with-cardinality-check)))))))
|#
(defmethod cardinality-ok-p (value slotd)
  (declare (ignore value slotd))
  t)

;;
;;
;;

(defmethod collect-all-instances-of ((class symbol)) ;smh
  (collect-all-instances-of (symbol-value class)))

(defmethod collect-all-instances-of ((class rdfs:Class)) ;smh
  (declare (optimize (speed 3) (safety 0)))
  (remove-duplicates
   (append (class-direct-instances class)
           (loop for sub in (mop:class-direct-subclasses class)
               append (cond ((eq sub (find-class 'rdfsClass)) nil)
                            (t (collect-all-instances-of sub)))))))
#|
(defmethod collect-all-instances-of ((class metaRDFSclass)) ; when class is rdfsClass
  nil)
|#
(defun delete-slot (class slot-name)
  (setf (slot-value class 'prototype) nil)
  (let ((direct-slots (mop:class-direct-slots class)))
    (when (find slot-name direct-slots :key #'mop:slot-definition-name)
      (setf (mop:class-direct-slots class)
        (delete slot-name direct-slots :key #'mop:slot-definition-name))

      (excl::fix-slot-accessors class direct-slots 'add)))
  )

;; End of module
;; --------------------------------------------------------------------
