;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Domain and Range module
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
;;
;;; ==================================================================================

(cl:provide :domainrange)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  (require :namespace)
  (require :rdfboot)
) ; end of eval-when

(in-package :gx)

(export '(domain-value get-domain collect-domains domainp rangep range-value get-range collect-ranges
           *autoepistemic-local-closed-world*))

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
(defvar *autoepistemic-local-closed-world*)
;;;
;;;; Unsatisfiable Error
;;;

(define-condition rdf-unsatisfiable (error)
  ()
  )

(define-condition domain-condition-unsatisfiable (rdf-unsatisfiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "domain condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition range-condition-unsatisfiable (rdf-unsatisfiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "range condition unsatisfiable: ~A" 
         (apply #'format nil fcont args)))))
  )

(define-condition invalid-slot-value-for-range (rdf-unsatisfiable)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "invalid slot value for range: ~A" 
         (apply #'format nil fcont args)))))
  )

;;; ==================================================================================
;;;
;;;; Top Class rdfs:Resource and Top Metaclass rdfs:Class
;;;
;;; In the RDF universe, the top class, that is a superclass of every class, is rdfs:Resource.
;;; The top metaclass, that is a class of every class, is rdfs:Class.
;;; Note that the direct class of rdfs:Class is (virtually) rdfs:Class (actually rdfsClass in lisp), 
;;; but the superclass of rdfs:Resource is (virtually) null (actually gx::gnode in lisp).
;;; In our OWL connection to RDF, the OWL universe is included into and inherites characteristics from 
;;; the RDF universe. The top concept is rdfs:Resource and the top meta concept is rdfs:Class in 
;;; the OWL universe, too. 

(defparameter *top* rdfs:Resource
  "The top concept, i.e. rdfs:Resource in RDFS, and in OWL, too.")

(defparameter *meta* rdfs:Class
  "The top meta-object, i.e. rdfs:Class in RDFS, and in OWL, too.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;; Domain Value
;;;

(defmethod domain-value ((property rdf:Property))
  "retrieve the domain value of <property>, or returns nil if not exists.
   Note that this method is refined for owl:ObjectProperty."
  (and (slot-boundp property 'rdfs:domain) (slot-value property 'rdfs:domain)))

(defun %domain-value (property)
  "returns a list of domains for property or null list."
  (let ((domain (domain-value property)))
    (when domain
      (cond ((atom domain) (list domain))           ; '(x)
            ((eq (car domain) 'and) (cdr domain))      ; '(x ... )
            (t domain)))))                             ; '(x ... ) or '(or ...)

(defun %get-domain (property)
  (let ((dmn (%domain-value property))
        (abs (most-specific-inherited-domain property)))
    ;; disjoint check
    (some #'(lambda (x)
              (some #'(lambda (y)
                        (when (and (not (eq x y)) (disjoint-p x y))
                          (error 'domain-condition-unsatisfiable
                            :format-control "disjoint clash: ~S ~S"
                            :format-arguments (list x y))))
                    (append dmn abs))) dmn)
    (most-specific-concepts (append dmn abs))))

(defun most-specific-inherited-domain (property)
  (let ((domain nil)
        (supers (and (slot-boundp property 'rdfs:subPropertyOf)
                     (slot-value property 'rdfs:subPropertyOf))))
    (loop for super in (if (listp supers) supers (list supers))
        do (setq domain (more-specific-domain property domain super)))
    domain))

(defun get-domain (property)
  "returns domain of <property> or nil. This function searches the super-properties and 
   returns the most specific domain."
  (mkatom (%get-domain property)))

(defun more-specific-domain (property domain super)
  (declare (ignore property))
  (let ((abst-domain
         (append (%domain-value super)
                 (most-specific-inherited-domain super))))
    (flet ((clash-p () (some #'(lambda (x)
                                 (some #'(lambda (y)
                                           (when (and (not (eq x y)) (disjoint-p x y))
                                             (error 'domain-condition-unsatisfiable
                                               :format-control "disjoint clash: ~S ~S"
                                               :format-arguments (list x y))))
                                       abst-domain)) domain)))
      (clash-p)
      (most-specific-concepts (append domain abst-domain)))))

(defun collect-domains (properties)
  "collects domain information from <properties>. A property must be a symbol.
   If anyone in properties is not defined, this function executes rdf1 entaiment rule."
  (loop for role in properties with domains
      when (and (not (eq role 'rdf:about))
                (not (eq role 'rdf:ID))
                (not (eq role 'xml:lang))
                (not (keywordp role)))
      do (when (not (property? role))
           (warn "Entail by rdf1: ~S rdf:type rdf:Property." role)
           (set role (make-instance rdf:Property :name role)))
        (setq domains (union (%get-domain (symbol-value role)) domains))
      finally (return domains)))

(defun domainp (property domain)
  "returns true if <property>'s domain is a subclass of <domain>, or if 
   some of <property>'s super-properties has a subclass domain of <domain>."
  (or (and (slot-boundp property 'rdfs:domain)
           (let ((cls (slot-value property 'rdfs:domain)))
             (cond ((listp cls) (some #'(lambda (d) (subtypep d domain)) cls))
                   ((subtypep cls domain)))))
      (some #'(lambda (superp) (domainp superp domain))
            (slot-value property 'rdfs:subPropertyOf))))

;;;
;;;;  Range Value
;;;

(defmethod range-value ((property rdf:Property))
  "retrieve the range value of <property>, or returns nil if not exists.
   This method is refined for owl:ObjectProperty."
  (and (slot-boundp property 'rdfs:range) (slot-value property 'rdfs:range)))

(defun %range-value (property)
  "returns a list of ranges for property or null list."
  (let ((range (range-value property)))
    (when range
      (cond ((atom range) (list range))
            ((eq (car range) 'and) (cdr range))
            (t range)))))

(defun most-specific-inherited-range (property)
  (let ((range nil)
        (supers (and (slot-boundp property 'rdfs:subPropertyOf)
                     (slot-value property 'rdfs:subPropertyOf))))
    (unless (listp supers) (setq supers (list supers)))
    (loop for super in supers
        do (setq range (more-specific-range property range super)))
    range))

(defun get-range (property)
  "returns range of <property> or nil. This function searches the super-properties and 
   returns the most specific range."
  (mkatom (%get-range property)))

(defun %get-range (property)
  (let ((rng (%range-value property))
        (abs (most-specific-inherited-range property)))
    (some #'(lambda (x)
              (some #'(lambda (y)
                        (when (and (not (eq x y)) (disjoint-p x y))
                          (error 'range-condition-unsatisfiable
                            :format-control "disjoint clash: ~S ~S"
                            :format-arguments (list x y))))
                    (append rng abs))) rng)
    (most-specific-concepts (append rng abs))))

(defun more-specific-range (property range super)
  (declare (ignore property))
  (let ((abst-range
         (append (%range-value super)
                 (most-specific-inherited-range super))))
    (flet ((clash-p () (some #'(lambda (x)
                                 (some #'(lambda (y)
                                           (when (and (not (eq x y)) (disjoint-p x y))
                                             (error 'range-condition-unsatisfiable
                                               :format-control "disjoint clash: ~S ~S"
                                               :format-arguments (list x y))))
                                       abst-range)) range)))
      (clash-p)
      (most-specific-concepts (append range abst-range)))))

(defun collect-ranges (properties)
  "collects range information from <properties>. A property must be a symbol."
  (loop for role in properties with ranges
      when (and (not (eq role 'rdf:about))
                (not (eq role 'rdf:ID))
                (not (eq role 'xml:lang))
                (not (keywordp role)))
      do (when (not (property? role))
           (warn "Entail by rdf1: ~S rdf:type rdf:Property." role)
           (set role (make-instance rdf:Property :name role)))
        (setq ranges (union (%get-range (symbol-value role)) ranges))
      finally (return ranges)))

(defun rangep (property range)
  "returns true if <property>'s range is a subclass of <range>, or if 
   some of <property>'s super-properties has a subclass range of <range>."
  (or (and (slot-boundp property 'rdfs:range)
           (let ((cls (slot-value property 'rdfs:range)))
             (cond ((consp cls) (some #'(lambda (r) (subtypep r range)) cls))
                   ((subtypep cls range)))))
      (let ((supers (slot-value property 'rdfs:subPropertyOf)))
        (unless (listp supers) (setq supers (list supers)))
        (some #'(lambda (super) (rangep super range)) supers))))

;;
;; Domain and Range Check
;;

(defun domains-satisfied-p (instance domains)
  "Is every domain satisfied?"
  (every #'(lambda (d)
             (or (cl:typep instance d)
                 (and (eql d rdfs:Class) (cl:subtypep instance rdfs:Resource))))
         domains))

(defun domain-check-for-class (class domain)
  "checks <class> for <domain> constraint, if <class> violates the constraint,
   entailment is invoked as much as possible. This function returns either class or domain."
  (setq domain (mkatom domain))
  (when (consp domain)
    (case (car domain)
      ((forall exists fills) nil)
      ((and or not) nil)
      (otherwise (setq domain (cons 'and domain)))))
  (cond ((null domain) class)
        ((cl:subtypep class domain) class)
        ((and (eql domain rdfs:Class) (cl:subtypep class rdfs:Class)) class)
        (t (warn "Domain entail:class ~S to ~S." (class-name class) domain)
           domain)))

(defun domain-check-for-instance (instance domain)
  "checks <instance> for <domain> constraint, if <instance> violates the constraint,
   entailment is invoked as much as possible."
  (cond ((null domain) instance)
        ((cl:typep instance domain) instance)
        ((and (eql domain rdfs:Class) (cl:subtypep instance rdfs:Resource)) instance)
        ((rsc-object-p instance)
         (cond ((rdf-class-p domain)
                (warn "Domain entail4:change class of ~S to ~S." instance domain)
                (cond ((eq (class-of instance) (class-of domain))
                       (change-class instance domain))
                      (t (change-class instance domain)
                         (apply #'reinitialize-instance instance ()))))
               ((and (symbolp domain) (class? domain))
                (warn "Domain entail5:change class of ~S to ~S." instance domain)
                (cond ((eq (class-of instance) (class-of domain))
                       (change-class instance (symbol-value domain)))
                      (t (change-class instance (symbol-value domain))
                         (apply #'reinitialize-instance (symbol-value domain) ()))))
               (t (error "Cant happen!"))))
        (t (error "Cant happen!"))))

(defun slot-value-range-check (role value range)
  "checks <value> for <range> constraint, if <value> violates the constraint,
   entailment is invoked as much as possible. Otherwise error caused.
   This function returns <value>. 
   NOTE. this function does not infer on oneOf information. This is important
   for establishing oneOf entailment."
  (when (eq range t)
    (return-from slot-value-range-check
      (if (and (property? role) (setq range (get-range (symbol-value role))))
          (slot-value-range-check role value range)
        value)))
  (typecase range
    (null (cond ((and (symbolp value) (object? value)) (symbol-value value))
                ((consp value)
                 (cond ((lang? (car value))
                        (make-instance 'rdf:inLang
                          :lang (intern (car value) "keyword")
                          :content (cadr value)))
                       (t (loop for fil in value collect (slot-value-range-check role fil range)))))
                ((and value (symbolp value)
                      (not (keywordp value))
                      (not (eq value t)))
                 ;(warn "Entail by rdfs4b: ~S rdf:type rdfs:Resource." value)
                 (make-instance '|rdfs:Resource| :name value))
                (t value)))
    (symbol (if (class? range) (slot-value-range-check role value (symbol-value range))
               (error "Not Yet!")))
    (cons (case (op range)
            (and (loop for r in (args range) with val = value
                     unless (setq val (slot-value-range-check role val r))
                     do (return-from slot-value-range-check nil)
                     finally (return val)))
            (or (loop for r in (args range) with val
                    when (setq val (slot-value-range-check role value r))
                    do (return-from slot-value-range-check val)))
            (not (if (slot-value-range-check role value (args range)) nil value))
            (otherwise (slot-value-range-check role value (cons 'and range)))))
    (forall (cond ((consp value)
                   (cond ((subtypep rdf:List range) value)
                         (t (remove nil
                                    (loop for val in value collect (slot-value-range-check role val range))))))
                  ((and (equivalent-property-p role (forall-role range))
                        (typep value (forall-filler range)))
                   value)
                  ((subtypep (forall-filler range) (class-of value))
                   (warn "ForAll constraint on ~S in ~S entailment: ~S rdf:type ~S."
                     role (slot-subject-type range) (forall-filler range))
                   (change-class value (forall-filler range)))
                  (t (error 'invalid-slot-value-for-range 
                       :format-control "~S for range ~S"
                       :format-arguments (list value range)))))
    (exists (cond ((consp value)
                   (remove nil
                           (loop for val in value collect (slot-value-range-check role val range))))
                  ((and (equivalent-property-p role (exists-role range))
                        (typep value (exists-filler range)))
                   value)
                  (*autoepistemic-local-closed-world*
                   (error 'invalid-slot-value-for-range 
                     :format-control "~S for range ~S"
                     :format-arguments (list value range)))
                  (t value))) ; others may satisfy, so passes it.
    (fills (cond ((consp value)
                (remove nil
                        (loop for val in value collect (slot-value-range-check role val range))))
               ((and (equivalent-property-p role (fills-role range))
                     (owl-same-p value (fills-filler range)))
                value)
               (*autoepistemic-local-closed-world*
                (error 'invalid-slot-value-for-range 
                  :format-control "~S for range ~S"
                  :format-arguments (list value range)))
               (t value)))
    (otherwise (cond ((consp value)
                      (cond ((subtypep rdf:List range) value)
                            (t (remove nil
                                       (loop for val in value collect (slot-value-range-check role val range))))))
                     (t (%slot-value-range-check role value range))))))
(defun %slot-value-range-check (role value range)
  "range is an atom."
  (when (eq value t)
    (return-from %slot-value-range-check
      (cond ((cl:subtypep range 'xsd:|boolean|) value)
            ((and (symbolp range) (eq range 'xsd:|boolean|)) value)
            ((error "Cant happen:t for range ~S" range)))))
  (etypecase value
    (null (cond ((cl:subtypep range 'xsd:|boolean|) value)
                ((and (symbolp range) (eq range 'xsd:|boolean|)) value)
                (t value))) ; pass null
    (cons (cond ((subtypep rdf:List range) value)
                ((lang? (car value))
                 (setq value
                       (make-instance 'rdf:inLang
                         :lang (intern (car value) "keyword")
                         :content (cadr value)))
                 (assert (and range
                              (or (eql range 'xsd:|string|)
                                  (cl:subtypep rdfs:Literal range))))
                 value)
                (t (remove nil (loop for val in value collect (%slot-value-range-check role val range))))))
    (symbol ; maybe keyword
     (error "Not Yet!")
     (cond ((not (keywordp value))
            (cond ((rdf-class-p range)
                   (warn "Entail by range: ~S rdf:type ~S." value (class-name range))
                   (cond ((rdf-metaclass-p range)
                          (apply #'mop:ensure-class-using-class (find-class value nil) value
                                 :metaclass range
                                 ()))
                         (t (make-instance range :name value))))
                  ((and (symbol-value range) (class? range))
                   (warn "Entail by range: ~S rdf:type ~S." value range)
                   (cond ((rdf-metaclass-p range)
                          (apply #'mop:ensure-class-using-class (find-class value nil) value
                                 :metaclass (symbol-value range)
                                 ()))
                         (t (make-instance (symbol-value range) :name value))))
                  ((error "Cant happen!" 1 value range))))  ; by smh
           (t value)))
    (number (cond ((typep value range) value)
                  ((cl:subtypep range 'xsd:|decimal|) value)
                  (t (error "Not Yet!"))))
    (string (cond ((typep value range) value)
                  ((cl:subtypep range 'xsd:|decimal|)
                   (read-from-string value))
                  (t (error "Not Yet!"))))
    (rdf:inLang (cond ((typep value range) value) 
                      (t (error "Not Yet!"))))
    (uri (cond ((typep value range) value)               ; OK
                       ((cl:subtypep range rdfs:Resource) value) ; range is owl:Ontology
                       ((typep (iri-value value) range) value)  ; check 
                       ((warn "*** INVALID SLOT VALUE1 ~S FOR ~S ***" value range)
                        value)))
    (rdfs:Resource
     (cond ((rdf-metaclass-p range)
            (cond ((typep value range) value)
                  ((error "Bingo!!!"))
                  ((rdf-metaclass-p value) ; higher metaclassing
                   (cond ((%clos-subtype-p value *top*) value)
                         (t (error "Not Yet for higher-level metaclassing"))))
                  ((rdf-class-p value)     ; value in class layer
                   (warn "Range entail41:change class of ~S to ~S." value range)
                   (change-class value range))
                  (t                       ; value in instance layer
                   (warn "Range entail42:change class of ~S to ~S." value range)
                   (change-class value range))))
           ((rdf-class-p range)
            (cond ((typep value range) value)
                  (t (warn "Range or onProperty entail:change class of ~S to ~S." value range)
                     (when (class-name range) ; not anonymous
                       (cond ((eq (class-of value) (class-of range))
                              (error "Cant Happen!")
                              (change-class value range))
                             (t (change-class value range)
                                (apply #'reinitialize-instance value ()))))
                     value)))
           ((symbolp range) (error "Cant happen!"))
           ((and (consp range) (eq (car range) 'or) (cadr range) (null (cddr range)))
            (error "Not Yet!")
            (cond ((typep value range) value)             ; OK
                  (t
                   ;; smh added this clause to avoid error on a singleton or range, 
                   ;; Still don't handle an or type with more than a single subform. -smh
                   (slot-value-range-check role value (cadr range))) ; by smh
                  ))))
    (xsd:|anySimpleType|
     (format t "~%YYYYES!")
     (cond ((typep value range) value)              ; OK
           ((eq range 'rdfs:Literal) value)
           ((or (cl:subtypep range 'xsd:|decimal|)
                (cl:subtypep range 'xsd:|float|)
                (cl:subtypep range 'xsd:|double|))
            (cond ((and (numberp value) (cl:typep value range))
                   value)
                  ((stringp value)
                   (setq value (read-from-string value))
                   (cond ((cl:typep value range) value)
                         ((error 'invalid-slot-value-for-range
                            :format-control "~S for range ~S"
                            :format-arguments (list value range)))))))
           ((cl:subtypep range 'xsd:|string|) value)
           (t (warn "*** INVALID SLOT VALUE2 ~S FOR ~S ***" value range) value)))
    ))

;; End of module
;; --------------------------------------------------------------------
