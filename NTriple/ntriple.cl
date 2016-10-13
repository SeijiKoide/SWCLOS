;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; N-triple module
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; Copyright (c) 2003 by Galaxy Express Corporation
;;;
;; History
;; -------
;; 2009.09.04    name RDFSclass is changed to _rdfsClass.
;; 2008.12.11    resource-p is renamed to rdf-objectp
;; 2003.08.3    File created
;;
;;; ==================================================================================

(cl:provide :ntriple)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfscore)
  )

(in-package :gx)

(export '(defTriple addTriple /. ./ get-triple
           superclasses-of subclasses-of))

;;;
;;;; defTriple
;;;

(defmacro defTriple (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))

(defmacro /. (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))
(defmacro ./ (subject predicate object)
  "defines a triple with forward-reference functionality."
  `(addTriple ',subject ',predicate ',object))

;;;
;;;; Add Triple
;;;
;;; addTriple shows very convenient usage of method. 
;;; A triple of subject/predicate/object in various types, which may be undefined in forward reference, 
;;; are accepted and they are set up step by step using appropriate entailment rules piecewisely.
;;; 
;;; ----------------------------------------------------------------------------------
;;;        +-- t/URI/t
;;;        +-- t/sym/t
;;;        +-- URI/t/t
;;;        |                                   +-- sym/rdf:type/sym
;;;        |                                   +-- sym/rdf:type/cls
;;;        +-- sym/rsc/t                       +-- sym/rdfs:subPropertyOf/sym
;;; t/t/t -+-- sym/prop/t --+-- sym/prop/sym --+-- sym/rdfs:subPropertyOf/prop
;;;        |                +-- sym/prop/data  +-- sym/rdfs:subClassOf/sym
;;;        |                                   +-- sym/rdfs:subClassOf/cls
;;;        |
;;;        +-- rsc/prop/t --+-- rsc/prop/sym
;;;        |                +-- cls/prop/t ---+-- cls/prop/sym 
;;;        |                                  +--------------------- cls/rdfs:subClassOf/cls
;;;        +-- rsc/rsc/t
;;;
;;; ----------------------------------------------------------------------------------

(defgeneric addTriple (subject predicate object)
  (:documentation
   "adds a subject-predicate-object triple.")
  )

;;;
;;;; <t> rdf:type <t>
;;;

(defmethod addTriple (subject (predicate (eql rdf:|type|)) object)
  (error "Triple input error: ~S ~S ~S." subject predicate object))

;;;
;;;; <Class> rdf:type <Class> --> <Class> rdf:type <Class>
;;;

(defmethod addTriple ((subject rdfs:Class) (predicate (eql rdf:|type|)) (object rdfs:Class))
  (cond ((rdf-metaclass-p object)
         (cond ((typep subject object)
                (addClass (list (class-of subject)) subject '() `((rdf:|type| ,object))))
               (t (addClass (list object) subject '() `((rdf:|type| ,object))))))
        (t (warn "Entail in ~S rdf:type ~S:~%..... ~S rdfs:subClassOf rdfs:Class." 
             subject object object)
           (addClass (class-of object) object `(,rdfs:Class) '())
           (cond ((typep subject object)
                  (addClass (list (class-of subject)) subject '() `((rdf:|type| ,object))))
                 (t (addClass (list object) subject '() `((rdf:|type| ,object))))))))

;;;
;;;; <Resource> rdf:type <Class> --> <Resource> rdf:type <Class>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate (eql rdf:|type|)) (object rdfs:Class))
  (assert (not (rdf-metaclass-p subject)))
  (cond ((typep subject object) subject)
        (t (addClass (list object) subject '() `((rdf:|type| ,object))))))

;;;
;;;; <Resource> rdf:type t --> <Resource> rdf:type <Resource>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate (eql rdf:|type|)) object)
  (cond ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((range (get-range predicate))) ;range == rdfs:Class
             (warn "Entail in ~S rdf:type ~S:~%..... ~S rdf:type ~S." 
               subject object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (symbol-value object))))))

(defmethod addTriple ((subject rdfs:Resource) (predicate (eql rdf:|type|)) (object rdfs:Resource))
  (assert (strict-class-p object))
  (warn "Entail in ~S rdf:type ~S:~%..... ~S rdfs:subClassOf rdfs:Class." 
    subject object object subject)
  (addClass rdfs:Class object `(,rdfs:Class) '())
  (addClass (list object) subject '() '()))

;;;
;;;; <symbol> rdf:type t --> <Resource> rdf:type t
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdf:|type|)) object)
  (cond ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        (t (call-next-method))))

;;;
;;;; <t> rdf:type <symbol>  -->  <t> rdf:type <Class>
;;;
;;; Range constraint is used for proactive entailment for undefined <symbol>.
;;; See entaiment rule rdfs3.

(defmethod addTriple (subject (predicate (eql rdf:|type|)) (object symbol))
  (unless (object? object)
    (let ((range (get-range predicate))) ;range == rdfs:Class
      (warn "Entail in ~S rdf:type ~S:~%..... ~S rdf:type ~S." 
        subject object object (name range))
      (addTriple object rdf:|type| range)))
  (addTriple subject predicate (symbol-value object)))

;;;
;;;; <t> rdf:type <iri>  -->  <t> rdf:type <Class>
;;;

(defmethod addTriple (subject (predicate (eql rdf:|type|)) (object iri))
  (unless (and (iri-boundp subject)(rsc-object-p (iri-value object)))
    (let ((range (get-range predicate))) ;range == rdfs:Class
      (warn "Entail in ~S rdf:type ~S:~%..... ~S rdf:type ~S." 
        subject object object (name range))
      (addTriple object rdf:|type| range)))
  (addTriple subject predicate (iri-value object)))

;;;
;;;; <symbol> rdf:type <Class>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdf:|type|)) (object rdfs:Class))
  "This form is turned out to an <addClass> form, due to predicate rdf:type."
  (cond ((and (boundp subject) (typep (symbol-value subject) object))
         (symbol-value subject))
        ((rdf-metaclass-p object)
         (addClass (list object) subject '() '()))
        (t (addInstance (list object) subject '()))))

;;;
;;;; <iri> rdf:type <Class>
;;;

(defmethod addTriple ((subject iri) (predicate (eql rdf:|type|)) (object rdfs:Class))
  "This form is turned out to an <addClass> form, due to predicate rdf:type.
   <subject> as iri is stored into rdf:about slot."
  (cond ((iri-boundp subject)
         (cond ((typep (iri-value subject) object)
                (iri-value subject))
               (t (addTriple (iri-value subject) predicate object))))
        ((rdf-metaclass-p object)
         (addClass (list object) (uri2symbol subject) '() `((rdf:|about| ,subject))))
        (t (addInstance (list object) (uri2symbol subject) `((rdf:|about| ,subject))))))

;;;
;;;; <t> rdfs:subClassOf <t>
;;;

(defmethod addTriple (subject (predicate (eql rdfs:subClassOf)) object)
  (error "Triple input error: ~S ~S ~S." subject predicate object))

;;;
;;;; <Class> rdfs:subClassOf <Class>
;;;

(defmethod superclasses-of ((object rdfs:Class))
  (mop:class-direct-superclasses object))
(defmethod subclasses-of ((object rdfs:Class))
  (mop:class-direct-subclasses object))

(defmethod addTriple ((subject rdfs:Class) (predicate (eql rdfs:subClassOf)) (object rdfs:Class))
  ;; if subject is already subclass of object, nothing done.
  (if (cl:subtypep subject object) subject
    (let ((supers (superclasses-of subject)))
      (setq supers (most-specific-concepts (cons object supers)))
      (addClass (list (class-of subject)) subject supers '()))))

;;;
;;;; <Resource> rdfs:subClassOf <Resource>  -->  <Class> rdfs:subClassOf <Class>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate (eql rdfs:subClassOf)) (object rdfs:Resource))
  (unless (rdf-class-p subject) (change-class subject rdfs:Class))
  (unless (rdf-class-p object) (change-class object rdfs:Class))
  (addTriple subject predicate object))

;;;
;;;; <symbol> rdfs:subClassOf <Resource>  -->  <Class> rdfs:subClassOf <Resource>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subClassOf)) (object rdfs:Class))
  (cond ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S rdfs:subClassOf ~S:~%..... ~S rdf:type ~S." 
               subject object subject (name domain))
             (addTriple subject rdf:|type| domain)
             (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; <iri> rdfs:subClassOf <Resource>  -->  <Class> rdfs:subClassOf <Resource>
;;;

(defmethod addTriple ((subject iri) (predicate (eql rdfs:subClassOf)) (object rdfs:Class))
  (cond ((and (iri-boundp subject) (iri-value subject))
         (addTriple (iri-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S rdfs:subClassOf ~S:~%..... ~S rdf:type ~S." 
               subject object subject (name domain))
             (addTriple subject rdf:|type| domain)
             (addTriple (iri-value subject) predicate object)))))

;;;
;;;; <symbol> rdfs:subClassOf <symbol>  -->  <symbol> rdfs:subClassOf <Class>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subClassOf)) (object symbol))
  (cond ((property? object)
         (error "Range violation: ~S for rdfs:subClassOf range." (symbol-value object)))
        ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((range (get-range predicate)))
             (warn "Entail in ~S rdfs:subClassOf ~S:~%..... ~S rdf:type ~S." 
               subject object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (symbol-value object))))))

;;;
;;;; <iri> rdfs:subClassOf <iri>  -->  <iri> rdfs:subClassOf <Class>
;;;

(defmethod addTriple ((subject iri) (predicate (eql rdfs:subClassOf)) (object iri))
  (cond ((and (iri-boundp subject) (property-p (iri-value object)))
         (error "Range violation: ~S for rdfs:subClassOf range." (iri-value object)))
        ((iri-value object)
         (addTriple subject predicate (iri-value object)))
        (t (let ((range (get-range predicate)))
             (warn "Entail in ~S rdfs:subClassOf ~S:~%..... ~S rdf:type ~S." 
               subject object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (iri-value object))))))

;;;
;;;; <t> rdfs:subPropertyOf <t>
;;;

(defmethod addTriple (subject (predicate (eql rdfs:subPropertyOf)) object)
  (error "Triple input error: ~S ~S ~S." subject predicate object))

;;;
;;;; <Property> rdfs:subPropertyOf <Property>
;;;

(defun strict-abst-property-p (abst spec)
  (strict-subproperty-p spec abst nil))

(defun most-specific-property (absts)
  (let ((l (remove-duplicates absts)))    ; eql should be assured
    (set-difference l l :test #'strict-abst-property-p)))

(defmethod addTriple ((subject rdf:|Property|) (predicate (eql rdfs:subPropertyOf)) (object rdf:|Property|))
  ;; if subject is already subproperty of object, nothing done.
  (if (subproperty-p subject object) subject
    (let ((supers (slot-value subject 'rdfs:subPropertyOf)))
      (setq supers (most-specific-property (cons object supers)))
      (addInstance (class-of subject) subject `((rdfs:subPropertyOf ,@supers))))))

;;;
;;;; <symbol> rdfs:subPropertyOf <Property>  -->  <Resource> rdfs:subPropertyOf <Property>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subPropertyOf)) (object rdf:|Property|))
  (cond ((object? subject)
         (addTriple (symbol-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object subject (name domain))
             (addTriple subject rdf:|type| domain)
             (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; <iri> rdfs:subPropertyOf <Property>  -->  <Resource> rdfs:subPropertyOf <Property>
;;;

(defmethod addTriple ((subject iri) (predicate (eql rdfs:subPropertyOf)) (object rdf:|Property|))
  (cond ((and (iri-boundp subject) (iri-value subject))
         (addTriple (iri-value subject) predicate object))
        (t (let ((domain (get-domain predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object subject (name domain))
             (addTriple subject rdf:|type| domain)
             (addTriple (iri-value subject) predicate object)))))

;;;
;;;; <symbol> rdfs:subPropertyOf <symbol> -->  <symbol> rdfs:subPropertyOf <Resource>
;;;

(defmethod addTriple ((subject symbol) (predicate (eql rdfs:subPropertyOf)) (object symbol))
  (cond ((property? object)
         (addTriple subject predicate (symbol-value object)))
        ((object? object)
         (error "Range violation ~S for rdfs:subPropertyOf" object))
        (t (let ((range (get-range predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (symbol-value object))))))

;;;
;;;; <Resource> <Property> <symbol>  -->  <Resource> <Property> <Resource>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:|Property|) (object symbol))
  (cond ((object? object)
         (addTriple subject predicate (symbol-value object)))
        (t (let ((range (or (get-range predicate) |rdfs:Resource|)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (symbol-value object))))))

;;;
;;;; <Resource> <Property> <iri>  -->  <Resource> <Property> <Resource>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:|Property|) (object iri))
  (cond ((and (iri-boundp object) (iri-value object))
         (addTriple subject predicate (iri-value object)))
        (t (let ((range (or (get-range predicate) |rdfs:Resource|)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:|type| range)
             (addTriple subject predicate (iri-value object))))))

;;;
;;;; <Resource> <Property> <uri>  -->  <Resource> <Property> <iri>
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:|Property|) (object uri))
  (addTriple subject predicate (iri object)))
#|
;;;
;;;; <Class> <Property> <t> ; intall with domain checking
;;;

(defmethod addTriple ((subject rdfs:Class) (predicate rdf:|Property|) object)
  (let ((domains (get-domain predicate)))
    (cond ((typep subject domains)
           (addClass (list (class-of subject)) subject '() `((,(name predicate) ,object))))
          ((atom domains)
           (warn "Domain Entail: ~S rdf:|type| ~S." (name domains) object)
           (addInstance domains subject `((,(name predicate) ,object))))
          ((error 'domain-condition-unsatisfiable
             :format-control
             "CHECK DOMAIN of ~S to ~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]."
             :format-arguments `(,subject ,@(mapcar #'get-form (cdr domains))))))))
|#
;;;
;;;; <Resource> <Property> <t> ; intall with domain checking
;;;

(defmethod addTriple ((subject rdfs:Resource) (predicate rdf:|Property|) object)
  ;(format t "~%Adding ~S ~S ~S ." subject predicate object)
  (let ((name (if (anonymous-p subject) (make-unique-nodeID "aa") (name subject)))
        (domains (get-domain predicate)))
    (setf (symbol-value name) subject)
    (cond ((null domains)
           (addInstance (class-of subject) name `((,(name predicate) ,object))))
          ((typep subject domains)
           (addInstance (class-of subject) name `((,(name predicate) ,object))))
          ((atom domains)
           (warn "Domain Entail: ~S rdf:|type| ~S." (name domains) object)
           (addInstance domains name `((,(name predicate) ,object))))
          ((error 'domain-condition-unsatisfiable
             :format-control
             "CHECK DOMAIN of ~S to ~#[ none~; ~S~; ~S and ~S~:;~@{~#[~; and~] ~S~^,~}~]."
             :format-arguments `(,subject ,@(mapcar #'get-form (cdr domains))))))))

(defmethod addTriple ((subject rdfs:Resource) (predicate rdfs:Resource) object)
  (addTriple subject (change-class predicate rdf:|Property|) object))

(defun collect-domaind (slots)
  "collects domain information from properties in <slots>."
  (loop for slot in slots with domain
      when (setq domain (and (boundp (slot-role slot))
                             (get-domain (symbol-value (slot-role slot)))))
      collect (if (and (symbolp domain) (boundp domain)) (symbol-value domain) domain)))

;;;
;;;; <symbol> <Property> <t>  -->  <Resource> <Property> <t>
;;;

(defmethod addTriple ((subject symbol) (predicate rdf:|Property|) object)
  (let ((domain nil))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:|type| ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (symbol-value subject) predicate object))
          ((typep object rdfs:Resource)
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource."
             subject (name predicate) object subject)
           (addTriple subject rdf:|type| |rdfs:Resource|)
           (addTriple (symbol-value subject) predicate object))
          ((error "NOT YET")))))

(defmethod addTriple ((subject symbol) (predicate rdfs:Resource) object)
  (addTriple subject (change-class predicate rdfs:Resource) object))

;;;
;;;; <iri> <Property> <t>  -->  <Resource> <Property> <t>
;;;

(defmethod addTriple ((subject iri) (predicate rdf:|Property|) object)
  (let ((domain nil))
    (cond ((and (iri-boundp subject) (iri-value subject))
           (addTriple (iri-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (iri-value subject) predicate object))
          ((typep object rdfs:Resource)
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource."
             subject (name predicate) object subject)
           (addTriple subject rdf:|type| |rdfs:Resource|)
           (addTriple (iri-value subject) predicate object))
          ((error "NOT YET")))))

(defmethod addTriple ((subject iri) (predicate rdfs:Resource) object)
  (addTriple subject (change-class predicate rdfs:Resource) object))

;;;
;;;; <symbol> <Property> <number>  -->  <Resource> <Property> <number>
;;;

(defmethod addTriple ((subject symbol) (predicate rdf:|Property|) (object cl:number))
  "Domain entailment and range checks."
  (let ((domain nil)
        (range nil))
    (when (setq range (get-range predicate))
      (unless (typep object range)
        (error "Range violation:~S for ~S" object range))
      (unless (cl:typep object range)
        (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S"
          subject (name predicate) object object range)
        (addTriple object rdf:|type| range)))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object
             subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (symbol-value subject) predicate object))
          (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
               subject (name predicate) object subject)
             (addTriple subject rdf:|type| |rdfs:Resource|)
             (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; <iri> <Property> <number>  -->  <Resource> <Property> <number>
;;;

(defmethod addTriple ((subject iri) (predicate rdf:|Property|) (object cl:number))
  "Domain entailment and range checks."
  (let ((domain nil)
        (range nil))
    (when (setq range (get-range predicate))
      (unless (typep object range)
        (error "Range violation:~S for ~S" object range))
      (unless (cl:typep object range)
        (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S"
          subject (name predicate) object object range)
        (addTriple object rdf:|type| range)))
    (cond ((and (iri-boundp subject) (rsc-object-p (iri-value subject)))
           (addTriple (iri-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object
             subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (iri-value subject) predicate object))
          (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
               subject (name predicate) object subject)
             (addTriple subject rdf:|type| |rdfs:Resource|)
             (addTriple (iri-value subject) predicate object)))))

;;;
;;;; <symbol> <Property> <string>  -->  <Resource> <Property> <string>
;;;

(defmethod addTriple ((subject symbol) (predicate rdf:|Property|) (object cl:string))
  (let ((domain nil))
    (cond ((object? subject)
           (addTriple (symbol-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (symbol-value subject) predicate object))
          (t ;; very new input without any information of property
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
             subject (name predicate) object subject)
           (addTriple subject rdf:|type| |rdfs:Resource|)
           (addTriple (symbol-value subject) predicate object)))))

;;;
;;;; <iri> <Property> <string>  -->  <Resource> <Property> <string>
;;;

(defmethod addTriple ((subject iri) (predicate rdf:|Property|) (object cl:string))
  (let ((domain nil))
    (cond ((and (iri-boundp subject) (rsc-object-p (iri-value subject)))
           (addTriple (iri-value subject) predicate object))
          ((setq domain (get-domain predicate))
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
             subject (name predicate) object subject (name domain))
           (addTriple subject rdf:|type| domain)
           (addTriple (iri-value subject) predicate object))
          (t ;; very new input without any information of property
           (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource"
             subject (name predicate) object subject)
           (addTriple subject rdf:|type| |rdfs:Resource|)
           (addTriple (iri-value subject) predicate object)))))

;;;
;;;; <symbol> <Property> <symbol>  -->  <symbol> <Property> <Resource>
;;;
;;; Range constraint is used for satisfiability checking and proactive entailment.
;;; See entaiment rule rdfs3.

(defmethod addTriple ((subject symbol) (predicate rdf:|Property|) (object symbol))
  (cond ((subproperty-p predicate rdf:|type|)     ; accepts every subproperty of rdf:type but not rdf:|type|
         (unless (object? object)
           (let ((range (get-range predicate)))
             (cond ((null range) (error "Check it!"))
                   (t
                    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                      subject (name predicate) object object (name range))
                    (addTriple object rdf:|type| range)))))
         (addTriple subject rdf:|type| (symbol-value object))
         (addTriple subject predicate (symbol-value object)))
        ((object? object)
         (let ((range (get-range predicate))
               (obj (symbol-value object)))
           (cond ((null range)
                  (addTriple subject predicate obj))
                 (t (unless (typep obj range)
                      (warn "Range entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                        subject (name predicate) object object (name range))
                      (addTriple obj rdf:|type| range))
                    (addTriple subject predicate obj)))))
        (t (let ((range nil))
             (cond ((setq range (get-range predicate))
                    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                      subject (name predicate) object object (name range))
                    (addTriple object rdf:|type| range)
                    (addTriple subject predicate (symbol-value object)))
                   (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource." 
                        subject (name predicate) object object)
                      (addTriple object rdf:|type| |rdfs:Resource|)
                      (addTriple subject predicate (symbol-value object))))))))


;;;
;;;; <iri> <Property> <iri>  -->  <iri> <Property> <Resource>
;;;
;;; Domain and range constraint is used for satisfiability checking and proactive entailment.
;;; See entaiment rule rdfs3.

(defmethod addTriple ((subject iri) (predicate rdf:|Property|) (object iri))
  (cond ((subproperty-p predicate rdf:|type|)     ; accepts every subproperty of rdf:type but not rdf:|type|
         (unless (and (iri-boundp object) (rsc-object-p (iri-value object)))
           (let ((range (get-range predicate)))
             (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
               subject (name predicate) object object (name range))
             (addTriple object rdf:|type| range)))
         (addTriple subject rdf:|type| (iri-value object))
         (addTriple subject predicate (iri-value object)))
        ((and (iri-boundp object) (rsc-object-p object))
         (addTriple subject predicate (iri-value object)))
        (t (let ((range nil))
             (cond ((setq range (get-range predicate))
                    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type ~S." 
                      subject (name predicate) object object (name range))
                    (addTriple object rdf:|type| range)
                    (addTriple subject predicate (iri-value object)))
                   (t (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdfs:Resource." 
                        subject (name predicate) object object)
                      (addTriple object rdf:|type| |rdfs:Resource|)
                      (addTriple subject predicate (iri-value object))))))))

;;;
;;;; (quote <resource>) Property t --> <resource> Property t
;;;

(defmethod addTriple ((subject cons) (predicate rdf:|Property|) object)
  (if (eq (car subject) 'quote)
      (if (null (cddr subject))
          (addTriple (second subject) predicate object)
        (error "Cant happen!"))
    (error "Cant happen!")))  

;;;
;;;; <t> <Property> <uri>  -->  <t> <Property> <iri>
;;;

(defmethod addTriple (subject (predicate rdf:|Property|) (object uri))
  (addTriple subject predicate (iri object)))

;;;
;;;; <iri> <symbol> t  -->  <iri> <Property> t
;;;
;;; If <symbol> is undefined, entailment rule rdf1 is applied.

(defmethod addTriple ((subject iri) (predicate symbol) object)
  (when (null subject) (error "Cant happen!"))
  (unless (property? predicate)
    (export predicate (symbol-package predicate))
    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdf:Property." 
      subject predicate object predicate)
    (addTriple predicate rdf:|type| rdf:|Property|))
  (addTriple subject (symbol-value predicate) object))

;;;
;;;; t <symbol> t  -->  t <Property> t
;;;
;;; If <symbol> is undefined, entailment rule rdf1 is applied.

(defmethod addTriple (subject (predicate symbol) object)
  (when (null subject) (error "Cant happen!"))
  (unless (property? predicate)
    (export predicate (symbol-package predicate))
    (warn "Entail in ~S ~S ~S:~%..... ~S rdf:type rdf:Property." 
      subject predicate object predicate)
    (addTriple predicate rdf:|type| rdf:|Property|))
  (addTriple subject (symbol-value predicate) object))

;;;
;;;; t <uri> t  -->  t <symbol> t
;;;

(defmethod addTriple (subject (predicate uri) object)
  (let ((symbol (uri2symbol predicate)))
    (when (or (null symbol) (not (symbolp symbol)))
      (error "predicate in SWCLOS must be turned a QName."))
    (prog1 (addTriple subject symbol object)
      (setf (slot-value (symbol-value symbol) 'rdf:|about|) predicate))))

;;;
;;;; <iri> <uri> t  -->  <iri> <symbol> t
;;;

(defmethod addTriple ((subject iri) (predicate uri) object)
  (let ((symbol (uri2symbol predicate)))
    (when (or (null symbol) (not (symbolp symbol)))
      (error "predicate in SWCLOS must be turned a QName."))
    (prog1 (addTriple subject symbol object)
      (setf (slot-value (symbol-value symbol) 'rdf:|about|) predicate))))

;;;
;;;; <uri> t t  -->  <iri> t t
;;;

(defmethod addTriple ((subject uri) predicate object)
  (addTriple (iri subject) predicate object))

;;
;; Invalid Statements
;;

(defmethod addTriple ((subject symbol) (predicate (eql rdf:|type|)) (object rdfs:Resource))
  (error "Invalid statement: ~S ~S ~S." subject predicate object))

(defmethod addTriple ((subject (eql 'xsd:|string|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|decimal|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|float|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|double|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|int|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|integer|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|long|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|short|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|positiveInteger|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|nonPositiveInteger|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|negativeInteger|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))
(defmethod addTriple ((subject (eql 'xsd:|nonNegativeInteger|)) predicate object)
  (error "Invalid statement: ~S ~S ~S." subject predicate object))

;;
;; File Interface
;;

(defun intern-from-nodeID (ID)
  (assert (char= #\_ (elt ID 0)))
  (assert (char= #\: (elt ID 1)))
  (let ((package (or (find-package :_) (make-package :_ :use nil))))  ; by smh
    (let ((symbol (intern (subseq ID 2) package)))
      (export symbol package)
      symbol)))

(defun intern-langString (literal)
  "langString ::= '\"' string '\"' ( '@' language )?"
  (let ((i 0))
    (let ((string (subseq literal i (setq i (1+ (position-with-escape #\" literal (1+ i)))))))  ; " to "
      (setq string (subseq string 1 (1- (length string)))) ; strip " and "
      (cond ((and (< i (length literal)) (char= (char literal i) #\@))
             (let ((lang (parse-language literal (1+ i))))
               (make-instance 'rdf:|inLang| :lang (intern lang "keyword") :content string)))
            (t string)))))

(defun intern-datatypeString (literal)
  "datatypeString ::= langString '^^' uriref "
  (let ((i 0))
    (let ((string (subseq literal i (setq i (1+ (position-with-escape #\" literal (1+ i)))))))  ; " to "
      (cond ((and (< i (1- (length literal))) (char= (char literal i) #\^) (char= (char literal (1+ i)) #\^))
             (let ((uriref (parse-iriref literal (1+ (1+ i)))))
               (make-instance (uri2symbol (subseq uriref 1 (1- (length uriref))))
                 :value (intern-langString string))))
            (t (intern-langString string))))))

(defun intern-literal (literal)
  (cond ((datatypeString-p literal 0) (intern-datatypeString literal))
        ((langString-p literal 0) (intern-langString literal))
        ((error "Illegal literal."))))

(defun intern-from-QName (QName)
  (multiple-value-bind (name space) (name&space QName)
    (let ((package (if space (find-package space) *package*)))
      (assert (or package
                  (and (y-or-n-p "Make package ~A?" space)
                       (setq package (make-package space :use nil)))))  ; by smh
      (let ((symbol (intern name package)))
        (export symbol package)
        symbol))))

(defun addTriple-from-file (subject predicate object)
  (cond ((and (null subject) (null predicate) (null object))) ; Null line
        ((and (null predicate) (null object)))                ; Comment line
        (t (addTriple (intern-subject subject) (intern-predicate predicate) (intern-object object)))))

(defun intern-subject (str)
  (cond ((uriref-p str 0) (iri (subseq str 1 (1- (length str)))))
        ((ID-p str 0) (intern-from-nodeID str))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal subject:~%~A" str))))

(defun intern-predicate (str)
  (cond ((uriref-p str 0) (iri (subseq str 1 (1- (length str)))))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal predicate:~%~A" str))))

(defun intern-object (str)
  (cond ((uriref-p str 0) (iri (subseq str 1 (1- (length str)))))
        ((ID-p str 0) (intern-from-nodeID str))
        ((literal-p str 0)(intern-literal str))
        ((QName-p str 0)  (intern-from-QName str))
        ((error "Illegal object:~~%~A" str))))

#|

(/. xsd:|integer| rdfs:subClassOf xsd:|string|)  -> ERROR
(/. xsd:|integer| rdfs:subClassOf xsd:|decimal|) -> ERROR

(/. prop1 rdfs:range xsd:|string|)
(/. foo prop1 25)                     -> ERROR

(/. prop2 rdfs:range xsd:|integer|)
(/. foo prop2 "25")                   -> 25

(/. prop3 rdf:|type| rdf:|Property|)
(/. bar rdfs:subClassOf prop3)         -> ERROR

(/. bar rdf:|type| rdf:|Property|)
(/. bas rdfs:subPropertyOf bar)
(/. bar rdfs:domain Domain1)
(/. bas rdfs:domain Domain2)
(/. bar rdfs:range Range1)
(/. bas rdfs:range Range2)
(/. baz1 bas baz2)

(defun revert-slot (slotd)
  (let ((role (mop:slot-definition-name slotd))
        (filler (mop:slot-definition-initform slotd))
        (readers (mop:slot-definition-readers slotd))
        (writers (mop:slot-definition-writers slotd)))
    `(:name ,role :initargs (,role) :initform ,filler :type ,filler
            :readers ,readers :writers ,writers)))

(defun revert-slots (dslots)
  (loop for slotd in dslots collect (revert-slot slotd)))

|#

(defun get-triple (resource)
  (when (null resource) (return-from get-triple))
  (assert (typep resource rdfs:Resource))
  (flet ((name-in-get-triple (rsc)
           (cond ((rsc-object-p rsc)
                  (or (name rsc) (slot-value rsc 'rdf:|about|)))
                 ((and (symbolp rsc) (nodeID? rsc)) rsc)
                 ((error "Cant happen!"))))
         (object-in-get-triple (rsc)
           (cond ((rsc-object-p rsc) rsc)
                 ((and (symbolp rsc) (nodeID? rsc)) (symbol-value rsc))
                 ((error "Cant happen!")))))
    (let ((subject (name-in-get-triple resource)))
      (append 
       (mapcar #'(lambda (ty) `(,subject rdf:|type| ,(name-in-get-triple ty)))
         (mclasses (object-in-get-triple resource)))
       (loop for slot in (get-slots resource)
           append (let ((role (slot-role slot))
                        (forms (slot-forms slot)))
                    (mappend #'(lambda (filler)
                                 (cond ((typep filler rdfs:Literal)
                                        `((,subject ,role ,filler)))
                                       ((rsc-object-p filler)
                                        (cond ((named-p filler)
                                               `((,subject ,role ,(name filler))))
                                              (t (let ((nodeid (make-unique-nodeID "gx")))
                                                   (setf (symbol-value nodeid) filler)
                                                   (cons
                                                    `(,subject ,role ,nodeid)
                                                    (get-triple nodeid))))))
                                       (t `((,subject ,role ,filler)))))
                             forms)))))))

;; End of module
;; --------------------------------------------------------------------

