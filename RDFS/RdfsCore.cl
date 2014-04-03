;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdfs Core module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright (c) 2002, 2003, 2004, Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2008, Seiji Koide
;;
;; History
;; -------
;;
;;; ==================================================================================

(cl:provide :rdfscore)

(cl:defpackage :gx
  (:export addObject addForm addInstance addClass defResource defProperty defConcept 
           defIndividual subproperty-p addRdfXml)
  )

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :utils)
  (require :swclospackages)
  (require :rdfshare)
  (require :rdfsobjects)
  (require :domainrange)
  (require :rdfskernel)
  (copy-readtable rdf::*standard-readtable* cl:*readtable*) ; after RdfsKernel See it.
) ; end of eval-when

(in-package :gx)

(declaim (inline create-slot slot-role slot-forms slot-filler get-filler))

;;;; Top Level Macro and Intermediate Layers out of Three Layers in RDF(S) and OWL Definition
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
;;; This file includes (i) the top level macro layer and (ii) the intermediate function/method layer. 
;;; The MOP programming layer is contained in Rdfs Kernel module.

;;;
;;;; Some Global Vars
;;;

(defvar lang-env nil "xml:lang evnironment")

;;;
;;;; Syntax of Top Level Definition
;;; The syntax of top level macros are as follows.
;;; # <defConcept>    : define a resource class in RDF(S) or OWL.
;;; # <defProperty>   : define a property in RDF(S) or OWL.
;;; # <defIndividual> : define an individual or instance in RDF(S) or OWL.
;;; ----------------------------------------------------------------------------------
;;;  <defform>        ::= <resource-def> | <property-def> | <individual-def>
;;;  <resource-def>   ::= (defConcept <resource-name> <slot-form>* )
;;;  <property-def>   ::= (defProperty <property-name> <slot-form>* )
;;;  <individual-def> ::= (defIndvidual <individual-name> <slot-form>* )
;;;  <slot-form>      ::= (<role> [<lang>] <form> <form>*)
;;;  <form>           ::= (<typetag> [<name>] [<lang-form>] <slot-form>*)
;;;                     | (<datatype> <data>) | (<lang> <form>) 
;;;                     | <cl:string> | <cl:number> | <uri> 
;;;  <lang-form> ::= (xml:lang <lang>)
;;; ----------------------------------------------------------------------------------
;;; <resource-name>, <propety-name>, <individua-name> is a symbol that represents QName of resource.
;;; <role> is a symbol that represents a property QName.
;;; <lang> is a language tag such as :en or :ja.
;;; <typetag> is a symbol that represents a resource class QName.
;;; <datatype> is a symbol that represents a xsd datatype QName.
;;;
;;; To direct a class in defining an entity, rdf:type is used in <slot-form> as same as other slot-forms. For example,
;;; ----------------------------------------------------------------------------------
;;;    (defIndividual vin::ElyseZinfandel (rdf:type vin::Zinfandel)).
;;; ----------------------------------------------------------------------------------
;;; As default, rdf:Property is used for property class in <defProperty>. 
;;; To direct an object property in OWL, rdf:type is used in <defProperty> as follows.
;;; ----------------------------------------------------------------------------------
;;;    (defProperty vin::hasColor (rdf:type owl::ObjectProperty))
;;; ----------------------------------------------------------------------------------
;;; Note that owl:FunctionalProperty is not a subclass of owl:ObjectProperty, while 
;;; owl:InverseFunctionalProperty is a subclass of owl:ObjectProperty. Therefore, it may 
;;; be needed to add owl:ObjectProperty with owl:FunctionalProperty as follows.
;;; ----------------------------------------------------------------------------------
;;;  (defProperty vin::hasMaker 
;;;    (rdf:type owl:FunctionalProperty owl:ObjectProperty))
;;;
;;;  (defProperty vin::producesWine
;;;    (rdf:type owl:InverseFunctionalProperty)
;;;    (owl:inverseOf vin::hasMaker))
;;; ----------------------------------------------------------------------------------

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defun expand-def (type name args)
    "<type> may be symbol rdfs:Class, rdf:Property, rdfs:Resource, or '|rdfs:Resource|."
    (cond ((null name)
           (cond ((and (null type) (assoc 'rdf:type args))
                  `(addForm ',(cons (mkatom (cdr (assoc 'rdf:type args))) args)
                            t))
                 (t `(addForm ',(cons type args)
                              t))))
          ((symbolp name)
           (cond ((and (null type) (assoc 'rdf:type args))
                  `(progn
                     (defparameter ,name
                       (addForm ',(cons (mkatom (cdr (assoc 'rdf:type args))) (cons `(:name ,name) args))
                                t))
                     ,name))
                 (t `(progn
                       (defparameter ,name
                         (addForm ',(cons type (cons `(:name ,name) args))
                                  t))
                       ,name))))
          ((error "Cant happen!"))))
  )

(defmacro defResource (name &rest args)
  "defines a class in OWL or a resource class in RDF(S). 
   This macro sets the class object to the symbol <name> 
   and returns the class object."
  (assert (symbolp name))
  `(progn (excl:record-source-file ',name :type :type)
     ,(expand-def 'rdfs:Class name args)))

(defmacro defConcept (name &rest args)
  "defines a class in OWL or a resource class in RDF(S). 
   This macro sets the class object to the symbol <name> 
   and returns the class object."
  (assert (symbolp name))
  (cond ((and (assoc 'rdf:type args)
              (length=1 (cdr (assoc 'rdf:type args))))
         (cond ((eql 'owl:Class (cadr (assoc 'rdf:type args)))
                `(progn (excl:record-source-file ',name :type :type)
                   ,(expand-def 'owl:Class name (remove 'rdf:type args :key #'car))))
               ((eql 'rdfs:Class (cadr (assoc 'rdf:type args)))
                `(progn (excl:record-source-file ',name :type :type)
                   ,(expand-def 'rdfs:Class name (remove 'rdf:type args :key #'car))))
               (t `(progn (excl:record-source-file ',name :type :type)
                     ,(expand-def 'rdfs:Class name args)))))
        (t `(progn (excl:record-source-file ',name :type :type)
              ,(expand-def 'rdfs:Class name args)))))

(defmacro defProperty (name &rest args)
  "defines an instance of rdf:Property. 
   This macro sets a property object to the symbol <name> and 
   returns the property object."
  (assert (symbolp name))
  (expand-def 'rdf:Property name args))

(defmacro defIndividual (name &rest args)
  "defines an individual of owl:Resource 
   with <name> and slots.  This macro sets the individual object to 
   the symbol <name> and the object."
  (assert (symbolp name))
  (cond ((assoc 'rdf:type args) (expand-def 'rdfs:Resource name args))
        (t (expand-def '|rdfs:Resource| name args))))

(defun addRdfXml (description)
  ;;(format t "~%~S" description)
  (cond ((Description-p description)
         (let* ((form (Description-form description))
                (about (second (assoc 'rdf:about (cdr form))))
                (id (second (assoc 'rdf:ID (cdr form))))
                (nodeID (second (assoc 'rdf:nodeID (cdr form))))
                (name (cond ((string= (string (Description-tag description)) "Ontology")
                             (name-ontology about))
                            (about (uri2symbol about)))))
           (when id
             (assert (null nodeID))
             (setq form (cons (car form)
                              (remove (assoc 'rdf:ID (cdr form)) (cdr form)))))
           (when nodeID
             (setq form (cons (car form)
                              (remove (assoc 'nodeID (cdr form)) (cdr form)))))
           (unless name
             (when id (setq name id))
             (when nodeID (setq name (nodeID2symbol nodeID))))
           ;(format t "~%~S" (list* (car form) `(:name ,name) (cdr form)))
           (addForm (list* (car form) `(:name ,name) (cdr form)) nil)))
        ((error "What ~S" description))))

(defun lasyAddRdfXml (description)
  "This function lazily <addForm> when it is forced.
   <delay> must be explicitly called with <force> function."
  (cond ((Description-p description)
         (let* ((form (Description-form description))
                (about (second (assoc 'rdf:about (cdr form))))
                (id (second (assoc 'rdf:ID (cdr form))))
                (nodeID (second (assoc 'rdf:nodeID (cdr form))))
                (name (cond ((string= (string (Description-tag description)) "Ontology")
                             (name-ontology about))
                            (about (uri2symbol about)))))
           (when id
             (assert (null nodeID))
             (setq form (cons (car form)
                              (remove (assoc 'rdf:ID (cdr form)) (cdr form)))))
           (when nodeID
             (setq form (cons (car form)
                              (remove (assoc 'nodeID (cdr form)) (cdr form)))))
           (unless name
             (when id (setq name id))
             (when nodeID (setq name (nodeID2symbol nodeID))))
           ;(format t "~%~S" (list* (car form) `(:name ,name) (cdr form)))
           (lasyAddForm (list* (car form) `(:name ,name) (cdr form)) nil)))
        ((error "What ~S" description))))

;;;
;;;; Defining Form in S-expression for RDF Entity
;;;
;;; The defining macro at the top level mentioned above internally produces the code for input form 
;;; in S-expression and calls function <addForm> with it.
;;; Reading RDF/XML file also makes a sequence of input forms in S-expression (See <addRdfXml> and <Description-form>), 
;;; and such input form is processed by <addForm>.
;;;
;;; Input form in S-expression has recursive structures. The BNF syntax for defining an RDF resource in S-expression is as follows.
;;; ----------------------------------------------------------------------------------
;;;  <form> ::= <cl:string> | <cl:number> | <uri> 
;;;           | (<lang> <form>)
;;;           | (<^^> <data>) 
;;;           | (<typetag> [<lang-form>] <slot-form>*)
;;;  <slot-form>   ::= <about-form> | <id-form> | <nodeID-form> | <name-form> | <prop-form>
;;;  <about-form>  ::= (rdf:about <uri>)
;;;  <id-form>     ::= (rdf:ID <name>)
;;;  <nodeID-form> ::= (rdf:nodeID <name>)
;;;  <name-form>   ::= (:name <name>)
;;;  <lang-form>   ::= (xml:lang <lang>)
;;;  <prop-form>   ::= (<role> [<lang>] <form> <form>*)
;;;  <data>        ::= <cl:number> | <cl:string>
;;; ----------------------------------------------------------------------------------
;;; Here,
;;; * <typetag> is a class name in symbol or a symbol 'rdf:Description'. 
;;; * <role> is a property name in symbol. Note that this name turns out a name of Property-direct-slot-definition object.
;;; * <datatype> is a QName symbol or URI or URI string for datatype.
;;; * <data> is a lisp string or lisp number, but its interpretation must be coinside with designated xsd type definition.
;;; * <uri> is a uri or uri-string or QName symbol. 
;;; * <cl:string> is a lisp string.
;;; * <cl:number> is a lisp number.
;;; * <lang> is a keyword that denotes langugage, e.g., :en, :ja. See <lang?>
;;;
;;;; addForm
;;;
;;; In lisp, <addForm> accepts any defining form that defines an entity or a fragment of entity described above 
;;; and returns the denotation of the form. Namely, 
;;; # If a form is a number in lisp, then it is returned.
;;; # If a form is a string in lisp, then it is read and interpreted as one of RDF datatype 
;;;   (when <role> is supplied and the range constraint is effective), or plane literal without language option 
;;;   or with language option (when some <lang> is set up in the environment). See <read-data>. 
;;; # If a form is a URI, then if <role> suggensts URI such as rdf:about or "imports" it is returned, 
;;;   else its QName is obtained and recurses with the QName.
;;; # If a form is a symbol, then it must be a QName and the denoted object is returned if exists, 
;;;   else the denoted object is newly created at the minimal constraint from role range or rdfs4b rule. 
;;;   See <make-object-with-minimal-constraint>.
;;; # If a form is (<lang> <form>), then the <form> is <addForm>ed in <lang> environment.
;;; # If a form is (<datatype> <data>), then the typed data is created and returned.
;;; # Otherwise the form denotes a complex entity, then each subforms are evaluated through <form2slot> 
;;;   and the form replaced with results is computed by <%addForm>.
;;;

(defun addForm (form &optional role)
  "<form> is a form described above, and <role> is nil in calling at top level 
   but a role for <form> as filler in recursive call."
  (when (eq form t) (return-from addForm t))
  (when (eq form nil) (return-from addForm nil))
  (assert (not (boundp 'rdf:about)))
  (let ((lang-env lang-env))  ; this is effective in read-in-lang-env
    (etypecase form
      (cl:number form)                          ; a number denotes itself
      (cl:string (cond ((property? role)        ; a string denotes itself or typed data if role indicates so.
                        (read-data (get-range-constraint-from role) form))
                       (t (read-in-lang-env form))))
      (net.uri:uri (assert (and (not (eq role t)) (not (eq role nil))))
                   (cond ((eq role 'rdf:about) (error "Cant happen!")) ; form2slot processes it before falling here
                         ((eq role 'rdf:ID) (error "Cant happen!"))
                         ((eq role 'rdfs:isDefinedBy) form)
                         ((string= (string role) "priorVersion") form)  ; avoiding owl package in RDF subsystem
                         ((string= (string role) "imports") form)
                         ((and (object? role)
                               (string= (string (cl:type-of (symbol-value role)))
                                        "OntologyProperty"))
                          form)
                         ((and (object? role)
                               (not (eq role 'rdfs:member))
                               (let ((range (get-range-constraint-from role)))
                                 (or (and (atom range)
                                          (cl:subtypep 'xsd:anyURI range)) ; Seiji 2008/08/06
                                     (and (consp range)
                                          (every #'(lambda (cls) (cl:subtypep 'xsd:anyURI cls)) range)))))
                          ;(format t "~%Form ~S with role ~S" form role)
                          ;(format t "~%  Constraint ~S" (get-range-constraint-from role))
                          form)
                         ((eq (get role :datatype) 'xsd:anyURI) (error "Cant happen!") ; old edition?
                          form)
                         (t (let ((name (uri2symbol form))     ; getting name of URI
                                  (uri (iri form)))            ; ensure interning and getting valued-uri 
                              (cond (name (setf (iri-value uri) (addForm name role)))
                                    ; un-named URI denotes anonymous object
                                    (t (if (iri-boundp uri) (iri-value uri)
                                         (setf (iri-value uri) (addForm `(rdfs:Resource ) role)))
                                       ))))))
      ;; typed literal???
      (symbol (assert (and (not (eq role t)) (not (eq role nil))))
              (unless (keywordp form)
                (push `(,form line ,*line-number*) *referenced-resources*))
              (cond ((lang? form) (setq lang-env form))
                    ((object? form) 
                     (ensure-object-with-minimal-constraint (symbol-value form) role))
                    ((keywordp form) (error "Keyword symbol ~S is designated to QName." form))
                    (t ;; then form is a name
                     (let ((uri (symbol2uri form))
                           (obj (make-object-with-minimal-constraint form role)))
                       (when uri (setf (iri-value uri) obj))
                       obj))))
      (rdf:inLang form)
      (rdfs:Literal form)                                   ; RDF literal denotes itself
      (rdfs:Resource (assert (and (not (eq role t)) (not (eq role nil))))
                     (error "Cant happen!")
                     (when (name form)
                       (push `(,(name form) line ,*line-number*) *referenced-resources*))
                     form)
      (cons (cond ((lang? (car form))
                   (setq lang-env (car form))
                   (addForm (second form) role)) ; skip lang
                  ((and (car form) (rdf-subtypep (car form) 'xsd:anySimpleType))
                   (^^ (second form) (car form)))
                  ((and (symbolp (car form)) (fboundp (car form)) (not (eq (car form) 'rdfs:subClassOf)))
                   (ecase (car form)
                     (iri (iri (second form)))
                     (^^ (^^ (second form) (third form)))
                     (@ (@ (second form) (third form)))))
                  (t (when (and (second form) (atom (second form)))
                       (setf (second form) `(:name ,(second form))))
                     ;(format t "~%Form:~S" form)
                     (let ((domains (most-specific-concepts (collect-domains (mapcar #'car (cdr form))))))
                       ;(format t "~%Domains:~S" domains)
                       (loop for cls in (cdr (assoc 'rdf:type (cdr form)))
                           do (cond ((symbolp cls))
                                    ((net.uri:uri-p cls) (setq cls (uri2symbol cls)))
                                    ((consp cls) (setq cls (addForm cls 'rdf:type)))
                                    (t (error "Not Yet!")))
                             (unless (or (rdf-class-p cls) (class? cls))
                               (warn "Range entail by rdf:type: ~S rdf:type ~S." cls (name (rdfs:range rdf:type)))
                               (cond ((length=1 domains)
                                      (cond ((eq (car domains) rdfs:Resource)
                                             (addClass rdfs:Class cls (list rdfs:Resource)))
                                            ((y-or-n-p "Define ~S as subclass of ~S?" cls (car domains))
                                             (warn "Superclass of ~S is entailed to ~S by domain constraint of other properties."
                                               cls (name (car domains)))
                                             (addForm `(,(type-of (car domains))
                                                          (:name ,cls)
                                                          (rdfs:subClassOf ,(name (car domains))))))
                                            ((warn "Nothing done!"))))
                                     (t (let ((uri (symbol2uri cls))
                                              (obj (addClass (rdfs:range rdf:type) cls ())))
                                          (when uri (setf (iri-value uri) obj))
                                          obj)))))
                       (let ((class
                              (cond ((or (null (car form)) (eq (car form) 'rdf:Description))
                                     (if (assoc 'rdf:type (cdr form))
                                         (let ((cls (cadr (assoc 'rdf:type (cdr form)))))
                                           (etypecase cls
                                             (symbol (symbol-value cls))
                                             (net.uri:uri (symbol-value (uri2symbol cls)))
                                             (cons (addForm cls))))
                                       (if domains (car domains) (symbol-value '|rdfs:Resource|))))
                                    ((consp (car form))
                                     (car (most-specific-concepts
                                           (append domains (mapcar #'symbol-value (reverse (car form)))))))
                                    ((class? (car form))
                                     ;; then class may be symbol-value of (car form)
                                     (car (most-specific-concepts (append domains (list (symbol-value (car form)))))))
                                    (t (warn "Implicit range entailment: ~S rdf:type rdfs:Class." (car form))
                                       (let ((uri (symbol2uri (car form)))
                                             (obj (addClass rdfs:Class (car form) ())))
                                         (when uri (setf (iri-value uri) obj))
                                         obj))))
                             (slots (aggregate-slots (cdr form)))
                             (obj nil))
                         ;(format t "~%Class:~S" class)
                         (cond ((and (assoc 'owl:oneOf slots) (assoc :name slots))
                                (setq obj
                                      (%addForm class (mapcar #'form2slot (remove (assoc 'owl:oneOf slots) slots)) role))
                                ;; obj = class of oneOfs, rule7
                                (loop for one in (cdr (assoc 'owl:oneOf slots))
                                    do (addForm
                                        (cond ((consp one)                                    ; maybe a form
                                               (cond ((eq (car one) 'owl:Thing)
                                                      (cons (name obj) (cdr one)))
                                                     (t (append one `((rdf:type ,(name obj)))))))
                                              ((symbolp one) `(,(name obj) (:name ,one)))     ; maybe a name
                                              (t `(,(name obj) ,one))                         ; object ?
                                              )))
                                (%addForm class (mapcar #'form2slot slots) role))
                               (t (setq obj (%addForm class (mapcar #'form2slot slots) role))))
                         (assert obj)
                         (setf (type-tag obj) (car form))
                         (let* ((name (name obj))
                                (uri (when (and name (symbolp name)) (symbol2uri name))))
                           (when uri (setf (iri-value uri) obj)))
                         obj))))))))

(defun form2slot (slot-form)
  "accepts a <slot-form> and evaluates the filler using <addForm> and returns a slot, namely it makes a filler object 
   or data, and returnes a list of role and filler.
   when a slot-form is a non-nil symbol, it should be a name of resource
   and a name slot is created and returned."
  (when (not (consp slot-form))
    (setq slot-form `(:name ,slot-form)))
  (let ((role (slot-role slot-form))
        (forms (slot-forms slot-form)))
    (case role 
      (:name
       (push `(,(car forms) line ,*line-number*) *referenced-resources*)
       slot-form)
      (rdf:about
       (let ((*uri2symbol-package-mapping-fun* #'excl::false)
             (*uri2symbol-name-mapping-fun* #'excl::false)
             (about (iri (car forms))))
         (let ((symbol (uri2symbol about)))
           (when symbol
             (push `(,symbol line ,*line-number*) *referenced-resources*)))
         `(rdf:about ,about)))
      (rdf:ID
       (let ((*uri2symbol-package-mapping-fun* #'excl::false)
             (*uri2symbol-name-mapping-fun* #'excl::false))
         (let ((symbol (car forms)))
           (when symbol
             (push `(,symbol line ,*line-number*) *referenced-resources*))))
       slot-form)
      (xml:lang
       (setq lang-env (intern (string-downcase (string (car forms))) :keyword))
       `(xml:lang ,lang-env))
      (otherwise
       (cons role
             ;; null filler happens.
             (mapcar #'(lambda (form) (addForm form role)) forms))))))

(defun make-object-with-minimal-constraint (name role)
  "This function returns a resource entity (object) of which class is 
   suggested by range constraint of <role> or several entailment rules."
  (if (property? role)
      (let ((range (get-range-constraint-from role)))
        (cond (range
               (cond ((subproperty-p (symbol-value role) rdfs:subClassOf)
                      (cond ((not (object? name))
                             (warn "Range entailX1 by ~S: ~S rdf:type ~S." role name (name range))
                             (addObject range `((:name ,name))))
                            ((not (cl:typep (symbol-value name) range))
                             (warn "Range entailX1 by ~S: ~S rdf:type ~S." role name (name range))
                             (change-class (symbol-value name) range))
                            (t (symbol-value name))))
                     ((subproperty-p (symbol-value role) rdf:type)
                      (cond ((not (object? name))
                             (warn "Range entailX2 by ~S: ~S rdf:type ~S." role name (name range))
                             (addObject range `((:name ,name))))
                            ((not (cl:typep (symbol-value name) range))
                             (warn "Range entailX2 by ~S: ~S rdf:type ~S." role name (name range))
                             (change-class (symbol-value name) range))
                            (t (symbol-value name))))
                     ((not (object? name))
                      ;;(error "Check it!")
                      (warn "Range entailX3 by ~S: ~S rdf:type ~S." role name (name range))
                      (addObject range `((:name ,name))))
                     ((not (cl:typep (symbol-value name) range))
                      (error "Check it!")
                      (warn "Range entailX3 by ~S: ~S rdf:type ~S." role name (name range))
                      (change-class (symbol-value name) range))
                     (t (symbol-value name))))
              (t ;(warn "Rdfs4b: ~S rdf:type rdfs:Resource." name)
                 (make-instance '|rdfs:Resource| :name name))))
    (progn ;(warn "Rdfs4b: ~S rdf:type rdfs:Resource." name)
      (make-instance '|rdfs:Resource| :name name))))

(defun ensure-object-with-minimal-constraint (obj role)
  "returns an <obj> that satisfies constraints by <role>."
  (if (property? role)
      (let ((range (get-range-constraint-from role)))
        (cond ((null range) obj)
              ((atom range)
               (cond ((eq obj rdfs:Class) obj)
                     ((cl:typep obj range) obj)
                     (t (warn "Range entailX4 by ~S: ~S rdf:type ~S." role obj (name range))
                        (change-class obj range))))
              ((consp range)
               (cond ((every #'(lambda (cls) (cl:typep obj cls)) range) obj)
                     (t (warn "Range entailX5 by ~S: ~S rdf:type ~S." role obj (name range))
                        (change-class obj range))))
              ((error "Cant happen!"))))
    obj))

(defun aggregate-slots (slots)
  "This function collects fillers in several slots on an identical role and makes them in one slot."
  (let ((roles (loop for slot in slots collect (slot-role slot))))
    (setq roles (remove-duplicates roles))
    (setq slots
          (loop for role in roles
              collect (cons role
                            (loop for slot in slots
                                when (eql role (slot-role slot))
                                append (slot-forms slot)))))))

(defun get-range-constraint-from (role)
  "returns range constraint of <role> if exists, otherwise returns nil. In case that 
   <role> is rdf:List, this function returns an appropriate class for the element of list.
   See also <get-range-constraint-from> in OWL module."
  (case role
    ((nil) nil)
    ((t) nil)
    (otherwise 
     (when (boundp role)
       (let ((range (get-range (symbol-value role))))
         (if (eql range rdf:List) rdfs:Resource range))))))

;;;
;;;; %addForm for RDF
;;; <%addForm> is a set of methods that are dedicated to each type of RDF entity.
;;; These methods are called with instantiated slots and returns an instance object of <type>.
;;;
;;; Calling sequence: %addForm (<type> <slots> <role>)
;;; * <slots> - aggregated slots such as ((<role>1 <filler>11 <filler>12 ...) (<role>2 <filler>21 <filler>22 ...) ...), 
;;;   where every <role>-n is a symbol and every <filler>-n is a designated value object.
;;; * <role>  - a role symbol that plays the rage constraint for (<role> <form>).
;;; * <type>  - a type of entity to be made.
;;; Main purpose of these methods as a whole is to decide the type of object before the object creation.
;;; In most of cases, the most specific concept (MSC) among domain constraints from roles that are included in <slots>,
;;; a range constraint of pair <role> in slot of upper nests, and rdf:type filler in <slots> is computed and used.
;;; If any roles in <slots> are not defined, they are tentatively defined as instance of rdf:Property. In case that,
;;; * %addForm((eql 'rdf:Description)) - If MSC from constraints exists it is used, otherwise the value of 
;;;   *top*(=rdfs:Resource) is <type>.
;;; * %addForm((eql rdfs:Class)) - If MSC is more special than rdfs:Class, 
;;;   it is used otherwise rdfs:Class is used for <type>.
;;; * %addForm((eql owl:Class)) - If MSC is more special than owl:Class, 
;;;   it is used otherwise owl:Class is used for <type>.
;;; * %addForm((eql rdfs:Resource)) - If MSC is more special than rdfs:Resource, 
;;;   it is used otherwise rdfs:Resource is used for <type>.
;;; * %addForm((eql owl:Thing)) - If MSC exists, it is used, else owl:Thing is used for <type>.
;;; * %addForm((eql owl:Restriction) t t) - MSC is used for type. Note that more special 
;;;   restrictions than owl:Restriction are computed from role domain constraints. For exmaple, 
;;;   owl:allValuesFrom's domain is set to owl:allValuedFromRestriction.
;;; * %addForm(rdfsClass) - Error. Never happen because %addForm((eql rdfs:Class)) supersedes this.
;;; * %addForm(rdfs:Class) - Indicated type is used.
;;; * %addForm(rdfs:Resource) - An instance is indicated for type. After changing it to a class, 
;;;   it or MSC is used.
;;; * %addForm(symbol) - If the type is already defined, recurse with the symbol value. 
;;;   Otherwise it is defined by metaclass that is computed from the MSC.
;;; In short, if a more special class than rdfs:Class is indicated for <type>, it is used 
;;; whether or not more special MSC exists. Otherwise, MSC is computed and it is used.
;;;
;;; See the following example. Here Species and EndangeredSpecies are defined as metaclass. 
;;; ----------------------------------------------------------------------------------
;;; (addForm
;;;   '(rdfs:Class Species
;;;      (rdfs:subClassOf rdfs:Class)        ; this makes Species a metaclass
;;;      (rdfs:comment "This example is for the demonstration of addForm.")))
;;; (addForm
;;;   '(rdfs:Class EndangeredSpecies
;;;      (rdfs:subClassOf Species)))         ; a subclass of metaclass is a metaclass
;;; (addForm
;;;   '(rdf:Property estimatedPopulation
;;;      (rdfs:domain EndangeredSpecies)
;;;      (rdfs:range xsd:nonNegativeInteger)))
;;; (addForm
;;;   '(rdfs:Class Hawk
;;;      (estimatedPopulation 2000)))         ; MSC is computed as EndangeredSpecies
;;; (addForm '(Hawk Harry))
;;; ----------------------------------------------------------------------------------

(defun %addForm (type slots role)
  "subfunction for <%addForm>. To be here, <type> must be fixed.
   This function creates an object with <type> and <slots> using <addObject>."
  (let ((types (most-specific-concepts
                (reverse
                 (cons type
                       (append (mklist (get-range-constraint-from role))
                               (remove rdf:List (collect-domains (mapcar #'slot-role slots)))
                               (cdr (find-if #'(lambda (role) 
                                                 (and (property? role) (subproperty-p (symbol-value role) rdf:type)))
                                             slots :key #'slot-role))))))))
    (cond ((null types) (setq type (symbol-value '|rdfs:Resource|)))
          ((length=1 types) (setq type (car types)))
          (t ;(warn "There are many independent types ~S for ~S." type slots)
           ;(setq type (make-proxy types))
           )))
  (setq slots (remove-if #'keywordp slots))
  (setq slots (remove 'xml:lang slots :key #'slot-role))
  (cond ((null slots)
         (addObject type nil))
        ((atom (car slots))  ; should be name
         (error "Cant happen!"))
        ((assoc :name slots)
         (when (or (eq role t) (eq role nil)) ;; top level input
           (accumulate-defined-name (assoc :name slots)))
         (addObject type slots))
        ((and (symbolp type) (string= (string type) "Ontology"))
         (let ((name (name-ontology (slot-filler (assoc 'rdf:about slots)))))
           (addObject type (acons :name (list name) slots))))
        ((assoc 'rdf:about slots)
         ;; if rdf:about attribute exists and no name, then name is set from rdf:about attribute.
         (let ((uri (slot-filler (assoc 'rdf:about slots))))
           (let ((name (unless (assoc :name slots) (uri2symbol uri))))
             (when (or (eq role t) (eq role nil)) ;; top level input
               (accumulate-defined-name name))
             (addObject type (acons :name (list name) slots)))))
        ((assoc 'rdf:ID slots)
         (let ((name (slot-filler (assoc 'rdf:ID slots))))
           (when (or (eq role t) (eq role nil)) ;; top level input
             (accumulate-defined-name name))
           (addObject type (acons :name (list name) slots))))
        (t  ;; nil suppresses domain collec computing
         (addObject type slots)))
  )

(defun make-proxy (types)
  (let* ((metas
          (most-specific-concepts
           (loop for type in types when (rdf-metaclass-p (class-of type)) append (mclasses type))))
         (meta
          (cond ((null metas) rdfs:Class)
                ((length=1 metas) (car metas))
                (t (error "Not Yet!")))))
    (addClass meta (make-coined-name types) types)))

(defun make-coined-name (types)
  (setq types (remove-if #'anonymous-p types))
  (when (length=1 types) (return-from make-coined-name (name (car types))))
  (setq types (remove-if #'(lambda (ty) (eql (symbol-package (name ty)) (find-package :rdf)))
                         types))
  (when (length=1 types) (return-from make-coined-name (name (car types))))
  (setq types (remove-if #'(lambda (ty) (eql (symbol-package (name ty)) (find-package :rdfs)))
                         types))
  (when (length=1 types) (return-from make-coined-name (name (car types))))
  (let ((types-1 
         (remove-if #'(lambda (ty) (eql (symbol-package (name ty)) (find-package :owl)))
                    types)))
    (when (length=1 types-1) (return-from make-coined-name (name (car types-1))))
    (let ((namelist (mapcar #'name (or types-1 types))))
      (let ((namestr (format nil "~A~{_~A~}" (car namelist) (cdr namelist))))
        (intern namestr (symbol-package (car namelist)))))))

(defun accumulate-defined-name (name)
  "This function is called at top level of input form and accumulate the name of entity 
   as defined name into <*defined-resources*>."
  (cond ((consp name)
         (when (eq (car name) :name)
           (push `(,(second name) line ,*line-number*) *defined-resources*)))
        ((null name))
        ((symbolp name) (push `(,name line ,*line-number*) *defined-resources*))
        ((rsc-object-p name) (push `(,(name name) line ,*line-number*) *defined-resources*))
        ((error "Cant happen!"))))

;;;
;;;; addObject
;;;
;;; <addObject>s are a set of methods of which main purpose is to distinguish creating a class or 
;;; creating an instance, and additionally recognize an instance of OneOf. See also <addObject> in OWL module. 
;;;
;;; # <addObject>(rdfs:Class) - main routine
;;; # <addObject>(rdfsClass)  - accepts only rdfs:Class, the body is same as the main routine above.
;;; # <addObject>:around(rdfs:Class) - for OneOf processing, see OneOf module
;;; # <addObject>:around(rdfsClass)  - same as <addObject>:around(rdfs:Class)
;;; # <addObject>:before(rdfs:Class) - for container membership property
;;; Note that if there is no information on <type> or superclasses of an object, due to the forward reference, 
;;; the object is created as instance even though it is changed to a class later when the regular expression
;;; are stated.

(defun addObject (type slot-forms)
  "<type> is a class or a meta-class except rdfs:Class. Every slot-filler is already objectized if it is an resource object.
   This method sets up QName's package in uri-namedspace from name in <slot-forms>, then calls <ensure-meta-absts> 
   to fix the meta class and abst classes for this object. Finally, calls <addClass> if the meta class or abst exists, else 
   calls <addInstance>. If optional <domains> is not supplied, the domain constrant is computed and it is used for ensuring 
   the metaclass and abst classes. To suppress domain computing, supply nil."
  (when (cl:subtypep type 'rdfs:Container) ;type = rdf:Alt,rdf:Seq,rdf:Bag, etc.
    (check-ordinal-properties slot-forms))
  (let ((name (second (assoc ':name slot-forms))))
    (setq slot-forms (remove (assoc ':name slot-forms) slot-forms :test #'eq))
    (multiple-value-bind (metas absts)
        (ensure-meta-absts type slot-forms nil)
      (when (and (member type metas) (not (eql type (car metas))))
        (setq metas (cons type (remove type metas))))
      ; anyway make slots under this type
      (cond ((rdf-metaclass-p (car metas))
             (addClass metas name absts slot-forms))
            (absts (error "Cant happen!"))
            (t (addInstance metas name slot-forms))))))

;;;
;;;; Ensuring Meta Classes and Abst(Super) Classes
;;; 

(defun ensure-meta-absts (class slot-forms domains)
  "picks up metaclasses and superclasses from <slot-forms>. If rdf:type slot is included in <slot-forms>,
   the range constraint supplies meta class(es). If rdfs:subClassOf slot is included, the range constraint 
   supplies superclasses (absts)."
  (ensure-meta-absts-using-class (class-of class) class slot-forms domains)
  )

(defmethod ensure-meta-absts-using-class (meta class slot-forms domains)
  (error "Can't happen!"))
(defmethod ensure-meta-absts-using-class ((meta rdfs:Class) (class rdfs:Resource) slot-forms domains)
  "<class> must be an instance."
  (error "Cant happen!"))

(defmethod ensure-meta-absts-using-class ((meta rdf-node) (class rdfsClass) slot-forms domains)
  "<class> must be rdfs:Class"
  (let ((types (mappend #'cdr
                        (remove-if-not #'(lambda (slot)
                                           (let ((role (slot-role slot)))
                                             (or (eq role 'rdf:type)
                                                 (and (boundp role) (symbol-value role)
                                                      (subproperty-p (symbol-value role) rdf:type)))))
                                       slot-forms)))
        (absts (mappend #'cdr
                        (remove-if-not #'(lambda (slot)
                                           (let ((role (slot-role slot)))
                                             (or (eq role 'rdfs:subClassOf)
                                                 (eq role 'owl:intersectionOf)
                                                 (and (boundp role) (symbol-value role)
                                                      (or (and (not (eq role 'owl:equivalentClass))
                                                               (subproperty-p (symbol-value role) rdfs:subClassOf))
                                                          (and (boundp 'owl:intersectionOf)
                                                               (subproperty-p (symbol-value role) (symbol-value 'owl:intersectionOf))))))))
                                       slot-forms))))
    ;; ensure absts
    (mapc #'(lambda (abst)
              (unless (rdf-class-p abst)
                ;; change to class
                (change-class abst *meta*)
                ))
      (setq absts (remove-duplicates absts)))
    (unless (length=1 absts)
      (setq absts (most-specific-concepts-by-superclasses (reverse absts))))
    ;; ensure types
    ;; note that types must be same level as class
    (mapc #'(lambda (typ)
              (cond ((and (rdf-metaclass-p class) (rdf-metaclass-p typ)))
                    ((and (rdf-metaclass-p class) (strict-class-p typ))
                     ;; change to metaclass
                     (reinitialize-instance
                      typ
                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses typ))))
                    ((and (rdf-metaclass-p class) (rdf-instance-p typ))
                     (cerror "Anyway change it to class ?" "~S is not metaclass!" (class-of typ))
                     (reinitialize-instance
                      (class-of typ)
                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses (class-of typ)))))
                    ((error "Cant happen!"))))
      (setq types (remove-duplicates types)))
    (let ((newmetas (adjoin class (union types domains))))
      (setq types
            (if (length=1 newmetas) newmetas
              (most-specific-concepts-by-superclasses (reverse newmetas)))))
    (values (refine-concept-by-intersection types)
            absts)))

(defmethod ensure-meta-absts-using-class ((meta rdfs:Class) (class rdfs:Class) slot-forms domains)
  "<class> must be a class or metaclass."
  (let ((types (mappend #'cdr
                        (remove-if-not #'(lambda (slot)
                                           (let ((role (slot-role slot)))
                                             (or (eq role 'rdf:type)
                                                 (and (boundp role) (symbol-value role)
                                                      (subproperty-p (symbol-value role) rdf:type)))))
                                       slot-forms)))
        (absts (mappend #'cdr
                        (remove-if-not #'(lambda (slot)
                                           (let ((role (slot-role slot)))
                                             (or (eq role 'rdfs:subClassOf)
                                                 (eq role 'owl:intersectionOf)
                                                 (and (boundp role) (symbol-value role)
                                                      (or (and (not (eq role 'owl:equivalentClass))
                                                               (subproperty-p (symbol-value role) rdfs:subClassOf))
                                                          (and (boundp 'owl:intersectionOf)
                                                               (subproperty-p (symbol-value role) (symbol-value 'owl:intersectionOf))))))))
                                       slot-forms))))
    ;; ensure absts
    (mapc #'(lambda (abst)
              (unless (rdf-class-p abst)
                ;; change to class
                (change-class abst *meta*)
                ))
      (setq absts (remove-duplicates absts)))
    (unless (length=1 absts)
      (setq absts (most-specific-concepts-by-superclasses (reverse absts))))
    ;; ensure types
    ;; note that types must be same level as class
    (mapc #'(lambda (typ)
              (cond ((and (rdf-metaclass-p class) (rdf-metaclass-p typ)))
                    ((and (strict-class-p class) (strict-class-p typ)))
                    ((and (rdf-metaclass-p class) (strict-class-p typ))
                     ;; change to metaclass
                     (reinitialize-instance
                      typ
                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses typ))))
                    ((and (strict-class-p class) (rdf-metaclass-p typ))
                     (error "Check it!"))
                    ((and (strict-class-p class) (rdf-instance-p typ))
                     (change-class typ (class-of class)))
                    ((and (rdf-metaclass-p class) (rdf-instance-p typ))
                     (cerror "Anyway change it to class ?" "~S is not metaclass!" (class-of typ))
                     (reinitialize-instance
                      (class-of typ)
                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses (class-of typ)))))
                    ((error "Cant happen!"))))
      (setq types (remove-duplicates types)))
    (let ((newmetas (adjoin class (union types domains))))
      (setq types
            (if (length=1 newmetas) newmetas
              (most-specific-concepts-by-superclasses (reverse newmetas)))))
    (values (refine-concept-by-intersection types)
            absts)))

(defun refine-concept-by-intersection (classes)
  "hook for OWL"
  classes)

#|
(defmethod ensure-meta-absts-using-class ((meta rdfs:Class) (class rdfs:Class) slot-forms domains)
  (flet ((ensure-meta (fil) (cond ((rdf-metaclass-p fil) fil)
                                  ((rdf-class-p fil)
                                   (reinitialize-instance
                                    fil
                                    :direct-superclasses (cons *meta* (mop:class-direct-superclasses fil))))
                                  (t (cerror "Anyway metaclassify ~S?" "~S is not metaclass!" (class-of fil))
                                     (reinitialize-instance
                                      (class-of fil)
                                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses (class-of fil))))
                                     fil)))
         (ensure-abst (fil) (cond ((rdf-class-p fil) fil)
                                  (t (cerror "Anyway metaclassify ~S?" "~S is not metaclass!" (class-of fil))
                                     (reinitialize-instance
                                      (class-of fil)
                                      :direct-superclasses (cons *meta* (mop:class-direct-superclasses (class-of fil))))
                                     fil))))
    (let ((absts nil)
          (metas nil))
      (loop for (role . fillers) in slot-forms
          when (or (eq role 'rdfs:subClassOf)
                   (eq role 'owl:intersectionOf)
                   (and (boundp role) (symbol-value role)
                        (or (and (not (eq role 'owl:equivalentClass))
                                 (subproperty-p (symbol-value role) rdfs:subClassOf))
                            (and (boundp 'owl:intersectionOf)
                                 (subproperty-p (symbol-value role) (symbol-value 'owl:intersectionOf))))))
          do (setq absts (union absts (mapcar #'ensure-abst fillers))))
      (loop for (role . fillers) in slot-forms
          when (or (eq role 'rdf:type)
                   (and (boundp role) (symbol-value role)
                        (subproperty-p (symbol-value role) rdf:type)))
          do (setq metas (union metas fillers)))
      (when (and absts (notany #'rdf-metaclass-p metas))
        (loop for meta in metas do (ensure-meta meta)))
      (unless (length=1 absts)
        (setq absts (most-specific-concepts-by-superclasses (reverse absts))))
      (let ((newmetas (adjoin class (union metas domains))))
        (setq metas
              (if (length=1 newmetas) newmetas
                (most-specific-concepts-by-superclasses (reverse newmetas)))))
      (values metas absts))))
|#

;;;
;;;; addClass
;;;
;;; Principles in multiple typing
;;;  # The association between a type and slots attached to it should be given by user, except the case that a slot states its domain. 
;;;  # Even if slots is defined in multiple classes that relates in super/sub class relation, system does not care of the redundancy of slots.

(defvar *subjects-defined* nil "storage where all subjects defined are stored")

(defun addClass (classes class absts &optional islots)
  "create a class of metaclass <classes> with <class>, <absts>, and <islots>.
   Each of <classes> must be an object. An element of <islots> is '(<role> . <fillers>)'.
   Note that <islots> are not slot definition for class but slots for this class.
   <absts> are already entailed and objectized. There is no undefined property in slots. 
   Slot definitions for <islots> are automatically defined by system, if 
   not defined yet. This function can process multiple different typing for existing objects.
   If any element in <classes> is a class but not a mete class, it is changed to a meta class.
   This function stores the result object into <*subjects-defined*>."
  (setq classes (mklist classes))
  (let ((name (etypecase class
                (symbol class)
                (net.uri:uri (uri2symbol class))
                (rdfs:Resource (name class))))
        (uri (etypecase class
               (symbol (symbol2uri class))
               (net.uri:uri class)
               (rdfs:Resource (iri class))))
        (obj (etypecase class
               (symbol (and (boundp class) (symbol-value class)))
               (net.uri:uri (iri-value class))
               (rdfs:Resource class)))
        (initargs
         (loop for slot in islots
             append (let ((role (slot-role slot))
                          (fillers (slot-forms slot)))
                      (cond ((null fillers) (list role :unbound))
                            ((eq role 'rdf:about) slot)
                            ((eq role 'rdf:ID) slot)
                            ((property? role)
                             (cond ((eq (get-range (symbol-value role)) rdf:List)
                                    (list role (mklist fillers)))
                                   ((cdr fillers)
                                    (list role fillers))
                                   (t slot)))
                            (t (warn "Entail by rdfs1: ~S rdf:type rdf:Property." role)
                               (make-instance 'rdf:Property :name role)
                               (cond ((cdr fillers) (list role fillers))
                                     (t slot))))))))
    ;(format t "~%Name:~S~%URL:~S~%CLS:~S~%Initargs:~S" name uri obj initargs)
    (flet ((ensure-multiple-classes (metas cls)
                                    (cond ((null (cdr metas)) cls)
                                          (t (let ((shadow (make-shadow (class-of cls) metas)))
                                               (cond ((eql (class-of cls) shadow) cls)
                                                     (t (warn "Multiple classing with ~S for ~S" metas cls)
                                                        (change-class cls shadow)))))))
           (set-iri-value (obj)
                          "binds <obj> to obj's uri of rdf:ID or rdf:about attribute value, 
              and push <obj> into *subjects-defined*"
                          (with-slots (rdf:about rdf:ID) obj
                            (cond ((slot-boundp obj 'rdf:about) (setf (iri-value (iri rdf:about)) obj))
                                  ((slot-boundp obj 'rdf:ID) (setf (iri-value (symbol2uri rdf:ID)) obj))))
                          (pushnew obj *subjects-defined*)
                          obj))
      (when (and name (not uri) (documentation (symbol-package name) t))
        (setf (uri-namedspace-package 
               (set-uri-namedspace (documentation (symbol-package name) t)))
          (symbol-package name)))
      (cond (obj                                                  ; redefining
             ;; redefinition in either same or different class is processed in ensure-class
             ;; :direct-superclasses and :direct-slots is added into old ones 
             ;; by shared-initialize :around (rdfs:Class)
             ;(format t "~%Adding Class by redefinition classes:~S~%   name:~S~%   absts:~S~%   slots:~S" classes name absts islots)
             (cond ((every #'(lambda (cls) (cl:typep obj cls)) classes)
                    ;(format t "~%~S is already an instance of ~S." obj classes)
                    (cond ((every #'(lambda (abst) (cl:subtypep obj abst)) absts)
                           (cond ((every #'(lambda (slot)
                                             (and (slot-boundp obj (slot-role slot))
                                                  (owl-equalp (mkatom (slot-forms slot))
                                                              (slot-value obj (slot-role slot)))))
                                         islots)
                                  ;; nothing done and return obj
                                  obj)
                                 (t ;(format t "~%Adding Class by redefinition1 ~S ~S ~S ~S" classes obj absts islots)
                                  (apply #'reinitialize-instance obj initargs))))
                          (t ;(format t "~%Adding Class by redefinition2 ~S ~S ~S ~S" classes obj absts islots)
                             (apply #'reinitialize-instance obj :direct-superclasses absts initargs))))
                   ((%instance-p obj)                          ; instance to class
                    ;(format t "~%Change class1 of instance:~S to class:~S" obj (name (car classes)))
                    (change-class obj (car classes))           ; change instance to class
                    (when name (setf (find-class name) obj))
                    (ensure-multiple-classes
                     classes
                     (apply #'mop:ensure-class-using-class obj name
                            :direct-superclasses absts
                            :metaclass (car classes)
                            initargs)))
                   ((rdf-metaclass-p (car classes))   ; class to class
                    ;(format t "~%Adding Class in redefinition ~S ~S ~S ~S" classes name absts islots)
                    (cond ((and (null absts) (null initargs))
                           (cond ((subtypep (class-of obj) (car classes)) ; only check multiple classing
                                  (ensure-multiple-classes classes obj))
                                 ((subtypep (car classes) (class-of obj)) ; change metaclass
                                  ;(format t "~%Change metaclass of class:~S to ~S" obj classes)
                                  (ensure-multiple-classes
                                   classes (change-class obj (car classes))))
                                 (t (warn "(~S type ~S) directed but it violates monotonicity, then no effect."
                                      obj (car classes)))))
                          ((and name absts)
                           (ensure-multiple-classes
                            classes
                            (apply #'mop:ensure-class name
                                   :direct-superclasses absts
                                   :metaclass (car classes)
                                   initargs)))
                          ((and name (null absts))
                           (ensure-multiple-classes
                            classes
                            (apply #'mop:ensure-class name
                                   :metaclass (car classes)
                                   initargs)))
                          ((and (null name) absts)
                           (ensure-multiple-classes
                            classes
                            (apply #'mop:ensure-class-using-class obj nil
                                   :direct-superclasses absts
                                   :metaclass (car classes)
                                   initargs)))
                          ((and (null name) (null absts))
                           (ensure-multiple-classes
                            classes
                            (apply #'mop:ensure-class-using-class obj nil
                                   :metaclass (car classes)
                                   initargs)))
                          ((error "cant happen!"))))
                   ((error "cant happen!"))))
            ((and name (null obj))                  ; new name or
             (when (boundp name)                    ; used name
               (warn "~&Bounded symbol ~S is dispatched for a resource." name))
             (set-iri-value
              (ensure-multiple-classes
               classes
               (if absts
                   (apply #'mop:ensure-class name
                          :direct-superclasses absts
                          :metaclass (car classes)
                          initargs)
                 (apply #'mop:ensure-class name
                        :metaclass (car classes)
                        initargs)))))
            ((null name)                                        ; anonymous
             ;; anonymous usually create new metaobjects
             (set-iri-value
              (ensure-multiple-classes
               classes
               (if absts
                   (apply #'mop:ensure-class-using-class nil nil
                          :direct-superclasses absts
                          :metaclass (car classes)
                          initargs)
                 (apply #'mop:ensure-class-using-class nil nil
                        :metaclass (car classes)
                        initargs)))))
            ))))

;;;
;;;; addInstance
;;;

(defun addInstance (classes instance &optional slots)
  "creates a new instance or redefines the instance with <instance> and <slots>. 
   <instance> must be a symbol, a URI, or resource object.
   Each of <classes> must be defined and <classes> must be MSCs. 
   An element of <slots> is '(<role> . <fillers>)'.
   In new creation, the car of <classes> is used for class of this instance. If there are multiple classes, 
   a shadow class for <classes> is made based the car of <classes>. In redefinition, 
   * if the old instance belongs to everyone in <classes>, <reinitialize-instance> method is invoked.
   * if a single new class is indicated, <change-class> is invoked.
   * if every new class is a subclass of some of old classes, then <change-class> is invoked, and shadowes for multiple classes.
   * otherwise, the MSCs are computed and <change-class> and shadowing are done.
   This function stores the result object into <*subjects-defined*>."
  (assert (not (null classes)))
  (setq classes (mklist classes))
  (let ((name (etypecase instance
                (symbol instance)
                (net.uri:uri (uri2symbol instance))
                (rdfs:Resource (name instance))))
        (uri (etypecase instance
                (symbol (symbol2uri instance))
                (net.uri:uri instance)
                (rdfs:Resource (iri instance))))
        (obj (etypecase instance
               (symbol (and (boundp instance) (symbol-value instance)))
               (net.uri:uri (iri-value instance))
               (rdfs:Resource instance)))
        (initargs
         (loop for slot in slots
             append (let ((role (slot-role slot))
                          (fillers (slot-forms slot)))
                      (cond ((null fillers) (list role :unbound))
                            ((eq role 'rdf:about) slot)
                            ((eq role 'rdf:ID) slot)
                            ((property? role)
                             (cond ((eq (get-range (symbol-value role)) rdf:List)
                                    (list role (mklist fillers)))
                                   ((cdr fillers)
                                    (list role fillers))
                                   (t slot)))
                            (t (warn "Entail by rdfs1: ~S rdf:type rdf:Property." role)
                               (make-instance 'rdf:Property :name role)
                               (cond ((cdr fillers) (list role fillers))
                                     (t slot))))))))
    ;(format t "~%Initargs: ~S" initargs)
    ;; Despite what multiple classes are, slots must be putted at this object.
    ;; The question is which class the slot difinition must be added to.
    ;; (car classes) is nominal one for multiple classes. Therefore, we put new slots there.
    ;; In that case, one problem is wasteful slot-definitions if slots are attached 
    ;; to upper classes afterward. 
    (when (and name (not uri) (documentation (symbol-package name) t))
      (setf (uri-namedspace-package 
             (set-uri-namedspace (documentation (symbol-package name) t)))
        (symbol-package name)))
    (flet ((ensure-multiple-classes (classes obj)
                                    (cond ((null (cdr classes)) obj)
                                          (t ;(format t "~%ENSUREING ~S ~S" classes obj)
                                           (let ((shadow (make-shadow (class-of obj) classes)))
                                             (cond ((eql (class-of obj) shadow) obj)
                                                   (t (warn "Multiple classing with ~S for ~S" classes obj)
                                                      (change-class obj shadow)))))))
           (set-iri-value (obj)
                          (with-slots (rdf:about rdf:ID) obj
                            (cond ((slot-boundp obj 'rdf:about) (setf (iri-value (iri rdf:about)) obj))
                                  ((slot-boundp obj 'rdf:ID) (setf (iri-value (symbol2uri rdf:ID)) obj))))
                          (pushnew obj *subjects-defined*)
                          obj))
      (cond ((and (null slots) (eq (car classes) (class-of obj)))
             (ensure-multiple-classes classes obj))
            (obj                                                   ; redefine
             (format t "~%Redefining ~S with classes:~S~%    slots:~S in addInstance" name classes slots)
             ;; If name is a blank node ID, definitely it is bound to an default object.
             ;; Therefore the control always falls here, and the object is anonymous.
             (let ((mclasses (substitute rdfs:Resource |rdfs:Resource| (mclasses obj))))
               (cond ((every #'(lambda (cls) (cl:typep obj cls)) classes)
                      (when initargs
                        (format t "~%Reinitializing ~S with ~S" obj initargs)
                        (apply #'reinitialize-instance obj initargs))
                      obj)
                     ((null (cdr classes)) ; different single class, maybe forward ref to regular def
                      (let ((newcls (car classes)))
                        (cond ((eq newcls |rdfs:Resource|)
                               (when initargs (apply #'reinitialize-instance obj initargs)))
                              ((every #'(lambda (old) (cl:subtypep old newcls)) mclasses)
                                ) ; nothing done
                              ((every #'(lambda (old) (cl:subtypep newcls old)) mclasses)
                               (format t "~%Changing a class of ~S to ~S." obj newcls)
                               (apply #'change-class obj newcls initargs))
                              (t (let ((shadow (make-shadow (class-of obj)
                                                            (most-specific-concepts (append mclasses classes)))))
                                   (format t "~%Shadowed class ~S from ~S." shadow (class-of obj))
                                   (apply #'change-class obj shadow initargs))
                                 )))
                      (when initargs (apply #'reinitialize-instance obj initargs))
                      obj)
                     ((null (set-difference classes mclasses
                                            :test #'(lambda (x y) (cl:subtypep x y))))
                      ;; refining from old classes to new classes
                      (apply #'change-class obj (car classes) initargs)
                      (when initargs (apply #'reinitialize-instance obj initargs))
                      (ensure-multiple-classes classes obj))
                     (t ;; for pizza ontology isBase in Functional and InverseFunctional
                      (let ((new-classes (most-specific-concepts (append classes mclasses))))
                        ;(format t "~%Old-classes ~S" mclasses)
                        ;(format t "~%New-classes ~S" new-classes)
                        (warn "Redefine ~S to independent multiple classes ~S"
                          (symbol-value name) (mapcar #'name classes))
                        (let ((shadow (make-shadow (class-of (symbol-value name)) new-classes)))
                          (apply #'change-class (symbol-value name) shadow initargs)
                          (when initargs
                            (apply #'reinitialize-instance (symbol-value name) initargs))
                          (symbol-value name)
                          ))))))
            ((and name (null obj) (not (boundp name)))  ; new name and may be ensured to multiple classes.
             ;(format t "~%NEW NAME ~S" name)
             (set-iri-value
              (ensure-multiple-classes
               classes (apply #'make-instance (car classes) :name name initargs))))
            ((and name (null obj) (boundp name))                          ; used name
             (warn "~&Bounded symbol ~S is dispatched for a resource." name)
             (set-iri-value
              (ensure-multiple-classes
               classes (apply #'make-instance (car classes) :name name initargs))))
            ((null name)                   ; anonymous but may be ensured to multiple classes
             (set-iri-value
              (ensure-multiple-classes
               classes (apply #'make-instance (car classes) initargs))))
            ))))

;;;
;;;; Make Instance
;;;

(defun ensure-class-slotds (class initargs)
  "checks properties in <initargs> and adds the new slot definitions to <class>.
   Note that initargs are for an instance of <class>, not for <class>."
  (let ((properties (collect-props-from-initargs initargs)))
    (when properties
      (unless (mop:class-finalized-p class) (mop:finalize-inheritance class))
      (let ((slot-names (collect-prop-names-from class))
            (new-props nil)
            (operated nil))
        (when (setq new-props (set-difference properties slot-names :test #'eq))
          (loop for prop in new-props with new-domain
              when (property? prop)
              do (reinitialize-instance
                  class
                  :direct-slots
                  `((:name ,prop :initargs (,prop) :type ,(or (get-range (symbol-value prop)) t)
                           :documentation "By ensuring this class slot definition"
                           :subject-type ,class)))
                (setq operated t))
          (when operated (mop:finalize-inheritance class)))))))

(defmethod make-instance :before ((class rdfs:Class) &rest initargs)
  (when initargs
    (ensure-class-slotds class initargs)))

(defun collect-domains-from-initargs (class initargs)
  " returns a list of domains for <class>."
  (let ((properties (collect-props-from-initargs initargs)))
    (when properties
      (let ((domains nil))
        (loop for prop in properties with domain
            when (property? prop)
            do (when (setq domain (get-domain (symbol-value prop)))
                 (cond ((consp domain)
                        (setq domains
                              (union (reverse (remove rdf:List domain)) domains)))
                       ((not (eql domain rdf:List))
                        (pushnew domain domains)))))
        domains))))
    
(defmethod make-instance ((class rdfs:Class) &rest initargs)
  "This method accepts any class and metaclass including rdfs:Class.
   Before calling the primary method, ensure slot definitions for this <class> for initargs.
   Domain constraints are also taken care, and make a shadow class if multiple classes indicated."
  ;; make-instance (class . initargs) and make-instance (meta . initags)
  ;(format t "~%MAKE-INSTANCE ~S ~S" class initargs)
  (cond ((null initargs) (call-next-method))
        (t (let ((domains (collect-domains-from-initargs class initargs)))
             (cond ((null domains) (call-next-method))
                   ((null (cdr domains))
                    (let ((domain (car domains)))
                      (cond ((and (strict-class-p domain) (strict-class-p class)))
                            ((and (rdf-metaclass-p domain) (rdf-metaclass-p class)))
                            ((rdf-metaclass-p class)
                             (unless (cl:subtypep class domain)
                               (error "Classing error. ~S might have to be a strict class." class)))
                            ((strict-class-p class)
                             (error "Classing error. ~S might have to be a metaclass." class))
                            (t (error "Cant happen!")))
                      (cond ((subsumed-p class domain)
                             (call-next-method))
                            ((subsumed-p (car domains) class)
                             (apply #'make-instance domain initargs))
                            (t (warn "Multiple classing1 ~S with ~S" (getf initargs :name) (cons class domains))
                               (apply #'make-instance (make-shadow class (cons class domains)) initargs)
                               ))))
                   ((every #'(lambda (ty) (subsumed-p class ty)) domains)
                    (call-next-method))
                   (t (let ((new-types (most-specific-concepts (nreverse (cons class domains)))))
                        (warn "Multiple classing1 ~S with ~S" (getf initargs :name) new-types)
                        (apply #'call-next-method (make-shadow class new-types) initargs)))
                   )))))

;;;
;;;; Reinitialize Instance
;;;

(defmethod reinitialize-instance :before ((instance rdfs:Resource) &rest initargs)
  "After ensuring that every slot is effectively defined at the class of <instance>,
   if new type option derived from domain constraints are special than the old class, 
   change-class is invoked before primary method of <reinitialize-instance>."
  (let ((class (class-of instance)))
    (when initargs
      (ensure-class-slotds (class-of instance) initargs)
      (let ((domains (collect-domains-from-initargs class initargs)))
        (setq initargs
              (loop for (role filler) on initargs by #'cddr
                  unless (eq filler :unbound)
                  append (list role filler)))
        (cond ((null domains))                          ; nothing done
              ((null (cdr domains))
               (cond ((subsumed-p class (car domains))) ; nothing done
                     ((subsumed-p (car domains) class)
                      (change-class instance (car domains)))
                     (t ; for pizza ontology that holds Functional and InverseFunctional
                      (warn "Multiple classing2 ~S with ~S" (getf initargs :name) (cons class domains))
                      (change-class instance (make-shadow class (cons class domains))))))
              ((every #'(lambda (ty) (subsumed-p class ty)) domains)) ; nothing done
              (t (let ((new-types (most-specific-concepts (nreverse (cons class domains)))))
                   (warn "Multiple classing2 ~S with ~S" (getf initargs :name) new-types)
                   (change-class instance (make-shadow class new-types)))))))))

(defun read-data (type str)
  "read <str> as <type> and returns an instance of <type>."
  (etypecase type
    (null (read-in-lang-env str))
    (xsd:string (read-in-lang-env str))
    (rdfs:Class 
     (cond ((cl:subtypep type rdfs:Literal) (read-in-lang-env str))
           ((cl:subtypep type rdfs:Resource) (read-in-lang-env str))
           (t (error "~S is not yet prepared!" type))))
    (symbol 
     (ecase type
       (xsd:string (read-in-lang-env str))
       ((xsd:integer xsd:long xsd:int xsd:short xsd:byte xsd:nonNegativeInteger  
                     xsd:negativeInteger xsd:positiveInteger xsd:nonPositiveInteger)
        (let ((value (read-from-string str)))
          (assert (cl:typep value type))
          value))
       (xsd:anyURI (iri str))
       (xsd:decimal (rational (read-from-string str)))
       (xsd:float (cl:float (read-from-string str)))
       (xsd:double (cl:float (read-from-string str) 1.0d0))
       (xsd:boolean
        (let ((value (read-from-string str)))
          (assert (cl:typep value type))
          value))))))

(defun read-in-lang-env (str)
  "reads <str> with language option in RDF, and returns an instance of rdf:inLang. See also, <lang-env>."
  (cond ((null lang-env) str)
        (t (make-instance 'rdf:inLang :lang lang-env :content str))))

;;;
;;;; Slots := (role . forms) | (role filler)
;;;

(defun create-slot (role filler) (declare (inline)) (list role filler))
(defun slot-role (slot) (declare (inline)) (first slot))
(defun slot-forms (slot) (declare (inline)) (rest slot))
(defun slot-filler (slot) (declare (inline)) (second slot))
(defun get-filler (slots role) (declare (inline)) (slot-filler (assoc role slots)))

;;;
;;;; Containers in RDF
;;; 
;;; See, RDFboot module.
;;;
#|
(defConcept rdfs:Container (rdf:type rdfs:Class)
  (rdfs:subClassOf rdfs:Resource)
  (rdfs:label "Container")
  (rdf:about "http://www.w3.org/2000/01/rdf-schema#Container")
  (rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#"))
  (rdfs:comment "The class of RDF containers."))

(defProperty rdfs:member
  (rdf:about "http://www.w3.org/2000/01/rdf-schema#member")
  (rdfs:label "member")
  (rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#"))
  (rdfs:comment "A member of the subject container.")
  (rdfs:domain rdfs:Container)                              ; reaxiomatized by Seiji 2008/6/27
  (rdfs:range rdfs:Resource))

(defConcept rdfs:ContainerMembershipProperty (rdf:type rdfs:Class)
  (rdfs:subClassOf rdf:Property)
  (rdfs:label "ContainerMembershipProperty")
  (rdf:about "http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty")
  (rdfs:isDefinedBy (iri "http://www.w3.org/2000/01/rdf-schema#"))
  (rdfs:comment "The class of container membership properties, rdf:_1, rdf:_2, ...,
  all of which are sub-properties of 'member'."))
|#
;;;; Rdfs12
;;; If <uuu> rdf:type rdfs:ContainerMembershipProperty, 
;;; then, <uuu> rdfs:subPropertyOf rdfs:member.

(defmethod shared-initialize :after
  ((instance rdfs:ContainerMembershipProperty) slot-names &rest initargs)
  "adds rdfs:member info into <instance> according to rdfs12 rule."
  (declare (ignore slot-names))
  (let ((superprops (and (slot-boundp instance 'rdfs:subPropertyOf)
                         (slot-value instance 'rdfs:subPropertyOf))))
    (cond ((null superprops)
           (setf (slot-value instance 'rdfs:subPropertyOf) rdfs:member))
          ((not (consp superprops))
           (unless (subproperty-p superprops rdfs:member)
             (setf (slot-value instance 'rdfs:subPropertyOf) (list superprops rdfs:member))))
          (t (pushnew rdfs:member (slot-value instance 'rdfs:subPropertyOf)
                      :test-not #'(lambda (m superp) (subproperty-p superp m))))))
  (pushnew instance (slot-value rdfs:member 'subproperty)
                      :test-not #'(lambda (ins subp) (subproperty-p ins subp))))

(defConcept rdf:Bag (rdf:type rdfs:Class)
  (rdfs:subClassOf rdfs:Container)
  (rdfs:label "Bag")
  (rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag")
  (rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
  (rdfs:comment "The class of unordered containers."))
(defConcept rdf:Seq (rdf:type rdfs:Class)
  (rdfs:subClassOf rdfs:Container)
  (rdfs:label "Seq")
  (rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq")
  (rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
  (rdfs:comment "The class of ordered containers."))
(defConcept rdf:Alt (rdf:type rdfs:Class)
  (rdfs:subClassOf rdfs:Container)
  (rdfs:label "Alt")
  (rdf:about "http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt")
  (rdfs:isDefinedBy (iri "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
  (rdfs:comment "The class of containers of alternatives."))

(defun rdfs:subClassOf (class)
  (slot-value class 'rdfs:subClassOf))
(defsetf rdfs:subClassOf (class) (value)
  `(setf (slot-value ,class 'rdfs:subClassOf) ,value))

(defun rdfs:subPropertyOf (class)
  (slot-value class 'rdfs:subPropertyOf))
(defsetf rdfs:subPropertyOf (class) (value)
  `(setf (slot-value ,class 'rdfs:subPropertyOf) ,value))

;;;
;;;; Automatated ContainerMembershipProperty Creation
;;; If <x> rdf:type rdfs:Container and <x> <p> <vvv>,
;;; then <p> rdf:type rdfs:ContainerMembershipProperty,
;;; and <p> rdfs:subPropertyOf rdfs:member.

(defun check-ordinal-properties (slot-forms)
  (loop for slot in slot-forms
      unless (or (atom slot)
                 (eq :name (slot-role slot))
                 (eq 'rdf:about (slot-role slot))
                 (eq 'rdf:ID (slot-role slot))
                 )
      do (make-ordinal-property (slot-role slot))))

(defun make-ordinal-property (role)
  (addInstance rdfs:ContainerMembershipProperty role ())
  role)

;;;
;;;; From Schank's M-SEQUENCE in Memory Organization Package (MOP)
;;;
(defun list2seq (l)
  "returns an instance of rdf:Seq with members from <l>. The first element of 
   <l> fills the first role rdf:_1, the second fills the second role rdf:_2, and so on. 
   If <l> is empty, rdf:nil is returned."
  (if (null l) rdf:nil
    (addInstance rdf:Seq nil
                 (loop for x in l
                     for i from 1 to (length l)
                     collect (create-slot (make-ordinal-property-from-number i) x)))))

(defun list2bag (l)
  "returns an instance of rdf:Seq with members from <l>. The first element of 
   <l> fills the first role rdf:_1, the second fills the second role rdf:_2, and so on. 
   If <l> is empty, rdf:nil is returned."
  (if (null l) rdf:nil
    (addInstance rdf:Bag nil
                 (loop for x in l
                     for i from 1 to (length l)
                     collect (create-slot (make-ordinal-property-from-number i) x)))))

(defun list2alt (l)
  "returns an instance of rdf:Seq with members from <l>. The first element of 
   <l> fills the first role rdf:_1, the second fills the second role rdf:_2, and so on. 
   If <l> is empty, rdf:nil is returned."
  (if (null l) rdf:nil
    (addInstance rdf:Alt nil
                 (loop for x in l
                     for i from 1 to (length l)
                     collect (create-slot (make-ordinal-property-from-number i) x)))))

(defmethod shared-initialize :after ((instance rdfs:Resource) slot-names &rest initargs)
  "book-keeping for reification seiji"
  (let ((args (copy-list initargs)))
    (let ((changed (remf args :direct-slots)))
      (setq changed (or (remf args :direct-superclasses) changed))
      (cond ((and (null slot-names) (null args))  ; when change-class
             )
            ((and (consp slot-names) (null args)) ; when propagated
             )
            (t                                        ; first or redefinition
             (typecase instance
               (rdfs:Literal nil)
               (rdfs:Datatype nil)
               (rdf:Statement nil)
               (rdf:List nil)
               (otherwise 
                (apply #'book-keeping-for-reification instance slot-names args)
                (let ((name (getf args :name)))
                  (when name
                    (when (nodeID? name)
                      (setf (slot-value instance 'excl::name) nil))
                    (export-as-QName name)
                    (setf (symbol-value name) instance)))
                
                ;; OWL module is inserted here.
                )))))))

(defun collect-owl-role-name-if (test obj)
  (loop for slotd in (mop:class-slots (class-of obj))
      when (and (cl:typep slotd 'gx::Property-effective-slot-definition)
                (funcall test (symbol-value (name slotd))))
      collect (name slotd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Followings classes have no metaclass reference.
;; Note that type check does not occure in making an instance.

;(setq *reify-p* t)

(eval-when (:execute :load-toplevel)
  (setq *referenced-resources* nil)
  )

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Aug-04-2009
;;;
