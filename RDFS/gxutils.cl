;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; GXUtils module
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT
;;;
;;; Copyright (c) 2002, 2003 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2008 Seiji Koide
;;;
;; History
;; -------
;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfparser)
  (require :rdfscore)
  )

(in-package :gx)

(export '(print-all-entity-uris do-all-entity-uris list-all-entity-uris
          print-all-entity-iris do-all-entity-iris list-all-entity-iris
          list-all-uri-namedspaces named-p anonymous-p nodeID-p dph dah
          get-form list-all-statements get-slots))

(export '(-> collect-direct-instances-of collect-all-instances-of collect-all-extensions-of
           all-instances-generator all-extensions-of-generator ; by smh
           collect-all-supers collect-all-subtypes
           all-concept-names all-role-names all-individuals
           get-value put-value list-all-entities-in list-all-resources
           *autoepistemic-local-closed-world*))

(defpackage :gx-user
  (:use :common-lisp :gx)
  (:shadow change-class subject predicate object type)
  (:shadowing-import-from :gx type-of typep subtypep)
  (:documentation "http://www.galaxy-express.co.jp/semweb/gx-user#"))

;;;
;;;; All Entities
;;;
;;; Every resource URI in SWCLOS is interned into the default uri space. Note that this default 
;;; uri space is independent from Named Spaces for prefix (package) and local name (env) for QName. 

(defun print-all-entity-uris (&optional (stream t))
  "prints out all entities as uri to <stream>. This function does not print blank nodes."
  (net.uri:do-all-uris (x) (print x stream)))

(defun print-all-entity-iris (&optional (stream t))
  "prints out all entities as iri to <stream>. This function does not print blank nodes."
  (do-all-entity-iris #'(lambda (x) (print x stream))))

(defun do-all-entity-uris (fun)
  "invokes <fun> for all entities as uri. <fun> should be one parameter funcallable object."
  (net.uri:do-all-uris (x) (funcall fun x)))

(defun do-all-entity-iris (fun)
  "invokes <fun> for all entities as iri. <fun> should be one parameter funcallable object."
  (net.uri:do-all-uris (x) (when (typep x 'iri) (funcall fun x))))

(defun list-all-entity-uris ()
  "collects all entities as uri, and returs it. Here, entity means ontologies 
   designated by owl:Ontology+rdf:about and entities in ontologies."
  (let (uris)
    (do-all-entity-uris
        #'(lambda (x) (when (and (iri-boundp x) (iri-value x)) (push x uris))))
    uris))

(defun list-all-entity-iris ()
  "collects all entities as iri, and returs it. Here, entity means ontologies 
   designated by owl:Ontology+rdf:about and entities in ontologies."
  (let (uris)
    (do-all-entity-iris
        #'(lambda (x) (when (and (iri-boundp x) (iri-value x)) (push x uris))))
    uris))

;;;
;;;; All NamedSpace
;;;

(defun list-all-uri-namedspaces ()
  "returns an association list of prefix name (package name) and uri on all ones in the system."
  (let (namespaces)
    (net.uri:do-all-uris (x *NameSpaces* namespaces)
      (when (cl:typep x 'uri-namedspace)
        (push x namespaces)))))

(defun do-all-uri-namedspaces (fun)
  "invokes <fun> for all namedspaces. <fun> should be one parameter funcallable object."
  (net.uri:do-all-uris (x *NameSpaces*)
    (when (cl:typep x 'uri-namedspace)
      (funcall fun x))))

(defgeneric list-all-entities-in ((namespace string) &optional uri?)
  (:documentation 
   "These methods return all entities in <namespace>, namely all external symbol in <namespace> package. 
Note that it is not cared that symbols are bound to resource objects or not."))

(defmethod list-all-entities-in ((namespace string) &optional uri?)
  "When <namespace> is a string, recursively called with a uri of <namespace>."
  (list-all-entities-in (iri namespace) uri?))
(defmethod list-all-entities-in ((namespace net.uri:uri) &optional uri?)
  "When <namespace> is a uri, the related package is retrieved of <namespace>, then recursively called with the package."
  (let ((pkg (uri2package namespace)))
    (when pkg (list-all-entities-in pkg uri?))))
(defmethod list-all-entities-in ((namespace symbol) &optional uri?)
  "When <namespace> is a non-nil symbol, recursively called with a package of <namespace>."
  (when namespace (list-all-entities-in (find-package namespace) uri?)))
(defmethod list-all-entities-in ((namespace package) &optional uri?)
  "When <namespace> is a package, every external symbol in <namespace> is collected, 
   and it is returned as a list of symbol (when uri? is false) or uri (when uri? is true)."
  (loop for x being each external-symbol in namespace
      collect (if uri? (symbol2uri x) x)))

;;
;; Utilities
;;

(defun named-p (resource)
  (typecase resource
    (rdfs:Class (not (not (class-name resource))))
    (rdf:Property (not (not (name resource))))
    (rdfs:Resource
     (not (not (slot-value resource 'excl::name))))))

(defun anonymous-p (resource)
  (typecase resource
    (rdfs:Class (not (class-name resource)))
    (rdf:Property (not (name resource)))
    (rdfs:Resource
     (or (not (slot-exists-p resource 'excl::name))
         (not (slot-value resource 'excl::name))))))
#|
(defun nodeID-p (resource)
  (and (cl:typep resource rdfs:Resource)
       (gx::name resource)
       (symbol-package (gx::name resource))
       (string= "_" (package-name (symbol-package (gx::name resource))))))
|#
(defun get-form (resource)
  (when (null resource) (return-from get-form))
  (assert (typep resource rdfs:Resource))
  `(,(or (type-tag resource) (type-of resource))
      ,@(unless (anonymous-p resource) (list (name resource)))
      ,@(when (and (slot-boundp resource 'rdf:about) (slot-value resource 'rdf:about))
          `((rdf:about ,(slot-value resource 'rdf:about))))
      ,@(loop for (role . fillers) in (get-slots resource)
           ; unless (eq role 'rdfs:label)
            collect (cons role 
                          (loop for filler in fillers
                              collect (cond ;((cl:typep filler 'rdf:inLang)
                                            ; `(,(lang filler) ,(content filler)))
                                            ((typep filler 'xsd:anySimpleType) filler)
                                            ((typep filler rdfs:Literal) filler)
                                            ((typep filler 'net.uri:uri) filler)
                                            ((eq role 'rdfs:subClassOf)
                                             (or (and (symbolp filler) filler)
                                                 (and (rdf-class-p filler) (class-name filler))
                                                 (get-form filler)))
                                            ((anonymous-p filler) (get-form filler))
                                            ;((nodeID-p filler) (get-form filler))
                                            ((rsc-object-p filler) (name filler))
                                            (t filler)))))))

(defun slots-of (ins)
  (loop for role in (collect-prop-names-from (class-of ins)) with filler
      when (and (slot-boundp ins role)
                (setq filler (slot-value ins role)))
      collect (cond ((consp filler) (cons role filler))
                    (t (list role filler)))))

(defgeneric get-slots (obj)
  (:documentation
   "get-slots <obj>
   returns a slot list of <obj>. Note that nil is returned if <obj> 
   is not a resource.")
  )

(defmethod get-slots ((obj rdfs:Class))
  (slots-of obj))

(defmethod get-slots ((obj rdfs:Resource))
  (slots-of obj))

(defmethod get-slots ((obj symbol))
  (when (object? obj)
    (get-slots (symbol-value obj))))

(defmethod get-slots (obj)
  (declare (ignore obj))
  nil)

(defun make-anonymous-label (ins)
  (make-symbol (concatenate 'string "an-anonymous-" (string (slot-value (class-of ins) 'rdfs:label)))))

(defun mop-specs (mop)
  "mop-specs <mop>
   returns a list of direct specials of <mop>."
  (cond ((typep mop 'rdfs:Class)
         (mop:class-direct-subclasses mop))
        (t nil)))

(defun tree->list (mop fn visited)
  "tree->list <mop> <function> <mop-list>
   returns a list starting with <mop>, followed by the elements of
   the list returned by calling <function> with <mop> and <mop-list>
   updated to include <mop>. If <mop> is already in <mop-list>, just
   a list with <mop> is returned."
  (cond ((eq mop (find-class 'rdfsClass)) nil)
        ((member mop visited) (name mop))
        ((not (%instance-p mop))
         (push mop visited)
         `(,(name mop) ,@(funcall fn mop visited)
             ,@(mapcar #'name (class-direct-instances mop))))))

(defun dah (mop)
  "dah <mop>
   prints all the specalizations under <mop>. The name is short for
   'display abstraction hierarchy'"
  (pprint (tree->list mop #'specs->list nil)))

(defun specs->list (mop visited)
  "SPECS->LIST <mop> <mop-list>
   returns a list starting with <mop>, followed by the specialization
   tree under each specializations of <mop>."
  (loop for spec in (or (mop-specs mop) (class-direct-instances mop))
      when (tree->list spec #'specs->list visited)
      collect it))

(defmethod get-about-slot ((mop rdf:Property))
  (when (get (slot-value mop 'rdfs:label) 'rdf:about)
    (list (list 'rdf:about (get (slot-value mop 'rdfs:label) 'rdf:about)))))

(defmethod get-about-slot ((mop rdfs:Resource))
  (when (and (slot-exists-p mop 'rdfs:label) (slot-boundp mop 'rdfs:label) (get (slot-value mop 'rdfs:label) 'rdf:about))
    (list (list 'rdf:about (get (slot-value mop 'rdfs:label) 'rdf:about)))))

(defun path-filler (mop path)
  "path-filler <mop> <path>
   returns the filler for <path> in <mop>. A path is a list of roles, and 
   path-filler follows that list in order, using get-filler. A role is a 
   property object."
  ;(format t "~%Path-filler:~S ~S" mop path)
  (cond ((null mop) nil)
        ((and (symbolp mop) (object? mop) (cl:typep (symbol-value mop) 'gx::shadowed-class))
         (car (mop:class-direct-superclasses (symbol-value mop))))
        ((and (rsc-object-p mop) (cl:typep mop 'gx::shadowed-class))
         (car (mop:class-direct-superclasses mop)))
        ((null path) (mkatom mop))
        ((consp mop)
         (loop for m in mop with result
             when (setq result (path-filler m path))
             return result))
        (t (let ((specifier (car path)))
             (let ((role (if (consp specifier) (car specifier) specifier))
                   (type (if (consp specifier) (second specifier) t)))
               (when (symbolp mop)
                 (setq mop (symbol-value mop)))
               (when (symbolp role)
                 (setq role (symbol-value role)))
               (cond ((gx::subproperty-p role rdf:type)
                      (path-filler (constraint-filter (type-of mop) type) (cdr path)))
                     ((gx::subproperty-p role rdfs:subClassOf)
                      (path-filler (constraint-filter (slot-value mop 'rdfs:subClassOf) type) (cdr path)))
                     ((gx::subproperty-p role rdfs:label)
                      (path-filler (constraint-filter (slot-value mop 'rdfs:label) type) (cdr path)))
                     (t (let ((vals (get-value mop role)))
                          (path-filler (constraint-filter vals type) (cdr path))))))))))

(defun collect-all-subproperties (property)
  (cons property
        (mappend #'collect-all-subproperties (mklist (slot-value property 'subproperty)))))

(defun collect-all-superproperties (property)
  (cons property
        (mappend #'collect-all-superproperties (mklist (superproperty-of property)))))

(defun constraint-filter (mop type)
  (cond ((consp mop) (loop for m in mop when (typep m type) collect m))
        ((typep mop type) mop)))

(defun -> (mop &rest roles)
  "-> <mop> <role1> <role2> ...
   returns the filler found by tracing <role1> <role2> ... from the <mop>.
   In other words, inherited filler of <mop> and <role1> is used in next 
   filler-retlieving with <role2>, and the result is used in next ..., and so 
   on.  <mop> is a mop object or a slot-list."
  (path-filler mop roles))

(defun (setf ->) (value mop &rest roles)
  (format t "~%(setf ->) Value=~S mop=~S roles=~S" value mop roles)
  (cond ((null (slot-exists-p mop (name (car roles)))) nil)
        ((null (cdr roles)) (setf (slot-value mop (name (car roles))) value))
        ((and (slot-boundp mop (name (car roles)))
              (slot-value mop (name (car roles))))
         (setf (apply #'-> (slot-value mop (name (car roles))) (cdr roles)) value))
        (t ;slot exists but unbound with multiple roles
         (let ((slotd (find (name (car roles)) (mop:class-slots (class-of mop))
                            :key #'mop:slot-definition-name)))
           (format t "~%SLOTD=~S" slotd)
           (when slotd
             (setf (slot-value mop (name (car roles)))
               (%setfvalue value (mop:slot-definition-type slotd) (cdr roles))))))))

(defun %setfvalue (value type roles)
  (cond ((null roles) value)
        (t (unless (mop:class-finalized-p (symbol-value type))
             (mop:finalize-inheritance (symbol-value type)))
           (let ((slotd (find (name (car roles)) (mop:class-slots (symbol-value type))
                              :key #'mop:slot-definition-name)))
             (format t "~%slotd=~S" slotd)
             (when slotd
               (let ((filler (%setfvalue value (mop:slot-definition-type slotd) (cdr roles))))
                 (addObject (symbol-value type) `((,(name (car roles)) ,filler)))))))))

;;
;;
;;

(defmethod collect-direct-subtypes ((class symbol))
  (collect-direct-subtypes (symbol-value class)))

(defmethod collect-direct-subtypes ((class rdfs:Class))
  (mop:class-direct-subclasses class))

(defmethod collect-all-subtypes ((class symbol))
  (collect-all-subtypes (symbol-value class)))

(defmethod collect-all-subtypes ((class rdfs:Class))
  (remove-duplicates 
   (append (collect-direct-subtypes class)
           (loop for sub in (mop:class-direct-subclasses class)
               append (cond ((eq sub (find-class 'rdfsClass)) (list rdfs:Class))
                            (t (collect-all-subtypes sub)))))))

(defmethod collect-all-subsumed-types ((class symbol))
  (collect-all-subsumed-types (symbol-value class)))

(defmethod collect-all-subsumed-types ((class rdfs:Class))
  (collect-all-subtypes class))

(defmethod collect-direct-instances-of ((class symbol)) ;smh
  (collect-direct-instances-of (symbol-value class)))
(defmethod collect-direct-instances-of ((class rdfs:Class)) ;smh
  (class-direct-instances class))

(defmethod all-instances-generator ((class symbol)) ; added smh
  (all-instances-generator (symbol-value class)))

(defmethod all-instances-generator ((class rdfs:Class)) ; added smh
  (let ((pending-classes (list class))
        (pending-instances nil))
    (flet ((generator ()
                      (loop
                        (when pending-instances
                          (return-from generator (pop pending-instances)))
                        (when (null pending-classes) (return-from generator nil))
                        (let ((next-class (pop pending-classes)))
                          (setf pending-classes (append (mop:class-direct-subclasses next-class) pending-classes)
                            pending-instances (class-direct-instances next-class))))))
      #'generator)))

;; See OWL module file for all-instances-generator method

(defun collect-all-supers (class)
  (cond ((eq class rdfs:Resource) (list class))
        ((eq class |rdfs:Resource|) (list class))
        (t (let ((supers (reduce #'union
                                 (mapcar #'collect-all-supers (mop:class-direct-superclasses class)))))
             (pushnew class supers)))))

(defmethod collect-all-extensions-of ((property symbol))
  (when (not (null property))
    (collect-all-extensions-of (symbol-value property))))

(defmethod collect-all-extensions-of ((property rdf:Property))
  (declare (optimize (speed 3) (safety 0)))
  (let ((collector (list nil)))
    (collect-all-extensions-of-1 property collector)
    (remove-duplicates (cdr collector) :test #'equalp)))

(defmethod collect-all-extensions-of-1 ((property rdf:Property) collector)
  (declare (optimize (speed 3) (safety 0)))
  (let ((prop-name (name property)))
    (loop for slotd in (slot-value property 'slotds)
        append (loop for individual in (collect-all-instances-of (slot-definition-subject-type slotd))
                   as values = (and (slot-exists-p individual prop-name) ; seiji 2006/2/17
                                    (slot-boundp individual prop-name)
                                    (slot-value individual prop-name))
                   if (consp values)
                   do (loop for value in values
                          do (push (list individual value) (cdr collector)))
                   else do (if values (push (list individual values) (cdr collector)))))
    ;; Do we need to check for circularities in superproperties?  Should at least check
    ;; for multiple appearances.
    (loop for superproperty in (slot-value property 'subproperty)
        do (collect-all-extensions-of-1 superproperty collector))))

;; added smh
(defmethod all-extensions-of-generator ((property symbol))
  (all-extensions-of-generator (symbol-value property)))

(defmethod all-extensions-of-generator ((property rdf:Property))
  (let ((prop-name nil)
        (pending-slotds nil)
        (pending-superproperties (list property))
        (individual-generator (lambda () nil))
        (pending-individual nil)
        (pending-values nil))
    (flet ((generator ()
                      (loop
                        (when pending-values
                          (return-from generator
                            (list pending-individual (pop pending-values))))
                        (loop as individual = (funcall individual-generator)
                            while individual
                            when (and (slot-boundp individual prop-name)
                                      (slot-value individual prop-name))
                            do (let ((val (slot-value individual prop-name)))
                                 (when (consp val)
                                   (setf pending-individual individual
                                     pending-values (cdr val))
                                   (return-from generator
                                     (list individual (car val))))
                                 (when val
                                   (return-from generator
                                     (list individual val)))))
                        (loop while (null pending-slotds)
                            do
                              (unless (setf property (pop pending-superproperties))
                                (return-from generator nil))
                              #+never (format t "~&commencing property: ~s~%" property)
                              (setf pending-slotds (slot-value property 'slotds)
                                prop-name (name property))
                              ;; Do we need to check for circularities in superproperties?  Should at
                              ;; least check for multiple appearances.
                              (loop for superproperty in (slot-value property 'subproperty)
                                  do #+never (format t "~&superproperty: ~s ~s~%" property superproperty)
                                    (push superproperty pending-superproperties)
                                    ))
                        #+never
                        (format t "~&processing slotd: ~s ~s ~s~%" property (car pending-slotds)
                          (slot-definition-subject-type (car pending-slotds)))
                        (setf individual-generator
                          (all-instances-generator (slot-definition-subject-type (pop pending-slotds)))))))
      #'generator)))

(defparameter *system-properties* (class-direct-instances rdf:Property))

(defun list-all-properties (&optional with-system-property-p)
  "lists all properties. If calling with parameter t, it forces to output 
   properties including system predefined properties. Otherwise only user
   properties."
  (loop for prop in (collect-all-instances-of rdf:Property)
        when (or with-system-property-p (not (member prop *system-properties*)))
      collect (name prop)))

(eval-when (:execute :load-toplevel :compile-toplevel)
(defun %%list-all-resources (root)
  (case (class-name root)
    ((rdfsClass rdf-node) nil)
    (rdf:XMLLiteral (list root))
    (otherwise
     (append (cons root (class-direct-instances root))
             (loop for sub in (mop:class-direct-subclasses root)
                 append (%%list-all-resources sub))))))
)

(defconstant *system-resources* (%%list-all-resources rdfs:Resource))

(defun list-all-resources (&optional with-system-rsc-object-p)
  (%list-all-resources rdfs:Resource with-system-rsc-object-p))
(defun %list-all-resources (root with-system-rsc-object-p)
  (case (class-name root)
    ((rdfsClass rdf-node) nil)
    (rdf:XMLLiteral (list root))
    (otherwise
     (when (or with-system-rsc-object-p (not (member root *system-resources*)))
       (append (cons root (class-direct-instances root))
               (loop for sub in (mop:class-direct-subclasses root)
                   append (%list-all-resources sub with-system-rsc-object-p)))))))

(defun list-all-statements ()
  (loop for state in (class-direct-instances rdf:Statement)
      collect (cond ((slot-value state 'excl::name)
                     `(,(slot-value state 'excl::name)
                         ,(slot-value state 'rdf:subject)
                         ,(slot-value state 'rdf:predicate)
                         ,(slot-value state 'rdf:object)))
                    (t `(,(slot-value state 'rdf:subject)
                           ,(slot-value state 'rdf:predicate)
                           ,(slot-value state 'rdf:object))))))

(defun collect-domain-properties (subject &optional (prop rdf:Property))
  "collect all properties under <prop> that have <subject> as domain."
  (append (loop for ins in (class-direct-instances prop)
              when (let ((domain (get-domain ins)))
                     (or (and (symbolp domain) (eq (name subject) domain))
                         (and (rsc-object-p domain) (subtypep subject domain))))
              collect ins)
          (mapcan #'(lambda (sub) (collect-domain-properties subject sub))
            (mop:class-direct-subclasses prop))))

(defun collect-range-properties (object &optional (prop rdf:Property))
  "collect all properties under <prop> that have <object> as range."
  (append (loop for ins in (class-direct-instances prop)
              when (let ((range (get-range ins)))
                     (or (and (symbolp range) (eq (name object) range))
                         (and (rsc-object-p range) (subtypep object range))))
              collect ins)
          (mapcan #'(lambda (sub) (collect-range-properties object sub))
            (mop:class-direct-subclasses prop))))

;;;
;;;; DIG interface
;;;

(defun all-concept-names (&optional (ns *default-namespace*))
  "returns all named concept names in <ns>.
   If <ns> is neither a uri-namedspace uri nor string nor package
   nor package name symbol, then returns nil."
  (let ((pkg
         (typecase ns
          (string (uri2package ns))
          (net.uri:uri (uri2package ns))
          (symbol (find-package ns))
          (packagep ns)
          (otherwise (return-from all-concept-names)))))
    (loop for x being each external-symbol in pkg
        when (and (boundp x) (rdf-class-p (symbol-value x)))
        collect x)))

(defun all-role-names (&optional (ns *default-namespace*))
  "returns all role names in <ns>.
   If <ns> is neither a uri-namedspace uri nor string nor package
   nor package name symbol, then returns nil."
  (let ((pkg
         (typecase ns
           (packagep ns)
           (symbol (find-package ns))
           (uri-namedspace (uri-namedspace-package ns))
           (net.uri:uri (uri-namedspace-package (get-uri-namedspace ns)))
           (string (uri-namedspace-package (get-uri-namedspace (iri ns))))
           (otherwise (return-from all-role-names)))))
    (loop for x being each external-symbol in pkg
        when (and (boundp x) (role-p (symbol-value x)))
        collect x)))

(defun all-individuals (&optional (ns *default-namespace*))
  "returns all individuals or instances in <ns>.
   If <ns> is neither a uri-namedspace uri nor string nor package
   nor package name symbol, then returns nil."
  (let ((pkg
         (typecase ns
          (string (uri-namedspace-package ns))
          (net.uri:uri (uri-namedspace-package ns))
          (symbol (find-package ns))
          (packagep ns)
          (otherwise (return-from all-individuals)))))
    (labels ((%all-individuals (cls)
                               (when (and (not (eql cls (load-time-value (find-class 'rdfs:Class))))
                                          (not (eql cls (load-time-value (find-class 'rdfsClass)))))
                                 (append (remove-if-not #'(lambda (obj) (or (anonymous-p obj)
                                                                            (eql (symbol-package (name obj)) pkg)))
                                                        (class-direct-instances cls))
                                         (loop for sub in (mop:class-direct-subclasses cls)
                                             append (%all-individuals sub))))))
      (%all-individuals (load-time-value (find-class 'rdfs:Resource))))))

(defun concept-parents (concept)
  (etypecase concept
    (symbol )
    (rdfs:Datatype )
    (rdfs:Class )
    (net.uri:uri (concept-parents (iri-value concept)))
    (consp )))

(defun get-value (object role)
  "gets all values with sameAs definition. See rdfp11."
  (declare (optimize (speed 3) (safety 0)))
  (mkatom 
   (remove-duplicates
    (mappend #'same-as-of 
             (mappend #'(lambda (obj) (%get-value obj role)) (same-as-of object))))))
(defun %get-value (object role)
  "rdfs7 + inverserole"
  (declare (optimize (speed 3) (safety 0)))
  (let ((name (name role)))
    (let* ((values (and (slot-exists-p object name)
                        (slot-boundp object name)
                        (slot-value object name)))
           (invrole (%get-inverse-prop role))
           (invvalue (when invrole
                       (car (rassoc object (collect-all-extensions-of invrole) :test #'member))))
           (hasvalue (%get-hasfiller-inherited (class-of object) name)))
      (remove-duplicates (append (mklist values) (mklist invvalue) (mklist hasvalue)
                                 (mappend #'(lambda (r) (mklist (get-value object r)))
                                          (slot-value role 'subproperty)))))))

(defun %get-inverse-prop (prop)
  (declare (ignore prop))
  nil)

(defun %get-hasfiller-inherited (class role)
  (declare (optimize (speed 3) (safety 0)))
  (loop for slotd in (mop:class-slots class) with found
      when (and (eq (name slotd) role)
                (let ((type (mop:slot-definition-type slotd)))
                  (setq found
                        (cond ((consp type)
                               (find-if #'(lambda (ty) (cl:typep ty 'fills)) type))
                              ((cl:typep type 'fills)
                               type)))))
      do (return (fills-filler found))))

;;
;; put-value
;;

(defmethod put-value ((object rdfs:Class) (role (eql rdf:type)) value)
  (declare (ignore value))
  (error "Not Yet!"))

(defmethod put-value ((object rdfs:Class) (role (eql rdfs:subClassOf)) value)
  (declare (ignore value))
  (error "Not Yet!"))

(defmethod put-value ((object rdfs:Resource) (role (eql rdf:type)) value)
  (declare (ignore value))
  (error "Not Yet!"))

(defmethod put-value ((object rdfs:Resource) (role (eql rdfs:subClassOf)) value)
  (declare (ignore value))
  (error "Not Yet!"))

(defmethod put-value ((object rdfs:Resource) (role rdf:Property) value)
  (let ((pname (name role)))
    (if (slot-exists-p object pname)
        (if (slot-boundp object pname)
            (if (slot-value object pname)
                (setf (slot-value object pname)
                  (compute-slot-value value
                                      (slot-value object pname)
                                      (find pname (mop:class-slots (class-of object))
                                            :key #'mop:slot-definition-name)
                                      object))
              (setf (slot-value object pname) value))
          (setf (slot-value object pname) value))
      (progn
        (reinitialize-instance
         (class-of object)
         :direct-slots `((:name ,pname :initargs (,pname) :type ,(or (get-range role) t)
                          :documentation "By putting value into new slot"
                          :subject-type ,(class-of object))))
        (setf (slot-value object pname) value)))))

(defmethod put-value ((object rdfs:Resource) (role symbol) value)
  (cond ((keywordp role) (error "Illegal property designated:~S" role))
        ((property? role) (put-value object (symbol-value role) value))
        (t (warn "Entail by rdfs1: ~S rdf:type rdf:Property." role)
           (put-value object (make-instance 'rdf:Property :name role) value))))

(defmethod put-value (object role value)
  (declare (ignore object value))
  (error "Illegal property designated:~S" role))

;;;
;;;; Utilities for RDF Semantics
;;;
#|
(defun lean-p (&optional (ns *default-namespace*))
  (let ((pkg
         (typecase ns
           (packagep ns)
           (symbol (find-package ns))
           (uri-namedspace (uri-namedspace-package ns))
           (net.uri:uri (uri-namedspace-package (get-uri-namedspace ns)))
           (string (uri-namedspace-package (get-uri-namedspace (iri ns))))
           (otherwise (return-from lean-p)))))
    (loop for x being each external-symbol in pkg
        when (and (boundp x) (role-p (symbol-value x)))
        append (loop for (collect-all-extensions-of (symbol-value x)))
          ;; Seiji
          )))
|#
;;
;; Memoization from Norvig "Paradigms of AI Programming"
;;

(defun memoize (fn-name)
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))

(defun memo (fn)
  "Return a memo-function of fn."
  (let ((table (make-hash-table)))
    #'(lambda (x)
        (multiple-value-bind (val found-p) (gethash x table)
          (if found-p val
            (setf (gethash x table) (funcall fn x)))))))

(defun memoize2 (fn-name)
  "Replace fn-name's global definition with a memoized version with arity 2."
  (setf (symbol-function fn-name) (memo2 (symbol-function fn-name))))

(defun memo2 (fn)
  "Return a memo-function of fn with arity 2."
  (let ((table (make-hash-table)))   ; for x
    #'(lambda (x y)
        (multiple-value-bind (val1 found-p1) (gethash x table)
          (cond (found-p1
                 (multiple-value-bind (val2 found-p2) (gethash y val1)
                   (if found-p2 val2
                     (setf (gethash y val1) (funcall fn x y)))))
                (t (setf (gethash y (setf (gethash x table) (make-hash-table)))
                     (funcall fn x y))))))))

(eval-when (:load-toplevel)
  ;(memoize2 'ensure-class-slotds)
  )

#|
;; From Norvig, "Paradigms of AI Programming"
;; ==============================

;;; The Memoization facility:

(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))
|#
;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Aug-04-2009
;;;

(cl:provide :gxutils)
