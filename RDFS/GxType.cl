;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; GX Type module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002-2005 Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007-2008, 2009, 2012 Seiji Koide
;;;
;; History
;; -------
;;
;;; ==================================================================================

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :gxtype0)
  ) ; end of eval-when

(in-package :gx)

;;;
;;;; Non-Unique Name Assumption and Equality
;;;
;;; In Semantic Webs, the non Unique Name Assumption (nonUNA) is adopted in principle. 
;;; Namely, two and more different names or URIs may denote same object in the universe. It is 
;;; just like as people often call a person by both name and nickname. However, it is, so far, 
;;; unusual and not adopted in ordinary computer languages. In complete nonUNA principle, we would fall 
;;; into troublesome situation. For example, the computation could not conclude that (rdf:Description ex:A) 
;;; and (rdf:Description ex:B) are different or a triple ex:A/ex:p/1 is different from ex:B/ex:p/1. 
;;; The ex:A and ex:B might denote the same thing in RDF universe, and two graphs might mean same 
;;; meaning in the world. We need very laborious work in nonUNA principle to describe common knowledge 
;;; such as ex:Automobile is different from ex:Train, ex:Airplane, and ex:Ship. We must state that 
;;; xsd:|float| is different from xsd:|integer|, xsd:|URI|, xsd:|string|, xsd:|boolean|, etc. for all terms in ontology.
;;;
;;; Therefore, we set up the flag for non Unique Name Assumption false as default. Namely, 
;;; if two names or URIs are different, then the two bound objects must be different. When you 
;;; want to set the nonUNA principle up in program, encode like below. 
;;; ----------------------------------------------------------------------------------
;;;    (let ((*nonUNA* t))
;;;      ... some codes for nonUNA ...
;;;      )
;;; ----------------------------------------------------------------------------------
;;; Note that if two names or URIs are identical then they denote and are bound to an identical object 
;;; in spite of the value of *nonUNA*. Note also that, for anonymous objects, the graph structures of 
;;; objects must be compared in spite of the value of *nonUNA*. If two graphs for anonymous objects 
;;; are equal, then they are equal in RDF semantics. So, the question of equality is how to treat 
;;; the equality of named objects which have different names when the value of *nonUNA* is true. 
;;; In this implementation, 
;;;  * if two objects are atomic, namely no graph structure or no slot on rdf properties, 
;;;    if two objects have different names, then they are not equal in spite of the value of *nonUNA*.
;;;    if two objects are atomic and anonymous, then they are inevitably equal in spite of the value of *nonUNA*.
;;;  * if two objects are not atomic, namely they have composed structure or slots on rdf properties, 
;;;    they are equal if their structure is equal else not equal in case that the value of *nonUNA* is true. 
;;;    The name of complex objects are ignored when the value of *nonUNA* is true.
;;;
;;; Note that this implementation has no effects yet on owl:equivalentProperty. In SWCLOS, 
;;; every property must be named and different names must not be identical in equality checking. 
;;;
;;; Note that even though *nonUNA* is true or false, owl:sameAs and owl:differentFrom conduct SWCLOS and affect the 
;;; computation of equality in OWL.
;;;
;;; Note that owl:equivalentClass affects the computation of subsumption in OWL but does not affect the equality as 
;;; object or individual. See details in OWL module.
;;;
;;; Note that owl:disjointWith affects the computation of subsumption in OWL. Two concepts disjoint each other are 
;;; also different as object or individual. See details in OWL module.

(defvar *nonUNA* nil
  "A flag for non-UNA, it is false as default. Namely, the logic does not obey nonUNA as default.
   In order to set up complete nonUNA for Semantic Webs, set this value true. 
   Caution: we have seldom experienced on *nonUNA* = true.")

;;;
;;;; Algorithm of Equality in RDF Universe
;;;
;;; The algorithm of equality in RDF is described as follows, See RDF Primer specification from W3C, and
;;; http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/#section-Literal-Equality.
;;; # Two plane literals (string) without language tags are equal, if and only if they compare as equal, 
;;;   character by character.
;;; # Two plane literals with language tags are equal, if and only if the language tags are equal character 
;;;   by character without case sensitivity and the strings are equal character by character by case sensitivity.
;;; # Two RDF URI references are equal, if and only if they compare as equal, character by character, 
;;;   as Unicode strings.
;;; # Two plane literals as integer are equal, if and only if they are equal as mapped value in value space.
;;; # Two typed literals (xsd:|anySimpleType|) are equal, if and only if datatype URIs are equal and mapped values 
;;;   are equal in value space.
;;;
;;; Therefore, in this implementation
;;; # If <x> and <y> are equal as lisp object (integer, string, URI, symbol, CLOS object, etc.), 
;;;   then both are equal in RDF.
;;; # If <x> and <y> are instances of any datatype (xsd:|anySimpleType|), both are equal if both data types are 
;;;   equal and both values are equal.
;;; # If <x> and <y> are named CLOS objects, if both have a same name, then both are equal in RDF.
;;;   If both have different names, the both are not equal when *nonUNA* is false, or the graph equality is 
;;;   checked when *nonUNA* is true.
;;; # If <x> and <y> are anonymous CLOS objects, the graph equality is checked.
;;; # If at least one of <x> and <y> is an anonymous object then graph equality is checked.
;;; # If either <x> and <y> are URI or symbol and the relevant CLOS object exists as value, 
;;;   then the object is taken and recursively tested.
;;; # If either <x> and <y> are URI or symbol and no relevant CLOS object, then both are not equal. 
;;; # If <x> and <y> are instances of rdf:inLang (literal with language tag), both are equal if both tags are 
;;;   equal without case sensitivity and the strings are equal with case sensitivity.

;;; To test the equality in semantics of RDF, <rdf-equalp> is available. See the following example.
;;; ----------------------------------------------------------------------------------
;;; (rdf-equalp "this is string." "this is string.")                  -> true
;;; (rdf-equalp "string" "string"@en)                                 -> false
;;; (rdf-equalp "wine"@en (@ "wine" "EN"))                            -> true
;;; (rdf-equalp 1 1.0)                                                -> true
;;; (rdf-equalp 1 "1"^^xsd:|integer|)                                   -> false
;;; (rdf-equalp "1"^^xsd:|integer| (^^ 1 xsd:|integer|))                  -> true
;;; (rdf-equalp "1"^^xsd:|integer| (^^ 1 xsd:|nonNegativeInteger|))       -> false
;;; (rdf-equalp (iri "http://somewhere") (iri "http://somewhere"))    -> true
;;; (rdf-equalp (iri "http://somewhere") (iri "http://some%20where")) -> false
;;; (rdf-equalp 'foo 'bar)                                            -> false
;;; (defIndividual foo)                                     -> #<|rdfs:Resource| foo>
;;; (defIndividual bar)                                     -> #<|rdfs:Resource| bar>
;;; (rdf-equalp foo bar)                                              -> false
;;; (let ((gx::*nonUNA* t)) (rdf-equalp foo bar))                     -> false
;;; ----------------------------------------------------------------------------------
;;; However, if uris are bound to anonymous object, this predicate changes the result, 
;;; because SWCLOS retrieves the bound values by resolution, and compares two anonymous 
;;; objects. 
;;; ----------------------------------------------------------------------------------
;;; (rdf-equalp <http://somewhere> <http://anotherplace>)             -> false
;;; (setf (iri-value <http://somewhere>)
;;;   (addForm '(rdf:Description )))             -> #<|rdfs:Resource| :anonymous>
;;; (setf (iri-value <http://anotherplace>)
;;;   (addForm '(rdf:Description )))             -> #<|rdfs:Resource| :anonymous>
;;; (rdf-equalp <http://somewhere> <http://anotherplace>)             -> false
;;; (let ((*nonUNA* t))
;;;   (rdf-equalp <http://somewhere> <http://anotherplace>))          -> true
;;; ----------------------------------------------------------------------------------
;;; At the last case above, in nonUNA condition, two bound values are compared. 
;;; Then, two anonymous rdf-object are equal as graph structure, whereas the URIs are 
;;; different. 
;;;
;;; Note that the reader macro '\<' gives URI.

(defun %rdf-equalp (x y)
  "this is not revolve version of rdf-equalp."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((equal x y))                      ; cl:strings, symbols, objects, and iri. If both are equal, then equal.
        ((and (numberp x) (numberp y))     ; in case of cl:number, 
         (= x y))                          ; = tests in value space, e.g., 1 and 1.0 is equal
        ((and (uri-p x) (uri-p y) (uri= x y))) ; uri-string equal
        ((and (iri-p x) (iri-p y))                             ; uri-string different but
         (cond ((and *nonUNA* (iri-boundp x) (iri-boundp y))   ; if nonUNA and has value
                (%rdf-equalp (iri-value x) (iri-value y)))     ; then check values
               (t nil)))                                       ; else different uri means different
        ((and (cl:typep x 'rdf:|inLang|) (cl:typep y 'rdf:|inLang|))
         ;; see, http://www.w3.org/TR/2004/REC-rdf-concepts-20040210/#section-Graph-Literal
         (and (equalp (string (lang x)) (string (lang y))) (string= (content x) (content y))))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))   ; "1"^xsd:|integer| /= 1
         (and (eq (class-of x) (class-of y))                         ; "1"^xsd:|integer| /= "1.0"^xsd:|float|
              (%rdf-equalp (value-of x) (value-of y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y) (equal (name x) (name y))) t) ; if names are equal, then equal. name may be a cons
               ((and (not *nonUNA*) (name x) (name y)) nil) ; if UNA and different names then different
               (t                                    ; if nonUNA, check subtree, even though different names or anonymous
                (not (not (rdf-graph-equalp x y))))))
        ((and (consp x) (consp y))
         (and (%rdf-equalp (car x) (car y))
              (%rdf-equalp (cdr x) (cdr y))))))

(defun rdf-equalp (x y)
  "returns true if <x> and <y> is equal in the semantics of RDF(S). 
   This function resolves syntactical difference among symbol, uri, and object identification.
   This function uses <rdf-graph-equalp> internally."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((%rdf-equalp x y))                                    ; same kind objects, cl:strings, cl:number, symbols, objects, and iri.
        ((and (symbolp x) (object? x) (symbolp y) (object? y))
         (rdf-equalp (symbol-value x) (symbol-value y)))
        ((and (symbolp x) (object? x)) (rdf-equalp (symbol-value x) y))
        ((and (symbolp y) (object? y)) (rdf-equalp x (symbol-value y)))
        ((and (iri-p x) (iri-boundp x)) (rdf-equalp (iri-value x) y))
        ((and (iri-p y) (iri-boundp y)) (rdf-equalp x (iri-value y)))
        ((and (consp x) (consp y))
         (and (rdf-equalp (car x) (car y))
              (rdf-equalp (cdr x) (cdr y))))))

#|
(rdf-equalp vin:CaliforniaRegion vin:ItalianRegion)                       -> nil
(let ((*nonUNA* t)) (rdf-equalp vin:CaliforniaRegion vin:ItalianRegion))  -> nil
|#

(defun owl-equalp (x y)
  "This is supplied as a hook for the eqaulity in OWL system. In this RDF module, 
   it is same as <rdf-equalp>. This is overloaded when OWL system is loaded."
  (rdf-equalp x y))

(defun owl-same-p (x y &optional pairs)
  "Hook for OWL. In RDFS module."
  (cond ((equal x y))     ; literals, symbols, objects, uris. See rdfp5a, rdfp5b.
        ((and (stringp x) (stringp y)) nil)
        ((and (numberp x) (numberp y)) nil)
        ((and (uri-p x) (uri-p y) (uri= x y))) ; else next
        ;;
        ((%owl-same-p x y pairs))
        ;;
        ((and (symbolp x) (object? x)) (owl-same-p (symbol-value x) y pairs))
        ((and (symbolp y) (object? y)) (owl-same-p x (symbol-value y) pairs))
        ((and (iri-p x) (slot-boundp x 'value)) (owl-same-p (iri-value x) y pairs))
        ((and (iri-p y) (slot-boundp y 'value)) (owl-same-p x (iri-value y) pairs))))

(defun definitely-%owl-same-p (x y &optional pairs)
  (declare (ignore x y pairs))
  nil)

(defmethod %owl-same-p (x y &optional pairs)
  "Non resolution version. This is used in <owl-equalp> and <owl-equivalent-p>."
  (declare (ignore pairs))
  (%rdf-equalp x y))

;;;
;;;; Equivalency as Class
;;;
;;; <owl-equivalent-p> tests equivalency as class. Namely, the equivalent two object have 
;;; the same class extension, even if the both is not equal as class individual. 
;;; However, if two objects are equal as class individuial, then the both are equivalent as class.

(defun owl-equivalent-p (x y)
  "This is a hook for OWL. In RDFS module, same as <rdf-equalp>."
  (rdf-equalp x y))

(defun make-ordinal-property-from-number (n)
  "returns an ordinal property symbol like 'rdf:_<n>' to which an instance of 
rdfs:ContainerMembershipProperty is bound."
  (let ((role (intern (format nil "_~D" n) :rdf)))
    (unless (and (boundp role)
                 (cl:typep (symbol-value role) 'rdfs:ContainerMembershipProperty))
      (addInstance (list (symbol-value 'rdfs:ContainerMembershipProperty)) role ()))
    role))

(defun collect-container-members (container)
  "<container> is an instance of rdfs:Container. This function retrieves all members in <container>, 
and returns a list of them."
  (loop for i = 1 then (1+ i)
      for role = (make-ordinal-property-from-number i) then (make-ordinal-property-from-number i)
      while (and (slot-exists-p container role) (slot-boundp container role))
      collect (slot-value container role)))

(defun rdf-graph-equalp (x y)
  "This function is effective for instances of rdf:Bag, rdf:Alt, rdf:Seq, rdf:List, and blank nodes.
For rdf:Bag, both member values of <x> and <y> are compared as set. For rdf:Alt, two first member value and two set 
of rest member values are compared. For rdf:Seq, two member values are compared as sequence. In other 
cases, each value is compared with each slot name."
  ;; graph should be asyclic.
  ;; if graph structure is deeply nested, you had better memoize.
  (declare (optimize (speed 3) (safety 0)))
  (when (equal x y) (return-from rdf-graph-equalp (values t t)))
  (cond ((and (cl:typep x 'rdfs:Container) (cl:typep y 'rdfs:Container))
         (let ((xfillers (collect-container-members x))
               (yfillers (collect-container-members y)))
           (cond ((and (null xfillers) (null yfillers)) (values t nil))
                 ((and (cl:typep x (symbol-value 'rdf:|Bag|)) (cl:typep y (symbol-value 'rdf:|Bag|)))
                  (values
                   (and (subsetp xfillers yfillers :test #'(lambda (xfil yfil) (rdf-equalp xfil yfil)))
                        (subsetp yfillers xfillers :test #'(lambda (yfil xfil) (rdf-equalp yfil xfil))))
                   t))
                 ((and (%clos-subtype-p x (symbol-value 'rdf:|Alt|)) (%clos-subtype-p y (symbol-value 'rdf:|Alt|)))
                  (values
                   (and (rdf-equalp (car xfillers) (car yfillers))
                        (subsetp (cdr xfillers) (cdr yfillers) :test #'(lambda (xfil yfil) (rdf-equalp xfil yfil)))
                        (subsetp (cdr yfillers) (cdr xfillers) :test #'(lambda (yfil xfil) (rdf-equalp yfil xfil))))
                   t))
                 ((and (%clos-subtype-p x (symbol-value 'rdf:|Seq|)) (%clos-subtype-p y (symbol-value 'rdf:|Seq|)))
                  (values (every #'rdf-equalp xfillers yfillers) t)))))
        ((owl-equivalent-p (class-of x) (class-of y))
         (let ((graph nil))
           (flet ((role-val-equalp (x1 role1 x2 role2)
                                   (cond ((eq role1 role2)
                                          (cond ((and (slot-boundp x1 role1) (slot-boundp x2 role2))
                                                 (let ((val1 (slot-value x1 role1))
                                                       (val2 (slot-value x2 role2)))
                                                   (cond ((and (null val1) (null val2))
                                                          t)   ; no sub-tree returns t but no graph
                                                         ((and val1 val2)
                                                          (setq graph t) ; sub-trees exist and operated
                                                          (owl-equalp val1 val2))
                                                         (t ; different sub-trees
                                                          (setq graph t)
                                                          nil))))
                                                (t (setq graph t)
                                                   nil)))
                                         )))
             (let* ((roles (collect-prop-names-from (class-of x))) ; class-of x = class-of y
                    (xroles (remove-if-not #'(lambda (role) (slot-boundp x role)) roles))
                    (yroles (remove-if-not #'(lambda (role) (slot-boundp y role)) roles)))
               ;(format t "~%roles:~S xroles:~S yroles:~S" roles xroles yroles)
               (cond ((and xroles yroles)
                      (values 
                       ;; no roles return t, if sub-trees exist, they are compared
                       (and (subsetp xroles yroles
                                     :test #'(lambda (xrole yrole) (role-val-equalp x xrole y yrole)))
                            (subsetp yroles xroles
                                     :test #'(lambda (yrole xrole) (role-val-equalp y yrole x xrole))))
                       graph))
                     ;; no subgraph
                     ((and (name x) (name y))
                      (if (equal (name x) (name y)) (values t t)
                        (values nil t)))
                     ((name x) ; anonymous y
                      (values nil t))
                     ((name y) ; anonymous x
                      (values nil t))
                     (t (values nil t)))))))
        (t (values nil nil))))

(defun owl-equalp-for-refining (x y)
  "<x> and <y> must be clos objects but may be anonymous."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eql x y))
        ((and (datatype-p (class-of x)) (datatype-p (class-of y)))
         (and (eq (class-of x) (class-of y))
              (equalp (value-of x) (value-of y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (cond ((and (name x) (name y))
                (cond ((equal (name x) (name y)) t)        ; if names are equal, then equal. 
                      ;; name may be cons
                      (*nonUNA*
                       (multiple-value-bind (result graph) (rdf-graph-equalp x y)
                         (declare (ignore graph))
                         result))  ; else if nonUNA, check the graph
                      (t nil)))                                      ; else returns nil
               (t (not (not (rdf-graph-equalp x y)))))) ; if anonymous, then check the graph
        ))

(defun rdf-graph-different-p (x y)
  (declare (optimize (speed 3) (safety 0)))
  (when (equal x y) (return-from rdf-graph-different-p (values nil t)))
  (cond ((and (cl:typep x 'rdfs:Container) (cl:typep y 'rdfs:Container))
         (error "Not Yet!")
         (let ((xfillers (collect-container-members x))
               (yfillers (collect-container-members y)))
           (cond ((and (null xfillers) (null yfillers)) (values t nil))
                 ((and (cl:typep x (symbol-value 'rdf:|Bag|)) (cl:typep y (symbol-value 'rdf:|Bag|)))
                  (values
                   (and (subsetp xfillers yfillers :test #'(lambda (xfil yfil) (rdf-equalp xfil yfil)))
                        (subsetp yfillers xfillers :test #'(lambda (yfil xfil) (rdf-equalp yfil xfil))))
                   t))
                 ((and (%clos-subtype-p x (symbol-value 'rdf:|Alt|)) (%clos-subtype-p y (symbol-value 'rdf:|Alt|)))
                  (values
                   (and (rdf-equalp (car xfillers) (car yfillers))
                        (subsetp (cdr xfillers) (cdr yfillers) :test #'(lambda (xfil yfil) (rdf-equalp xfil yfil)))
                        (subsetp (cdr yfillers) (cdr xfillers) :test #'(lambda (yfil xfil) (rdf-equalp yfil xfil))))
                   t))
                 ((and (%clos-subtype-p x (symbol-value 'rdf:|Seq|)) (%clos-subtype-p y (symbol-value 'rdf:|Seq|)))
                  (values (every #'rdf-equalp xfillers yfillers) t)))))
        ((owl-equivalent-p (class-of x) (class-of y))
         (flet ((role-val-equalp (x1 role1 x2 role2)
                                 (and (equivalent-property-p role1 role2)
                                      (cond ((and (slot-boundp x1 role1) (slot-boundp x2 role2))
                                             (let ((val1 (slot-value x1 role1))
                                                   (val2 (slot-value x2 role2)))
                                               (cond ((and (null val1) (null val2))
                                                      t)   ; no sub-tree returns t but no graph
                                                     ((and val1 val2)
                                                      (owl-equalp val1 val2))
                                                     (t ; different sub-trees
                                                      nil))))
                                            (t nil)))))
           (let* ((roles (remove-duplicates
                          (append (collect-prop-names-from (class-of x))
                                  (collect-prop-names-from (class-of y)))))
                  (xroles (remove-if-not #'(lambda (role) (slot-boundp x role)) roles))
                  (yroles (remove-if-not #'(lambda (role) (slot-boundp y role)) roles)))
             ;(format t "~%roles:~S xroles:~S yroles:~S" roles xroles yroles)
             (cond ((and xroles yroles)
                    ;; both subgraph exists, they are compared as set
                    (if (and (subsetp xroles yroles
                                      :test #'(lambda (xrole yrole) (role-val-equalp x xrole y yrole)))
                             (subsetp yroles xroles
                                      :test #'(lambda (yrole xrole) (role-val-equalp y yrole x xrole))))
                        (values nil t)
                      ;; else different
                      (values t t)))
                   (xroles ; no yroles
                    (values t t))
                   (yroles ; no xroles
                    (values t t))
                   ;; no subgraph, both atomic
                   ((and (name x) (name y))
                    (if (equal (name x) (name y))
                        (values nil t)
                      (values t t)))
                   ((name x) ; anonymous y
                    (values t t))
                   ((name y) ; anonymous x
                    (values t t))
                   (t ; both atomic anonymous
                    (values nil nil))))))
        (t (values t t))))

(defun equivalent-property-p (x y)
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
        ))

;;;
;;;; Disjointness
;;;
;;; CLOS classes are pairwise disjoint in ANSI Common Lisp, unless they have a common subclass 
;;; or one class is a subclass of the other. This agreement is supported by the premise that an 
;;; object in CLOS is typed to only one class. In the RDF universe, an entity can be typed to 
;;; multiple classes. So, the nature of disjointness in CLOS is not applicable in the RDF 
;;; universe. However, in SWCLOS, the pseudo multiple-classing is implemented by the special 
;;; mechanism called shadowed-class using CLOS class and metaclass mechanism. Therefore, from 
;;; the viewpoint of CLOS, the algorithm of disjointness for CLOS is also applicable in the RDF 
;;; universe in the virtue of CLOS.
;;;
;;; See also the explanation in OWL module.
#|
(defun disjoint-p (c d)
  "collects all subclasses of <c> and <d>, and checks the class-subclass relation between 
   a subclass of <c> and a subclass f <d>. If the relation is found, it implies <c> and <d> 
   shares intersection of the class extension of <c> and <d>. Then, immediately returns with value false.
   If every subclass pairs between one from <c> and one from <d> is not in class-subclass relation, 
   finally t is returned."
  ;; This function is used in %rdf-subtypep.
  ;; xsd:|decimal| -+- xsd:|integer| +- xsd:|long| -- xsd:|int| -- xsd:|short| -- xsd:|byte|
  ;;              +- xsd:|nonPositiveInteger| -- xsd:|negativeInteger|
  ;;              +- xsd:|nonNegativeInteger| -+-  xsd:|positiveInteger|
  ;;                                         +-- xsd:|unsignedLong| - xsd:|unsignedInt| - xsd:|unsignedShort| - xsd:|unsignedByte|
  (cond ((eq c d) (values nil t))
        ((eq c rdfs:Resource) (values nil t))         ; in RDF universe
        ((eq d rdfs:Resource) (values nil t))
        ((or (and (eq c (symbol-value 'xsd:|nonPositiveInteger|)) (eq d (symbol-value 'xsd:|nonNegativeInteger|)))
             (and (eq d (symbol-value 'xsd:|nonPositiveInteger|)) (eq c (symbol-value 'xsd:|nonNegativeInteger|))))
         ;; both shares zero
         (values nil t))
        ((or (and (%clos-subtype-p c (symbol-value 'xsd:|integer|))
                  (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|)))
             (and (%clos-subtype-p d (symbol-value 'xsd:|integer|))
                  (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|)))
             (and (%clos-subtype-p c (symbol-value 'xsd:|integer|))
                  (%clos-subtype-p d (symbol-value 'xsd:|nonNegativeInteger|)))
             (and (%clos-subtype-p d (symbol-value 'xsd:|integer|))
                  (%clos-subtype-p c (symbol-value 'xsd:|nonNegativeInteger|))))
         (values nil t))
        ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|))
                  (%clos-subtype-p d (symbol-value 'xsd:|positiveInteger|)))
             (and (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|))
                  (%clos-subtype-p c (symbol-value 'xsd:|positiveInteger|))))
         (values nil t))
        ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|))
                  (%clos-subtype-p d (symbol-value 'xsd:|unsignedLong|)))
             (and (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|))
                  (%clos-subtype-p c (symbol-value 'xsd:|unsignedLong|))))
         (values nil t))
        ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonNegativeInteger|))
                  (%clos-subtype-p d (symbol-value 'xsd:|negativeInteger|)))
             (and (%clos-subtype-p d (symbol-value 'xsd:|nonNegativeInteger|))
                  (%clos-subtype-p c (symbol-value 'xsd:|negativeInteger|))))
         (values nil t))
        (t (loop for csub in (cons c (collect-all-subs c)) with dsubs = (cons d (collect-all-subs d))
               do (loop for dsub in dsubs
                      when (or (%clos-subtype-p csub dsub) (%clos-subtype-p dsub csub))
                      do (return-from disjoint-p (values nil t))))
           (values t t))))
|#
(defun disjoint-p (c d)
  "returns true if <c> and <d> are disjoint in OWL."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq c d) (values nil t))
        ((eq c rdfs:Resource) (values nil t))         ; in RDF universe
        ((eq d rdfs:Resource) (values nil t))
        (t (%disjoint-p c d))))
#|
(defun %disjoint-p (c d)
  "In CLOS classes, two classes are disjoint, unless they have a common subclass."
  (not (intersection (cons c (collect-all-subs c)) (cons d (collect-all-subs d))
                     :test #'(lambda (cc dd)
                               (or (%clos-subtype-p cc dd) (%clos-subtype-p dd cc))))))
|#
(defun check-instance-sharing (c1 c2 &optional visited)
  "checks disjointWith constraint of <c1> against <c2> and its subclasses."
  (cond ((member c2 visited) nil)
        ((or (%clos-subtype-p c1 c2) (%clos-subtype-p c2 c1)) c2)
        ((some #'(lambda (sub) (check-instance-sharing c1 sub (cons c2 visited)))
               (mop:class-direct-subclasses c2)))
        ))
(defun %disjoint-p (c d)
  (cond ((and (rdf-class-p c) (rdf-class-p d))
         (cond ((or (%clos-subtype-p c d) (%clos-subtype-p d c))
                (values nil t))  ; xsd datatype is also reasoned.
               ((check-instance-sharing c d)
                (values nil t))
               ;; rdf disjoint part
               ((or (and (eq c (symbol-value 'xsd:|nonPositiveInteger|)) (eq d (symbol-value 'xsd:|nonNegativeInteger|)))
                    (and (eq d (symbol-value 'xsd:|nonPositiveInteger|)) (eq c (symbol-value 'xsd:|nonNegativeInteger|))))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:|integer|))
                         (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:|integer|))
                         (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|)))
                    (and (%clos-subtype-p c (symbol-value 'xsd:|integer|))
                         (%clos-subtype-p d (symbol-value 'xsd:|nonNegativeInteger|)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:|integer|))
                         (%clos-subtype-p c (symbol-value 'xsd:|nonNegativeInteger|))))
                (values nil t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|))
                         (%clos-subtype-p d (symbol-value 'xsd:|positiveInteger|)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|))
                         (%clos-subtype-p c (symbol-value 'xsd:|positiveInteger|))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonPositiveInteger|))
                         (%clos-subtype-p d (symbol-value 'xsd:|unsignedLong|)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:|nonPositiveInteger|))
                         (%clos-subtype-p c (symbol-value 'xsd:|unsignedLong|))))
                (values t t))
               ((or (and (%clos-subtype-p c (symbol-value 'xsd:|nonNegativeInteger|))
                         (%clos-subtype-p d (symbol-value 'xsd:|negativeInteger|)))
                    (and (%clos-subtype-p d (symbol-value 'xsd:|nonNegativeInteger|))
                         (%clos-subtype-p c (symbol-value 'xsd:|negativeInteger|))))
                (values t t))
               ;; implicit disjointness of datatype
               ((and (%clos-subtype-p c xsd:|anySimpleType|)
                     (%clos-subtype-p d xsd:|anySimpleType|))
                (loop for csub in (cons c (collect-all-subs c)) with dsubs = (cons d (collect-all-subs d))
                    do (loop for dsub in dsubs
                           when (or (%clos-subtype-p csub dsub) (%clos-subtype-p dsub csub))
                           do (return-from %disjoint-p (values nil t))))
                (values t t))
               ;; end of rdf disjoint part
               (t (values nil nil))))
        ((and (rdf-instance-p c) (rdf-instance-p d))
         ;; for transitive property, instances have subsumption
         (if gx::*autoepistemic-local-closed-world*
             (cond ((or (subsumed-p c d) (subsumed-p d c))
                    (values nil t))
                   (t (values t t)))
           (values nil nil)))
        (t (typecase c
             (forall (typecase d
                       (forall (error "Not Yet!"))
                       (exists (error "Not Yet!"))
                       (fills (error "Not Yet!"))
                       (rdfs:Class (disjoint-p (forall-filler c) d))
                       (rdfs:Resource (error "Not Yet!"))
                       (t nil)))
             (exists (typecase d
                       (forall (error "Not Yet!"))
                       (exists (error "Not Yet!"))
                       (fills (error "Not Yet!"))
                       (rdfs:Class (error "Not Yet!"))
                       (rdfs:Resource (error "Not Yet!"))
                       (t nil)))
             (fills (typecase d
                          (forall (error "Not Yet!"))
                          (exists (error "Not Yet!"))
                          (fills (cond ((and (equivalent-property-p (fills-role c) (fills-role d))
                                                 (disjoint-p (fills-filler c) (fills-filler d)))
                                            (values t t))
                                           (t (values nil t))))
                          (rdfs:Class (error "Not Yet!"))
                          (rdfs:Resource (error "Not Yet!"))
                          (t nil)))
             (rdfs:Class (typecase d
                           (forall (disjoint-p c (forall-filler d)))
                           (exists (error "Not Yet!"))
                           (fills (error "Not Yet!"))
                           (rdfs:Class (error "Not Yet!"))
                           (t nil)))
             (rdfs:Resource (typecase d
                              (forall (error "Not Yet!"))
                              (exists (error "Not Yet!"))
                              (fills (error "Not Yet!"))
                              (rdfs:Class (error "Not Yet!"))
                              (rdfs:Resource (error "Not Yet!"))
                              (t nil)))
             (t nil)
             ))))

(defun collect-all-subs (class)
  "returns all subclasses of <class> but <class> itself. 
   Note that this function uses only <mop:class-direct-subclasses>."
  (remove-duplicates 
   (append (mop:class-direct-subclasses class)
           (loop for sub in (mop:class-direct-subclasses class)
               append (collect-all-subs sub)))))

;;;
;;;; Subtypep Utilities
;;;

;; The following code is partly from ACL.
(defun %clos-subtype-p (c1 c2)
  "returns true if CLOS metaobject <c1> eql <c2> or <c1> is a subtype of <c2> 
   using the class precedence list (cpl) of <c1>. This is more efficient than (<subtypep> <c1> <c2>)
   when <c1> and <c2> are CLOS objects."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eql c1 c2))
        ((eql c2 excl::*the-class-t*))
        ((and (eql (class-of c1) rdfs:Class)
              (or (eql c2 rdfs:Resource) (eql c2 |rdfs:Resource|)))
         t)
        ((and (or (eql c1 rdfs:Resource) (eql c1 |rdfs:Resource|))
              (eql (class-of c2) rdfs:Class))
         nil)
        ((mop:class-finalized-p c1)
         (cond ((member c2 (mop:class-precedence-list c1) :test #'eq) t)
               (t nil)))
        ((labels ((walk-partial-cpl (c)
                                    ;(format t "~%walk-partial-cpl ~S" c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member c2 supers :test #'eq)
                        (return-from %clos-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl c1)
           nil))))

(defun %resource-subtype-p (class)
  "same as (<subtypep> <class> <rdfs:Resource>) in CLOS, but more efficient."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class rdfs:Resource))
        ((eq class |rdfs:Resource|))
        ((mop:class-finalized-p class)
         (cond ((member rdfs:Resource (mop:class-precedence-list class) :test #'eq) t)
               (t nil)))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member rdfs:Resource supers :test #'eq)
                        (return-from %resource-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))

(defun %rdf-class-subtype-p (class)
  "same as (<subtypep> <class> <rdfs:Class>) in CLOS, but more efficient."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class rdfs:Class))
        ((mop:class-finalized-p class)
         (cond ((member rdfs:Class (mop:class-precedence-list class) :test #'eq) t)
               ((and (string= (symbol-name (class-name class)) "Class")
                     (string= (package-name (symbol-package (class-name class))) "owl"))
                (cerror "Anyway continue?" "Maybe OWL module is not loaded!")
                (reinitialize-instance class :direct-superclasses `(,rdfs:Class)))
               (t nil)))
        ((labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member rdfs:Class supers :test #'eq)
                        (return-from %rdf-class-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))))

;;;
;;;; Forall, Exists, and Fills for Slot Definition Type Option
;;;
;;; In addition to concepts (classes) of RDFS and OWL, following CLOS classes may be 
;;; set into the type option of slot definition objects and used in subsumption computation
;;; between slot-type-constraints in subsumees and restrictions in subsumers.

(defclass slot-type-constraint (cl:standard-class)
  ((subject-type :initarg :subject-type :accessor slot-subject-type))
  (:metaclass cl:standard-class)
  (:documentation "This is the top abstract constraint for slot type constraints.
A subclass of this class is a metaclass.")
  )

;;;
;;; To compare slot-type-constraints with t (which is supplied by system as default type option)
;;; using cl:subtypep in the standard protocol of mop:compute-effective-slot-definition, 
;;; we needed slot type constraints as type or class, or else the default value t remains in 
;;; the type slot option together with slot type constraints. So, we needed following metaclasses 
;;; to create type slot constraints as class so as to eliminate t from type options in slot definitions.
;;;

(defclass forall (slot-type-constraint)
  ((role :initarg :role :accessor forall-role)
   (filler :initarg :filler :accessor forall-filler))
  (:metaclass cl:standard-class)
  (:documentation "type slot constraint metaclass for universal value constraints"))
(defclass exists (slot-type-constraint)
  ((role :initarg :role :accessor exists-role)
   (filler :initarg :filler :accessor exists-filler))
  (:metaclass cl:standard-class)
  (:documentation "type slot constraint metaclass for full existential constraints"))
(defclass fills (slot-type-constraint)
  ((role :initarg :role :accessor fills-role)
   (filler :initarg :filler :accessor fills-filler))
  (:metaclass cl:standard-class)
  (:documentation "type slot constraint metaclass for filler constraints"))

(defmethod print-object ((obj forall) stream)
  (cond ((and (forall-role obj)
              (forall-filler obj))
         (print-unreadable-object (obj stream :type t)
           (format stream "~S ~S"
             (forall-role obj)
             (or (name (forall-filler obj))
                 (get-form (forall-filler obj))))))
        (t (call-next-method))))
(defmethod print-object ((obj exists) stream)
  (cond ((and (exists-role obj)
              (exists-filler obj))
         (print-unreadable-object (obj stream :type t)
           (format stream "~S.~S"
             (exists-role obj)
             (or (name (exists-filler obj))
                 (get-form (exists-filler obj))))))
        (t (call-next-method))))
(defmethod print-object ((obj fills) stream)
  (cond ((and (fills-role obj)
              (fills-filler obj))
         (print-unreadable-object (obj stream :type t)
           (format stream "~S.~S"
             (fills-role obj)
             (or (name (fills-filler obj))
                 (get-form (fills-filler obj))))))
        (t (call-next-method))))

;;;
;;;; Subtyping from Slot Type Option
;;;
#+never
(defun strict-subtype-p-for-slotd-type (c1 c2)
  "This function understands forall, exists, and fills constraint in addition to OWL concepts.
   This function is used for slot definition type reduction in most-specific-concepts-for-slotd-type. 
   If unknown, this function returns nil."
  ;; Note that equivalent-classes and same individuals are not included (c1 c2).
  ;; If returns true, then c2 will disappear in slotd type option.
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq c1 t) nil)          ; then c2 remains
        ((eq c2 t) t)               ; then c2 disappears
        ((equal c1 c2) nil)      ; accept list structure
        ((and (atom c1) (atom c2))
         (%strict-subtype-p-for-slotd-type c1 c2))
        ((and (consp c1) (consp c2))
         (case (op c1)
           (and (case (op c2)
                  (and (error "Not Yet!"))
                  (or (error "Not Yet!"))
                  (not (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (or (case (op c2)
                 (and (error "Not Yet!"))
                 (or (error "Not Yet!"))
                 (not (error "Not Yet!"))
                 (otherwise (error "Not Yet!"))))
           (not (case (op c2)
                  (and (error "Not Yet!"))
                  (or (error "Not Yet!"))
                  (not (error "Not Yet!"))
                  (otherwise (error "Not Yet!"))))
           (otherwise (case (op c2)
                        (and (error "Not Yet!"))
                        (or (error "Not Yet!"))
                        (not (error "Not Yet!"))
                        (otherwise (error "Not Yet!"))))))
        ((consp c1)
         (case (op c1)
           (and (error "Not Yet!"))
           (or (error "Not Yet!"))
           (not (error "Not Yet!"))
           (otherwise (error "Not Yet!"))))
        ((consp c2)
         (case (op c2)
           (and (error "Not Yet!"))
           (or (error "Not Yet!"))
           (not (error "Not Yet!"))
           (otherwise (error "Not Yet!"))))
        (t (error "Cant happen!"))))

;;;
;;; From range constraints, C(y) comes up as this role extension R(x,y), here x is subjective object, and x is slot value.
;;; From allValuesFrom,  R(x,y)->C(y) comes up as this role extension R(x,y).
;;; From someValuesFrom, R(x,y)^C(y)  comes up as this role extension R(x,y).
;;; From hasValue, R:b,  R(x,b)       comes up as this role extension R(x,b).
;;;
#+never
(defun %strict-subtype-p-for-slotd-type (c1 c2)
  "<c1> and <c2> are a term from slotd type option."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (owl-class-p c1) (owl-class-p c2))
         (if (or (owl-equivalent-p c1 c2)
                 (owl-same-p c1 c2))
             nil                                   ; strict
           (%subsumed-p-without-equivalency c1 c2)))
        ((and (rdf-class-p c1) (rdf-class-p c2))
         (if (rdf-equalp c1 c2) nil                  ; strict
           (%rdf-subtypep c1 c2)))
        ((equal c1 c2) nil)                          ; strict
        (t ; combination with slot type constraints
         (etypecase c1
           (forall (etypecase c2
                     (forall (subsumed-p (forall-filler c1) (forall-filler c2))) ; 
                     (exists nil)       ; c2 remains    R(x,y)->C(y) vs. R(x,y)^D(y)
                     (fills                                            ; R(x,y)->C(y) vs. R(x,{b})
                      (if (typep (fills-filler c2) (forall-filler c1)) ; b . C
                          t nil))
                     (rdfs:Class                           ; R(x,y)->C(y) vs. R(x,y)^D(y)
                      nil)))             ; c2 remains
           (exists (etypecase c2
                     (exists (subsumed-p (exists-filler c1) (exists-filler c2)))
                     (forall nil)        ; c2 remains then both remains
                     (fills (if (typep (fills-filler c2) (exists-filler c1)) ; R(x,y)^C(y) vs. R(x,{b})
                                    t nil))
                     (rdfs:Class nil)))
           (fills (etypecase c2
                        (fills (subsumed-p (fills-filler c1) (fills-filler c2)))    ; sub object remains
                        (forall nil)                                              ; R(x,{a}) vs. R(x,y)->D(y)
                        (exists (if (typep (fills-filler c1) (exists-filler c2)) ; R(x,{a}) vs. R(x,y)^D(y)
                                    t nil))
                        (rdfs:Class nil)))                                        ; R(x,{a}) vs. {a}.D
           (rdfs:Class (etypecase c2
                         (forall (if (subsumed-p c1 (forall-filler c2))  ; C(y) ? D(y) for R(x,y)->D(y)
                                     t nil))
                         (exists (if (subsumed-p c1 (exists-filler c2))  ; C(y) ? D(y) for R(x,y)^D(y)
                                     t nil))
                         (fills nil)                              ; R(x,{b}) vs. b.C
                         (rdfs:Class (error "Can't happen!"))))))))

(defun strict-subsumed-p (c1 c2)
  (and (not (owl-same-p c1 c2))
       (subsumed-p c1 c2)))

(defun clos-strict-supertype-p (c2 c1)
  "returns true if <c1> is a supertype of <c2> in CLOS but not equal."
  (declare (inline))
  (and (not (eql c2 c1))
       (%clos-subtype-p c1 c2)))
#+never
(defun strict-supertype-p-for-slotd-type (c2 c1)
  "This function understands forall, exists, and has expressions in addition to subtypep."
  (declare (inline))
  (strict-subtype-p-for-slotd-type c1 c2))

(defun rdf-resolve (x &optional head-adding-stuff)
  "transforms any element in tree <x> to rdf object from QName symbol and uri, if possible."
  (typecase x
    (symbol (if (object? x) (symbol-value x) x))
    (iri (if (iri-value x) (iri-value x) x))
    (cons (if head-adding-stuff (cons head-adding-stuff (reuse-cons (rdf-resolve (car x)) (rdf-resolve (cdr x)) x))
            (reuse-cons (rdf-resolve (car x)) (rdf-resolve (cdr x)) x)
            ))
    (otherwise x)))

;;;
;;;; Subtypep in Semantics of RDF(S)
;;;
;;; To test rdfs:subClassOf relation including the transitivity, use <rdf-subtypep> for RDF(S) semantics.
;;; Note that the counterpart of <rdf-subtypep> in OWL semantics is <subsumed-p>.
;;; 
;;; Note that <cl:subtypep> in Common Lisp returns two values, e.g., value1 and value2.
;;; If value1 is true, then value2 is definitely true. So, a pair \<t nil\> never happens.
;;; The following table is taken from ANSI Common Lisp specs.
;;; ----------------------------------------------------------------------------------
;;;  value1   value2   meaning
;;; ---------------------------------------------------------------------
;;;  true     true    type1 is definitely a subtype of type2.
;;;  false    true    type1 is definitely not a subtype of type2.
;;;  false    false   subtypep could not determine the relationship, 
;;;                   so type1 might or might not be a subtype of type2. 
;;; ---------------------------------------------------------------------
;;; ----------------------------------------------------------------------------------
;;; See, http://www.franz.com/support/documentation/8.1/ansicl/dictentr/subtypep.htm
;;;
;;; We carry out this semantics in <subtypep>, <rdf-subtypep>, and <subsumed-p>.
;;; We capture \<true true\> is true value (expressed as T), \<false true\> is false value (expressed as F), 
;;; and \<false false\> is unknown value (expressed as U) in RDF(S) and OWL semantics. 

(defun subtypep (type1 type2)
  "subtype relation in RDF(S) is equivalent to CLOS except URIs and QName symbol resolution.
   Therefore, after resolving URIs and QNames this function calls cl:subtypep."
  (setq type1 (rdf-resolve type1))
  (setq type2 (rdf-resolve type2))
  (cond ((or (rdf-instance-p type1) (rdf-instance-p type2))
         (error 'rdf-type-error
           :format-control "SUBTYPEP ~S ~S"
           :format-arguments `(,type1 ,type2)))
        (t (labels ((normal-type (x)
                                 (cond ((typep x 'rdfs:Datatype)
                                        (type-form x))
                                       ((consp x)
                                        (case (op x)
                                          (and `(and ,@(mapcar #'normal-type (args x))))
                                          (or `(or ,@(mapcar #'normal-type (args x))))
                                          (not `(not ,@(normal-type (arg1 x))))
                                          (otherwise x)))
                                       (t x))))
             (cl:subtypep (normal-type type1) (normal-type type2))))))

(export 'subtypep)

(defun subsumed-p (type1 type2)
  "This is a hook for OWL. It is same as <rdf-subtypep> in RDFS."
  (setq type1 (rdf-resolve type1))
  (setq type2 (rdf-resolve type2))
  (cond ((or (rdf-instance-p type1) (rdf-instance-p type2))
         (error 'rdf-type-error
           :format-control "SUBTYPEP ~S ~S"
           :format-arguments `(,type1 ,type2)))
        (t (%rdf-subtypep type1 type2))))

;;;
;;; Function <subtypep> produces same logic as CLOS. However, in OWL semantics, it is wrong in reality. 
;;; More precisely, CLOS adopts Unique Name Assumption and do not have the explicit notion of disjointness. 
;;; Thus, if two classes are independent (not in relation of super-sub classes), then they seems to be disjoint. 
;;; However, if concept C and (not D) is compared in Allegro Common Lisp, ACL tends to fall into conclusion unknown. 
;;; It seems that CLOS does not have clear semantics on class extension. In RDF semantics, 
;;; every entity is a member of RDF universe, then the complement of xsd:|integer| or (not xsd:|integer|) extends out 
;;; of xsd:|integer| extension and xsd:|decimal| in RDF universe. For example, an instance of vin:Wine is not an 
;;; instance of xsd:|integer|. Therefore, it is obvious that (not xsd:|integer|) is not a subclass of xsd:|integer| and 
;;; xsd:|decimal|. However, cl:subtypep does not infer it (at least for ACL). 
;;;
;;; Furthermore, we have notions of equivalency and disjointness on class relation in OWL. 
;;; Therefore, the equivalency and disjointness must be checked in addition of super-sub class relation. 
;;; Specifically, if SWCLOS cannot infer two concepts are in relations of rdfs:subClassOf (and owl:intersectionOf, 
;;; owl:unionOf in OWL), then the relation of owl:equivalentClass, owl:disjointWith, owl:complementOf, owl:sameAs, 
;;; owl:differentFrom must be checked. Function <subsumed-p> returns unknown value only if there is no information of 
;;; super-sub class relation, equality, and disjointness. See <subsumed-p> in OWL module.
;;;
;;; To compute <rdf-subtypep> and <subsumed-p>, which takes care of unknown, 
;;; the following ternary truth table is used for T, F, and U.
;;; ----------------------------------------------------------------------------------
;;;  Ternary Truth Table 
;;; ----------------------
;;;     conjunction
;;;     |  T    U    F
;;;  ---+--------------    e.g.,
;;;   T |  T    U    F     True  ^ Unknown  => Unknown
;;;   U |  U    U    F
;;;   F |  F    F    F     False ^ Unknown  => False
;;;
;;;     disjunction    
;;;     |  T    U    F
;;;  ---+--------------    e.g.,
;;;   T |  T    T    T     True  v Unknown  => True
;;;   U |  T    U    U
;;;   F |  T    U    F     False v Unknown  => Unknown
;;;
;;;      negation
;;;    x | T    U    F
;;;  ----+--------------    e.g.,
;;;   ~x | F    U    T     ~Unkonw => Unkown
;;; ----------------------------------------------------------------------------------
;;; The following table shows rewriting rules for inclusiveness.
;;; Where '&lt;' stands for subtype relation. '^' means conjunction and 'v' means disjunction. 
;;; ----------------------------------------------------------------------------------
;;;      C < (A ^ B)  <=>  (C < A) ^ (C < B)
;;;      C < (A v B)  <=>  (C < A) v (C < B)
;;;      (A ^ B) < C  <=>  (A < C) v (B < C)
;;;      (A v B) < C  <=>  (A < C) ^ (B < C)
;;;
;;; (A v B) < (C ^ D) <=>  (A < C) ^ (A < D)  ^  (B < C) ^ (B < D) 
;;; (A ^ B) < (C v D) <=>  (A < C) v (A < D)  v  (B < C) v (B < D) 
;;; (A ^ B) < (C ^ D) <=> ((A < C) ^ (A < D)) v ((B < C) ^ (B < D))
;;; (A v B) < (C v D) <=> ((A < D) v (A < D)) ^ ((B < C) v (B < D))
;;;
;;;           ~A < ~B <=> B < A
;;; ----------------------------------------------------------------------------------
;;; <rdf-subtypep> has almost same logic as <subtypep> but it is only different with 
;;; respect to known/unknown. See following example.
;;; ----------------------------------------------------------------------------------
;;; (rdf-subtypep xsd:|long| xsd:|decimal|)         -> <t t>
;;; (subtypep xsd:|long| xsd:|decimal|)             -> <t t>
;;; (rdf-subtypep `(not ,xsd:|long|) xsd:|decimal|) -> <nil t>
;;; (subtypep `(not ,xsd:|long|) xsd:|decimal|)     -> <nil nil>
;;; (rdf-subtypep xsd:|decimal| `(not ,xsd:|long|)) -> <nil t>
;;; (subtypep xsd:|decimal| `(not ,xsd:|long|))     -> <nil nil>
;;; ----------------------------------------------------------------------------------
;;; In this example, xsd:|long| is subsumed by and not equal to xsd:|decimal|. Therefore, 
;;; there is the intersection between xsd:|long| and xsd:|decimal|.
;;; The complement of xsd:|long| extends over the boundary of xsd:|decimal|, then the extension 
;;; of (not xsd:|long|) is not included by the extension of xsd:|decimal|, and the extension of 
;;; the intersection between xsd:|long| and xsd:|decimal| is not included by (not xsd:|long|).
;;;
;;; Function <rdf-subtypep> obeys the set-theoretic semantics of RDF(S), but <subtypep> does not follow 
;;; the semantics of RDF(S) exactly. Even so, there is no difference between both, if only the 
;;; first value is used in program.

(defun rdf-subtypep (type1 type2)
  "returns true if <type1> is subsumed by <type2> in the sense of RDF(S).
   URIs and QName symbols are acceptable and resolved into resource objects. 
   Instances of Datatypes are also acceptable."
  (setq type1 (rdf-resolve type1))
  (setq type2 (rdf-resolve type2))
  (%rdf-subtypep type1 type2))
(defun %rdf-subtypep (type1 type2)
  "same as rdf-subtypep but <type1> and <type2> must be an RDF object or cons."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((and (atom type1) (atom type2))
         (cond ((eq type1 type2) (values t t))
               ((and (rdf-class-p type1) (rdf-class-p type2))
                (cond ((%clos-subtype-p type1 type2) (values t t))
                      (t (values nil t))))
               ((or (rdf-instance-p type1) (rdf-instance-p type2))
                (error 'rdf-type-error
                  :format-control "SUBTYPEP ~S ~S"
                  :format-arguments `(,type1 ,type2)))
               (t (values nil nil))))
        ((and (atom type1) (consp type2))
         (case (op type2)
           ((not and or)
            (setq type2 (->nnf type2))
            (ecase (op type2)
              (and (loop for t2 in (args type2)    ; C < (A ^ B)  <=>  (C < A) ^ (C < B)
                       do (multiple-value-bind (val1 val2) (%rdf-subtypep type1 t2)
                            ;; if false known (nil t) immediately return with false known
                            ;; if unknown (nil nil) immediately return with unknown
                            (when (null val1) (return-from %rdf-subtypep (values nil val2)))))
                   (values t t))
              (or (loop for t2 in (args type2) with known = t ; C < (A v B)  <=>  (C < A) v (C < B)
                      do (multiple-value-bind (val1 val2) (%rdf-subtypep type1 t2)
                           ;; if true then immediately return with true
                           (when val1 (return-from %rdf-subtypep (values t t)))
                           ;; else accumulate known, all false known (nil t) results false known.
                           (setq known (and known val2)))
                      finally ; pass false known/unknown
                        (return (values nil known))))
              (not (let ((ty2 (arg1 type2)))
                     (cond ((rdf-equalp type1 ty2)    (values nil t)) ; x and (not x), disjoint
                           ((rdf-equalp type1 rdfs:Resource) (values t t))
                           ((%rdf-subtypep type1 ty2) (values nil t)) ; type1 is included in t2, disjoint
                           ((%rdf-subtypep ty2 type1) (values nil t)) ; (not ty2) = (not type1) U (type1 - ty2)
                           ((disjoint-p type1 ty2)    (values t t))
                           (t (values nil nil)))))))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (%rdf-subtypep type1 (cons 'and type2)))))
        ((and (consp type1) (atom type2))
         (case (op type1)
           ((not and or)
            (setq type1 (->nnf type1))
            (ecase (op type1)
              (and ;; => (or (%rdf-subtypep t11 type2) (%rdf-subtypep t12 type2) ...)
               (loop for t1 in (args type1) with known = t
                   do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                        (when val1 (return-from %rdf-subtypep (values t t)))
                        (setq known (and known val2)))
                   finally
                     (return (values nil known))))
              (or ;; => (and (%rdf-subtypep t11 type2) (%rdf-subtypep t12 type2) ...)
               (loop for t1 in (args type1)
                   do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                        (when (null val1) (return-from %rdf-subtypep (values nil val2)))))
               (values t t))
              (not (let ((ty1 (arg1 type1)))
                     (cond ((rdf-equalp ty1 type2) (values nil t))     ; (not x) and x, disjoint
                           ((rdf-equalp type2 rdfs:Resource) (values t t))
                           ((%rdf-subtypep ty1 type2) (values nil t))  ; (not ty1) = (not type2) U (type2 - ty1)
                           ((%rdf-subtypep type2 ty1) (values nil t))  ; disjoint
                           ((disjoint-p ty1 type2) (values t t))
                           (t (values nil nil)))))))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (%rdf-subtypep (cons 'and type1) type2))))
        ((and (consp type1) (consp type2))
         (case (op type1)
           ((not and or) (setq type1 (->nnf type1)))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (setq type1 (->nnf (cons 'and type1)))))
         (case (op type2)
           ((not and or) (setq type2 (->nnf type2)))
           ((forall exists fills) (error "Cant happen!"))
           (otherwise (setq type2 (->nnf (cons 'and type2)))))
         (cond ((and (eq (op type1) 'not) (eq (op type2) 'not))
                (%rdf-subtypep (arg1 type2) (arg1 type1)))      ; ~A < ~B <=> B < A
               ;;
               ((and (eq (op type1) 'not) (eq (op type2) 'and)) ; C < (A ^ B)  <=>  (C < A) ^ (C < B)
                ;; (%rdf-subtypep type1 (and t1 t2 ...))
                (loop for t2 in (args type2)
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep type1 t2)
                         (when (null val1) (return-from %rdf-subtypep (values nil val2)))))
                (values t t))
               ((and (eq (op type1) 'not) (eq (op type2) 'or)) ; C < (A v B)  <=>  (C < A) v (C < B)
                ;; (%rdf-subtypep type1 (or t1 t2 ...))
                (loop for t2 in (args type2) with known = t
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep type1 t2)
                         (when val1 (return-from %rdf-subtypep (values t t)))
                         (setq known (and known val2)))
                    finally ; pass false known/unknown
                      (return (values nil known))))
               ;;
               ((and (eq (car type1) 'or) (eq (car type2) 'not)) ; (A v B) < C  <=>  (A < C) ^ (B < C)
                ;; (%rdf-subtypep (or t1 t2 ...) type2)
                (loop for t1 in (cdr type1)
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                         (when (null val1) (return-from %rdf-subtypep (values nil val2)))))
                (values t t))
               ((and (eq (car type1) 'and) (eq (car type2) 'not)) ; (A ^ B) < C  <=>  (A < C) v (B < C)
                ;; (%rdf-subtypep (and t1 t2 ...) type2)
                (loop for t1 in (cdr type1) with known = t
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                         (when val1 (return-from %rdf-subtypep (values t t)))
                         (setq known (and known val2)))
                    finally
                      (return (values nil known))))
               ;;
               ((and (eq (op type1) 'or) (eq (op type2) 'and))
                ;; (A v B) < (C ^ D) <=>  (A < C) ^ (A < D)  ^  (B < C) ^ (B < D) 
                (loop for t1 in (args type1)
                    do (loop for t2 in (args type2)
                           do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 t2)
                                (when (null val1) (return-from %rdf-subtypep (values nil val2))))))
                (values t t))
               ((and (eq (op type1) 'and) (eq (op type2) 'or))
                ;; (A ^ B) < (C v D) <=>  (A < C) v (A < D)  v  (B < C) v (B < D) 
                (loop for t1 in (args type1) with known = t
                    do (loop for t2 in (args type2)
                           do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 t2)
                                (when val1 (return-from %rdf-subtypep (values t t)))
                                (setq known (and known val2))))
                    finally
                      (return (values nil known))))
               ((and (eq (op type1) 'and) (eq (op type2) 'and))
                (loop for t1 in (args type1) with known = t
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                         (when val1 (return-from %rdf-subtypep (values t t)))
                         (setq known (and known val2)))
                    finally
                      (return (values nil known))))
               ((and (eq (car type1) 'or) (eq (car type2) 'or))
                (loop for t1 in (cdr type1)
                    do (multiple-value-bind (val1 val2) (%rdf-subtypep t1 type2)
                         (when (null val1) (return-from %rdf-subtypep (values nil val2)))))
                (values t t))
               ((error "Cant happen!"))))))

;;;
;;;; Most Specific Concepts (MSCs)
;;;
;;; Function <most-specific-concepts> is used to compute the most specific concepts among classes.
;;; For example, xsd:|integer| is a superclass of xsd:|int|, and xsd:|nonNegativeInteger| is a superclass 
;;; of xsd:|positiveInteger|, then those superclasses disappear in the most specific concepts among them.
;;; ----------------------------------------------------------------------------------
;;; (most-specific-concepts
;;;   (list xsd:|integer| xsd:|int| xsd:|positiveInteger|
;;;            xsd:|nonNegativeInteger| xsd:|unsignedInt|))
;;; -> (#<rdfs:Datatype xsd:|unsignedInt|> #<rdfs:Datatype xsd:|positiveInteger|>
;;;     #<rdfs:Datatype xsd:|int|>)
;;; ----------------------------------------------------------------------------------

(defun most-specific-concepts (classes)
  "returns the most specific concepts in RDF(S) and OWL semantics, or classes minus duplicates 
   and superclasses of other classes in <classes>. This function internally uses <subsumed-p> and 
   <owl-equivalent-p>. Note that this function does not check disjointness of classes. 
   <subsumed-p> is equal to <rdf-subtypep>, <owl-equivalent-p> is same 
   as <rdf-equalp> in RDF(S) module. Then, OWL module overwrites them. 
   This function allows <|rdfs:Resource|>, a temporal alternative of rdfs:Resource, and can accepts
   cons concepts as class. This function is taken from Memory Organization Package by Schank."
  (declare (optimize (speed 3) (safety 0)))
  (when classes
    (flet ((most-specific-concepts-1 (classes)
                                     (let ((l (remove-duplicates classes
                                                                 :test #'(lambda (x y)
                                                                           ;; this allows a cons
                                                                           (cond ((equal x y))
                                                                                 ((consp x) nil)
                                                                                 ((consp y) nil)
                                                                                 ((and (owl-oneof-p x) (owl-oneof-p y))
                                                                                  (owl-equivalent-p x y))
                                                                                 ((and (cl:typep x rdfs:Resource)
                                                                                       (cl:typep y rdfs:Resource)
                                                                                       (not (name x)) (not (name y)))
                                                                                  ; x and y might be an individual
                                                                                  (owl-equivalent-p x y)))))))
                                       (set-difference l l :test #'strict-abstp))))
      (let ((answer
             (cond ((member |rdfs:Resource| classes)
                    (substitute |rdfs:Resource| rdfs:Resource 
                                (most-specific-concepts-1 (substitute rdfs:Resource |rdfs:Resource| classes))))
                   (t (most-specific-concepts-1 classes)))))
        (assert answer)
        answer))))

(defun most-abstract-concepts (classes)
  "returns the most abstract concepts in RDF(S) semantics, or classes minus duplicates and subclasses
   of other classes in <classes>."
  (declare (optimize (speed 3) (safety 0)))
  (flet ((most-abstract-concepts-1 (classes)
                                   (let ((l (remove-duplicates classes
                                                               :test #'(lambda (x y)
                                                                         (cond ((equal x y))
                                                                               ((consp x) nil)
                                                                               ((consp y) nil)
                                                                               ((and (not (class-name x)) (not (class-name y)))
                                                                                (owl-equivalent-p x y)))))))
                                     (set-difference l l :test #'strict-specp))))
    (cond ((member |rdfs:Resource| classes)
           (substitute |rdfs:Resource| rdfs:Resource 
                       (most-abstract-concepts-1 (substitute rdfs:Resource |rdfs:Resource| classes))))
          (t (most-abstract-concepts-1 (substitute rdfs:Resource |rdfs:Resource| classes))))))
#|
(most-abstract-concepts (remove-if-not #'rdf-class-p (mapcar #'symbol-value (list-all-entities-in :sumo))))
(#<sumo:InheritableRelation sumo:IntentionalRelation> #<rdfs:Class sumo:Entity> #<sumo:Class sumo:InheritableRelation>)
|#
(defun strict-abstp (abst spec)
  "Is <abst> strictly (not equal to) superclass of <spec>?"
  (cond ((rdf-equalp abst spec) nil)
        ((subsumed-p spec abst) t)))

(defun strict-specp (spec abst)
  "Is <spec> strictly (not equal to) subclass of <abst>?"
  (and (not (rdf-equalp abst spec)) (subsumed-p spec abst)))

(defun most-specific-concepts-by-superclasses (classes)
  "same as <most-specific-concepts> but uses <clos-strict-supertype-p> instead of <strict-abstp>."
  (declare (optimize (speed 3) (safety 0)))
  (when (= 1 (length classes)) (error "Bingo!"))
  (flet ((most-specific-concepts-by-superclasses-1 (classes)
           (let ((l (remove-duplicates classes
                                       :test #'(lambda (x y)
                                                 (cond ((equal x y))
                                                       ((consp x) nil)
                                                       ((consp y) nil)
                                                       ((and (not (class-name x)) (not (class-name y)))
                                                        (rdf-graph-equalp x y)))))))
             (set-difference l l :test #'clos-strict-supertype-p))))
    (cond ((member |rdfs:Resource| classes :test #'eq)
           (substitute |rdfs:Resource| rdfs:Resource 
                       (most-specific-concepts-by-superclasses-1
                        (substitute rdfs:Resource |rdfs:Resource| classes  :test #'eq))
                       :test #'eq))
          (t (most-specific-concepts-by-superclasses-1
              (substitute rdfs:Resource |rdfs:Resource| classes :test #'eq))))))

(defun most-specific-concepts-by-clos-supers (classes)
  "same as <most-specific-concepts> but uses <clos-strict-supertype-p> instead of <strict-abstp>."
  (declare (optimize (speed 3) (safety 0)))
  (flet ((most-specific-concepts-by-clos-supers-1 (classes)
           (let ((l (remove-duplicates classes
                                       :test #'(lambda (x y)
                                                 (cond ((eql x y))
                                                       ((and (not (class-name x)) (not (class-name y)))
                                                        (rdf-graph-equalp x y)))))))
             (set-difference l l :test #'clos-strict-supertype-p))))
    (cond ((member |rdfs:Resource| classes :test #'eq)
           (substitute |rdfs:Resource| rdfs:Resource 
                       (most-specific-concepts-by-clos-supers-1
                        (substitute rdfs:Resource |rdfs:Resource| classes :test #'eq))
                       :test #'eq))
          (t (most-specific-concepts-by-clos-supers-1
              (substitute rdfs:Resource |rdfs:Resource| classes :test #'eq))))))

(defun most-specific-concepts-for-slotd-type (classes)
  "same as <most-specific-concepts> but understand forall, exists, and has in addition to subtypep.
   This is used for slot type reduction. If subtype relation is unknown, then the two remains in the list."
  (declare (optimize (speed 3) (safety 0)))
  (flet ((most-specific-concepts-for-slotd-type-1 (classes)
           (let ((l (remove-duplicates classes
                                       :test #'(lambda (x y)
                                                 (or (owl-equivalent-p x y)  ; allows one-of types
                                                     (%owl-same-p x y))))))
             (set-difference l l :test #'strict-supertype-p-for-slotd-type))))
    (cond ((member (symbol-value '|rdfs:Resource|) classes)
           (let ((rsc (symbol-value '|rdfs:Resource|))
                 (resource (symbol-value 'rdfs:Resource)))
             (substitute rsc resource 
                         (most-specific-concepts-for-slotd-type-1
                          (substitute resource rsc classes)))))
          (t (most-specific-concepts-for-slotd-type-1 classes)))))
#|
(defmethod mop:slot-definition-type :before ((slotd gx::Property-direct-slot-definition))
  "daemon for type in slotd, because it can be refined after the first creation."
  (declare (optimize (speed 3) (safety 0)))
  (let ((type (slot-value slotd 'excl::type)))
    (when (and (consp type)
               (case (car type)
                 (and (setq type (cdr type))) ; (and x ...) -> (x ...)
                 (or (error "Cant happen!")) 
                 (not nil)                 ; (not x)
                 (satisfies (error "Cant happen!"))
                 (forall (error "Cant happen!"))              ; (forall x c)
                 (exists (error "Cant happen!"))              ; (exists x c)
                 (fills (error "Cant happen!"))                 ; (fills x a)
                 (= nil)                   ; (= x n)
                 (<= nil)                  ; (<= x n)
                 (>= nil)                  ; (>= x n)
                 (otherwise t)))              ; (...)
      (let ((MSCs (most-specific-concepts-for-slotd-type type)))
        (assert MSCs)
        (cond ((<= (length MSCs) (length type))
               (setf (slot-value slotd 'excl::type) (mkatom MSCs)))
              ((and (cl:subtypep type MSCs) (cl:subtypep MSCs type)) type)
              (t (setf (slot-value slotd 'excl::type) (mkatom MSCs))))))))

(defmethod mop:slot-definition-type :before ((slotd gx::Property-effective-slot-definition))
  "daemon for type in slotd, because it can be refined after the first creation."
  (declare (optimize (speed 3) (safety 0)))
  (let ((type (slot-value slotd 'excl::type)))
    (when (and (consp type)
               (case (car type)
                 (and (setq type (cdr type))) ; (and x ...) -> (x ...)
                 ((or not) nil)
                 (satisfies (error "Cant happen!"))
                 ((forall exists fills) (error "Cant happen!"))
                 ((= <= >=) nil)
                 (otherwise t)))              ; (...)
      (let ((MSCs (most-specific-concepts-for-slotd-type type)))
        (assert MSCs)
        (cond ((<= (length MSCs) (length type))
               (setf (slot-value slotd 'excl::type) (mkatom MSCs)))
              ((and (cl:subtypep type MSCs) (cl:subtypep MSCs type)) type)
              (t (setf (slot-value slotd 'excl::type) (mkatom MSCs))))))))
|#
;;;
;;;; Miscellaneous Utilities for Typing
;;;

(defun rsc-object-p (x)
  "returns true if <x> is an RDF(S) metaclass, class, instance object, and xsd typed data, and not lisp data."
  ;(declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p x)
       (not (eq x (load-time-value (find-class 'gnode))))  ; not gnode
       (%resource-subtype-p (class-of x))))

(defun owl-thing-p (obj)
  "Hook"
  (declare (inline))
  (declare (ignore obj))
  nil)

(defun rdf-class-p (x)
  "returns true if <x> is an RDF(S) metaclass and class object."
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p x)
       (cond ((eq x (load-time-value (find-class 'rdfs:Class)))) ; rdfs:Class
             ((%rdf-class-subtype-p (class-of x))))))

(defun rdf-metaclass-p (x)
  "returns true if <x> is an RDF(S) metaclass resource object."
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p x)
       (cond ((eq x (load-time-value (find-class 'rdfs:Class)))) ; rdfs:Class
             ((and (%rdf-class-subtype-p (class-of x))
                   (%rdf-class-subtype-p x))
              t))))

(defun strict-class-p (x)
  "returns true if <x> is an RDF(S) class but not a metaclass."
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p x)
       (%rdf-class-subtype-p (class-of x))
       (not (%rdf-class-subtype-p x))))

(defun rdf-instance-p (x)
  "returns true if <x> is an instance of rdfs:Resource but not rdfs:Class.
   This returns true if <x> is a lisp string, a list number, a uri."
  (declare (optimize (speed 3) (safety 0)))
  (or (%instance-p x)
      (typecase x
        (cl:string t)
        (cl:number t)
        (uri t)
        (rdf:|inLang| t)
        (symbol 
         (when (and (boundp x) (not (eq x (symbol-value x))))
           (%instance-p (symbol-value x))))
        (cons nil))))
(defun %instance-p (x)
  "when <x> is a CLOS object, if <x> is an instance of rdfs:Resource but not rdfs:Class, this returns true, 
   otherwise nil." 
  (declare (optimize (speed 3) (safety 0)))
  (and (excl::standard-instance-p x)
       (%resource-subtype-p (class-of x))
       (not (%rdf-class-subtype-p (class-of x)))
       (not (eq x (load-time-value (find-class 'rdfs:Class))))
       (not (eq x (load-time-value (find-class 'gnode))))))

(defun datatype? (symbol)
  "Does this <symbol> denote an instance of rdfs:Datatype?"
  (and (boundp symbol)
       (cl:typep (symbol-value symbol) rdfs:Datatype)))

(defun datatype-p (obj)
  "Is <obj> an instance of rdfs:Datatype?"
  (and (rsc-object-p obj)
       (cl:typep obj rdfs:Datatype)))

(defun role-p (x)
  "returns true if <x> is an instance of rdf:Property."
  (declare (inline))
  (and (excl::standard-instance-p x)
       (%rdf-property-subtype-p (class-of x))))

(defun %rdf-property-subtype-p (class)
  "returns true if <x> is an instance of rdf:Property. <x> must be a CLOS object."
  (declare (optimize (speed 3) (safety 0)))
  (cond ((eq class (load-time-value (find-class 'rdf:|Property|))))
        ((not (mop:class-finalized-p class))
         (labels ((walk-partial-cpl (c)
                    (let ((supers (mop:class-direct-superclasses c)))
                      (when (member (load-time-value (find-class 'rdf:|Property|))
                                       supers
                                       :test #'eq)
                        (return-from %rdf-property-subtype-p t))
                      (mapc #'walk-partial-cpl supers))))
           (declare (dynamic-extent #'walk-partial-cpl))
           (walk-partial-cpl class)
           nil))
        ((member (load-time-value (find-class 'rdf:|Property|))
                    (mop:class-precedence-list class)
                    :test #'eq)
         t)))

(defun metaclass? (name)
  "returns true if <name> is a symbol that has a value of an RDF(S) metaclass resource 
   object. If <name> is not a symbol an error is signaled."
  (declare (inline))
  (and (boundp name) (rdf-metaclass-p (symbol-value name))))

(defun class? (name)
  "returns true if <name> is a symbol that has an RDF(S) metaclass, class, 
   and instance object. If <name> is not a symbol an error is signaled."
  (declare (inline))
  (and (boundp name) (rdf-class-p (symbol-value name))))

(defun object? (name)
  "returns true if <name> is a symbol that has an RDF(S) metaclass, class, 
   and instance object. If <name> is not a symbol an error is signaled."
  (declare (inline))
  (and (boundp name) (rsc-object-p (symbol-value name))))
(defun resource? (name)
  "same as <object?>."
    (declare (inline))
  (and (boundp name) (rsc-object-p (symbol-value name))))

(defun role? (name)
  "returns true if <name> is a symbol whose symbol value is an instance of 
   rdf:Property. If <name> is not a symbol an error is signaled."
  (declare (inline))
  (and (boundp name) (role-p (symbol-value name))))

(defun initarg= (initarg1 initarg2)
  (and (initarg-include-p initarg1 initarg2) (initarg-include-p initarg2 initarg1)))
(defun initarg-include-p (initarg1 initarg2)
  (loop for (role filler) on initarg1 by #'cddr with found
      do (setq found (getf initarg2 role :unbound))
      always (equal filler found)))

;;;
;;;; Subsumption on Property
;;;

(defun subproperty-p (subprop superprop &optional visited)
  "returns true if <subprop> is a sub-property of <superprop> or 
   <superprop> itself. Otherwise returns nil"
  (or (eql subprop superprop)
      (%owl-same-p subprop superprop)         ; See rdfp10
      (if (member subprop visited) nil
        (strict-subproperty-p subprop superprop visited))))
(defun strict-subproperty-p (subprop superprop visited)
  "returns true if <subprop> is not equal to and subproperty of <superprop>."
  (let ((superprops (and (slot-boundp subprop 'rdfs:subPropertyOf)
                         (slot-value subprop 'rdfs:subPropertyOf))))
    (unless (listp superprops) (setq superprops (list superprops)))
    (loop for subsSuper in superprops
        thereis (subproperty-p subsSuper superprop (cons subprop visited)))))

;;;
;;;; Type System
;;;
;;; Lisp types are mapped to RDF(S) types as shown in the table below.
;;; If you want to use the Lisp nature <typep> and <type-of>, use cl:typep and cl:type-of. 
;;; ----------------------------------------------------------------------------------
;;;  Lisp          RDF(S)
;;; --------------------------------------
;;;  null         rdfs:List
;;;  cons         rdfs:List
;;;  uri          xsd:|anyURI|
;;;  symbol       <type of symbol's value>
;;;  string       xsd:|string|
;;;  fixnum       xsd:|byte|, xsd:|short|, xsd:|int|
;;;  bignum       xsd:|int|, xsd:|long|, xsd:|integer|
;;;  inLang       rdf:|XMLLiteral|
;;;  <a shadowed-class> names of multiple classes
;;;  <a resource> cl:type-of value
;;;  rdfs:Class   rdfs:Class
;;;  <others>     cl:type-of value
;;; --------------------------------------
;;; ----------------------------------------------------------------------------------
;;; Example
;;; ----------------------------------------------------------------------------------
;;;  (type-of 32767)               => xsd:|short|
;;;  (type-of 2147483647)          => xsd:|int|
;;;  (type-of 9223372036854775807) => xsd:|long|
;;;  (type-of "string?")           => xsd:|string|
;;;  (type-of "Literal?"@en)       => rdf:|XMLLiteral|
;;;  (type-of ())                  => rdf:|List|
;;;  (type-of '(a b c))            => rdf:|List|
;;;  (type-of xsd:|true|)            => rdf:|boolean|
;;;  (type-of rdfs:label)          => rdf:|Property|
;;;  (type-of rdf:|Property|)        => rdfs:Class
;;;  (type-of rdfs:Class)          => rdfs:Class
;;; ----------------------------------------------------------------------------------

(defun type-of (x)
  "extended version of cl:type-of function for RDF(S) and OWL.
   This function returns type(s) of <x> as symbol. See above example."
  (case (cl:type-of x)
    (null 'rdf:|List|)
    (cons 'rdf:|List|)
    (uri 'xsd:|anyURI|)
    (iri 'xsd:|anyURI|)
    (fixnum (cond ((zerop x) 'xsd:|byte|)
                  ((plusp x)
                   (cond ((< x 128) 'xsd:|byte|)
                         ((< x 32768) 'xsd:|short|)
                         (t 'xsd:|int|)))
                  ((minusp x)
                   (cond ((>= x -128) 'xsd:|byte|)
                         ((>= x -32768) 'xsd:|short|)
                         (t 'xsd:|int|)))))
    (bignum (cond ((plusp x)
                   (cond ((< x 2147483648) 'xsd:|int|)
                         ((< x 9223372036854775808) 'xsd:|long|)
                         (t 'xsd:|integer|)))
                  ((minusp x)
                   (cond ((>= x -2147483648) 'xsd:|int|)
                         ((>= x -9223372036854775808) 'xsd:|long|)
                         (t 'xsd:|integer|)))))
    (single-float 'xsd:|float|)
    (double-float 'xsd:|double|)
    (rational     'xsd:|decimal|)
    (symbol (cond ((eq x t) 'xsd:|boolean|)
                  ((object? x) (type-of (symbol-value x)))
                  (t (error "Symbol ~S is not defined as QName." x))))
    (rdf:|inLang| 'rdf:|XMLLiteral|)
    (rdfs:Resource (cond ((shadowed-class-p (class-of x)) (mapcar #'name (mclasses x)))
                         (t (cl:type-of x))))
    (rdfsClass (cond ((eql x rdfs:Class) 'rdfs:Class)
                     (t (error "Another meta-metaclass than rdfs:Class:~S" x))))
    (otherwise (cond ((stringp x) 'xsd:|string|)
                     ((shadowed-class-p (class-of x)) (mapcar #'name (mclasses x)))
                     (t (cl:type-of x))))))

(export 'type-of)

;;;
;;;; Type Predicate
;;;
;;; Type predicate <typep> is an extension of cl:typep for RDF(S) and OWL.
;;; 
;;; The algorithm of the type predicate is described below. Here, <a> is an argument for 
;;; instance, <c> is an argument for class. I(<a>) means a resolved CLOS object for <a> against URI, 
;;; QName, and object itself, I(<c>) also means a resolved CLOS object for <c>.  Namely, I(<a>) and 
;;; I(<c>) are interpreted and tested as CLOS object. Note that T or true is expressed by two values 
;;; \<t t\>, F(alse) is expressed by two values \<nil t\>, and U(nknown) is expressed by two values 
;;; \<nil nil\>.
;;; # If <c> is t, returns T.
;;; # If <c> is nil, returns F.
;;; # If <a> is nil, returns T.
;;; # If <c> is an instance of rdfs:Resource, 
;;;   - If <a> is an instance of rdfs:Resource, then calls %type with <a> and <c>.
;;;   - If <a> can be resolved, then calls %type with I(<a>) and <c>.
;;;   - else calls %type with <a> and <c>.
;;; # If <c> is not an instance of rdfs:Resource,
;;;   - If <c> can be resolved, then recursively calls with <a> and I(<c>).
;;;   - If <c> is a list and logic form, then returns truth value according to the ternary table.
;;;   - else calls %type with <a> and <c>.
;;;
;;; Here is the algorithm of subfunction <%typep>.  
;;; # If <c> is an instance of rdfs:Class, 
;;;   - If <a> is an instance of rdfs:Resource, 
;;;     * If (cl:typep <a> <c>) is true, then returns T.
;;;     * If (cl:typep <c> <a>) is true, then returns F.
;;;     * Else returns the value of (<%%typep> <a> <c>). 
;;; # If <c> is an instance of rdfsClass, it implies <c> is rdfs:Class, then
;;;   - if <a> is rdfs:Class, then retursn T.
;;;   - if (cl:typep <a> <c>) is true, then returs T, else returns F. 
;;; # If <c> is an instance of rdfs:Resource, it implies a strict instance, so returns F.
;;; # Otherwise returns U.
;;;
;;; <%%typep> works for an rdf object and a class as follows. 
;;; Here <C> stands for equivalent classes of <c> including <c> itself.
;;; # If for some <c> of <C> (<%typep-without-type-equivalents> <a> <c>), then returns T. Otherwise returns F or U 
;;;   according to the accumulation of each equivalent class of <C>. 
;;;
;;; <%typep-without-type-equivalents> works for an rdf object and a class as follows.
;;; Note that <A> stands for same objects of <a> including <a> itself.
;;; # If for some <a> of <A> (<typep-without-sames-and-equivalents-in-owl> <a> <c>), then returns T. 
;;;   Otherwise returns F or U according to the accumulation of each same object of <A>.
;;;
;;; <typep-without-sames-and-equivalents-in-owl> works as follows.
;;; # If <c> is an intersection of concepts, returns the result of <owl-intersection-type-p> for the intersections.
;;; # If <c> is an union of concepts, returns the result of <owl-union-type-p> for the unions.
;;; # If <c> is an complement concepts, returns the result of <owl-complement-type-p> for the complement.
;;; # If <*autoepistemic-local-closed-world*> is true, returns F else returns U.
;;; 
;;;
;;; Examples of xsd data types
;;; ----------------------------------------------------------------------------------
;;; (typep 1 xsd:|positiveInteger|)
;;; (typep -1 xsd:|negativeInteger|)
;;; (typep 0 xsd:|nonNegativeInteger|)
;;; (typep 0 xsd:|nonPositiveInteger|)
;;; (typep 32767 xsd:|short|)
;;; (typep 32768 xsd:|int|)
;;; (typep 2147483647 xsd:|int|)
;;; (typep 2147483648 xsd:|long|)
;;; (typep 9223372036854775807 xsd:|long|)
;;; (typep 9223372036854775808 xsd:|integer|)
;;; (typep 1 xsd:|decimal|)
;;; (typep (cl:rational 1.0) xsd:|decimal|)
;;; (typep 1.0e0 xsd:|float|)
;;; (typep 1.0d0 xsd:|double|)
;;; (typep "string?" xsd:|string|)
;;; (typep "string?"@en xsd:|string|)
;;; (typep (uri "http://somewhere") xsd:|anyURI|)
;;; (typep xsd:|false| xsd:|boolean|)
;;; (typep 1 xsd:|anySimpleType|)
;;; (typep 1 rdf:|XMLLiteral|)
;;; (typep "1"^^xsd:|positiveInteger| xsd:|positiveInteger|)
;;; (typep "1"^^xsd:|positiveInteger| xsd:|anySimpleType|)
;;; (typep "1"^^xsd:|positiveInteger| rdf:|XMLLiteral|)
;;; ----------------------------------------------------------------------------------
;;;
;;; Examples of Literals
;;; ----------------------------------------------------------------------------------
;;; (typep 1 rdfs:Literal)
;;; (typep 1 rdfs:Resource)
;;; (typep "1"^^xsd:|positiveInteger| rdfs:Literal)
;;; (typep "1"^^xsd:|positiveInteger| rdfs:Resource)
;;; (typep "subway"@en rdf:|XMLLiteral|)
;;; (typep "subway"@en rdfs:Literal)
;;; (typep rdfs:label rdf:|Property|)
;;; ----------------------------------------------------------------------------------
;;;
;;; Examples of Others
;;; ----------------------------------------------------------------------------------
;;; (typep (list 1 2 3) rdf:|List|)
;;; (typep 1 (list 'and xsd:|integer| rdf:|XMLLiteral|))
;;; (typep 1 (list 'or xsd:|integer| xsd:|float|))
;;; (typep rdf:|Property| rdfs:Class)
;;; (typep rdfs:Class rdfs:Class)
;;; (typep rdfs:label rdfs:Resource)
;;; (typep rdf:|Property| rdfs:Resource)
;;; (typep rdfs:Class rdfs:Resource)
;;; ----------------------------------------------------------------------------------

(defun typep (object type)
  "extended typep function for Semantic Web. This function resolves the difference among 
   URI, QName, and object for parameters. Namely, if a parameter is URI or symbol, then 
   the related CLOS object is taken to test it."
  (declare (optimize (speed 3) (safety 0)))
  (when (eq type t) (return-from typep (values t t)))
  (when (null type) (return-from typep (values nil t)))
  (when (null object) (return-from typep (values t t)))
  (when (eql type rdfs:Resource) (return-from typep (values t t)))
  (typecase type
    (rdfs:Resource    ; type is an object in RDF universe including rdfs:Resource itself.
     (typecase object
       (rdfs:Resource (%typep object type))
       ;; resolve for object
       (uri (cond ((string= (name type) "Ontology") (values t t))
                          ((cl:subtypep (symbol-value 'xsd:|anyURI|) type) (values t t))
                          ((and (iri-p object) (iri-boundp object))
                           (%typep (iri-value object) type))
                          (t (values nil t))))
       (symbol (cond ((object? object) (%typep (symbol-value object) type))
                     ((cl:typep object type) (values t t)) ; not in RDF universe
                     (t (values nil t))))
       (otherwise (%typep object type))))
    ;; resolve for type
    (symbol (cond ((object? type) (typep object (symbol-value type)))
                  ((cl:typep object type) (values t t))
                  (t (values nil t))))
    (iri (cond ((iri-boundp type) (typep object (iri-value type)))
               ((cl:typep object type) (values t t))
               (t (values nil t))))
    (cons (case (op type)
            ((not and or) (setq type (->nnf type)))
            ((forall exists fills)
             ;; vin:SemillonGrape vs. (forall vin:madeFromGrape #<owl:Class {vin:SemillonGrape vin:SauvignonBlancGrape}>)
             (error "Not Yet!"))
            (otherwise (setq type (->nnf (cons 'and type)))))
          (if (atom type) (typep object type)   ; (and/or x) turns x through nnf
            (ecase (op type)
              (and (loop for ty in (args type) with known = t
                       do (multiple-value-bind (v1 v2) (typep object ty)
                            (when (not v1)
                              (cond (v2 ; definite false
                                     (return-from typep (values nil t)))
                                    (t ; unknown then unknown accumulated
                                     (setq known (and known v2))))))
                         ;; otherwise continue loop
                       finally (return (cond (known (values t t))
                                             (t (values nil nil))))))
              (or (loop for ty in (args type) with known = t
                      do (multiple-value-bind (v1 v2) (typep object ty)
                           (when v1 ; no <t, nil>, always <t, t>
                             (return-from typep (values t t)))
                           (setq known (and known v2))) ; v2 may be either t or nil
                      finally (return (values nil known))))
              (not (multiple-value-bind (v1 v2) (typep object (arg1 type))
                     (cond (v2 (values (not v1) t))
                           (t (warn "*** Negation of Unknow (typep ~S ~S) is unknown." object (arg1 type))
                              (values nil nil))))))))
    (otherwise  (%typep object type))
    ))

(export 'typep)

(defun %typep (object type)
  "<object> and <type> is an object in RDF universe."
  (declare (optimize (speed 3) (safety 0)))
  (when (eq type |rdfs:Resource|) (setq type rdfs:Resource))
  (when (and (cl:typep object 'cl:string) (cl:subtypep (symbol-value 'rdfs:Literal) type))
    (return-from %typep (values t t)))
  (when (and (cl:typep object 'cl:number) (cl:subtypep (symbol-value 'rdfs:Literal) type))
    (return-from %typep (values t t)))
  (typecase type
    (rdfs:Class    ; type is a class then object is an instance ?
     (typecase object
       (rdfs:Resource ; type is not rdfs:Class
        (when (cl:typep object type) (return-from %typep (values t t)))
        (when (cl:typep type object) (return-from %typep (values nil t)))
        ;; falling into here, and check it in OWL semantics
        (%%typep object type)
        )
       (rdf:|inLang| (if (cl:subtypep (symbol-value 'xsd:|string|) type) (values t t) (values nil t)))
       (cons (if (cl:subtypep rdf:|List| type) (values t t) (values nil t)))
       (cl:string (cond ((and (cl:typep type rdfs:Datatype) (cl:typep object (name type)))
                         (values t t))
                        ((cl:subtypep (symbol-value 'xsd:|string|) type)
                         (values t t))
                        (t (values nil t))))
       (cl:number (cond ((and (cl:typep type rdfs:Datatype) (cl:typep object (name type)))
                         (values t t))
                        ((cl:subtypep (symbol-value 'xsd:|decimal|) type)
                         (values t t))
                        (t (values nil t))))
       (otherwise (if (cl:typep object type) (values t t)
                    (if (cl:typep type object) (values nil t)
                      (values nil nil))))))
    (rdf:|inLang|         ; type is an instance of rdfs:Literal 
     (values nil t))
    (rdfs:Resource ; falling here means that type is a strict instance
     (values nil t))
    (otherwise (values nil nil))))

(defun %%typep (object type)
  "Hook for OWL. See OWL module."
  (declare (ignore object type))
  (values nil t))

(defun %typep-for-MSCs (object type)
  "<type> is a CLOS object including rdfs:Literal including datatypes (instances of rdf:Datatype).
   Note that this subfunction is invoked with <type> that is a CLOS class."
  (declare (optimize (speed 3) (safety 0)))
  (assert (excl::standard-instance-p object))
  (assert (excl::standard-instance-p type))
  (when (eq type |rdfs:Resource|) (setq type rdfs:Resource))
  (typecase object
    (rdfs:Class (%typep object type))
    (rdfs:Resource (%typep object type))
    (otherwise (cond ((%typep object type))
                     (t nil)))))

(defun owl-oneof-p (x)
  "Hook for OWL."
  (declare (ignore x))
  nil)

(defun same-as-of (x)
  "Hook for OWL."
  (list x))

(defun %owl-disjoint-p (c1 c2)
  "this is defined in owlequivalentdisjoint module."
  nil)

(defun disjoint-pairs-p (classes)
  (some #'(lambda (x)
            (some #'(lambda (y) (and (not (eq x y)) (%owl-disjoint-p x y)))
                  classes))
        classes))

;;;
;;;; Negation Normal Form (NNF)
;;;
;;; NNF is a logical form in which negation is applied to only logical atom.
;;; In the following routine, non-NNF should be a prefix form in S-expression.
;;; ----------------------------------------------------------------------------------
;;;  <form> ::= <atom> | (not <form>) | (and <form>*) | (or <form>*) | 
;;;             (forall <var> <form>*) | (exists <var> <form>*) | (fills <var> <form>)
;;;  <NNF>  ::= <atom> | (not <atom>) | (and <NNF>*)  | (or <NNF>*)
;;;             (forall <var> <NNF>) | (exists <var> <NNF>) | (fills <var> <NNF>)
;;; Ex.
;;; (->nnf '(not (and (not (or (not A) (and C (not D)))))))
;;;  -> (or (not A) (and C (not D)))
;;; ----------------------------------------------------------------------------------

(defun ->nnf (P)
  "transforms non-NNF S-expression <P> to NNF and returns it."
  (move-not-inwards (move-not-inwards P)))

;;;
;;; This program is borrowed from AIMA
;;;
;;; Note that <move-not-inwards> returns ~P for P.

(defun move-not-inwards (P)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (case (op P)
    ((t) 'nil)             ; seiji
    ((nil) 't)             ; seiji
    (not (arg1 P))
    (and (disjunction (mapcar #'move-not-inwards (args P))))
    (or  (conjunction (mapcar #'move-not-inwards (args P))))
    (forall (make-exp 'exists (arg1 P) (move-not-inwards (arg2 P))))
    (exists (make-exp 'forall (arg1 P) (move-not-inwards (arg2 P))))
    (fills  (error "Not Yet!") (make-exp 'fills  (arg1 P) (move-not-inwards (arg2 P))))
    (t (make-exp 'not P))))

(defun conjunction (args)
  "Form a conjunction with these args."
  (case (length args)
    (0 't)                   ; seiji
    (1 (first args))
    (t (cons 'and args))))

(defun disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 'nil)              ; seiji
    (1 (first args))
    (t (cons 'or args))))

(defun make-exp (op &rest args) "makes a form of <op> and <args>." (cons op args))
(defun op (exp) "Operator of an expression" (if (listp exp) (first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (rest exp) nil))
(defun arg1 (exp) "First argument" (first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))

;; End of module
;; --------------------------------------------------------------------

(cl:provide :gxtype)
