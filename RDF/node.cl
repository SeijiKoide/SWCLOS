;;;-*- Mode: common-lisp; syntax: common-lisp -*-

(cl:provide :rdfnode)

(defpackage :gx
  (:export name mclasses))
(in-package :gx)

;;;; gnode & rdf-node
;;; rdf-node will be a superclass of rdfs:Class. 
;;; gnode will be a superclass of rdfs:Resource. 
;;; gnode class is needed for registration of class-instance relation. 

(defclass rdf-node (standard-class)
  ((direct-instances :initarg :direct-instances
                     :initform nil 
                     :accessor class-direct-instances))
  (:documentation "This metaclass is node class. This metaclass provides method class-direct-instances"))

(defclass gnode ()
  ((excl::name :initarg :name :initform nil)
   (iri :initarg :iri :initform nil :accessor iri)
   ;(mclasses :initarg :mclasses :initform nil :accessor mclasses)
   (type-tag :initform nil :accessor type-tag)
   ;(inv-plist :initform nil)
   )
  (:metaclass rdf-node)
  (:documentation "This class provides the concept of RDF graph."))

(defmethod mclasses ((instance gnode))
  "returns multiple classes of <gnode>. This function returns length=1 list for single class."
  (labels ((get-bright-supers (super)
                              (cond ((not (shadowed-class-p super)) (list super))
                                    (t (mapcan #'get-bright-supers (mop:class-direct-superclasses super))))))
    (let ((class (class-of instance)))
      (cond ((shadowed-class-p class)
             (remove-duplicates (mapcan #'get-bright-supers (mop:class-direct-superclasses class))))
            (t (list class))))))

;;; An element of direct-instances slot are initially stored by <make-instance(rdf-node)> method 
;;; and maintained by <update-instance-for-different-class:after(gnode)> which is invoked by 
;;; change-class.

(defmethod make-instance ((class rdf-node) &rest initargs)
  (declare (ignore initargs))
  (let ((instance (call-next-method)))
    (push instance (class-direct-instances class))
    instance))

(defun shadowed-class-p (x)
  "returns true if <x> is an instance of shadowed class.
   shadowed-class is defined at RdfsObjects file."
  (eq (class-name (class-of x)) 'shadowed-class))

(defmethod update-instance-for-different-class :after ((previous gnode) current &rest initargs)
  (declare (ignore initargs))
  (cond ((cl:typep current 'destroyed-class)
         (let ((old-class (class-of previous)))
           (setf (class-direct-instances old-class)
             (remove current (class-direct-instances old-class) :test #'eq))
           ))
        (t (let ((old-class (class-of previous))
                 (new-class (class-of current)))
             ;; domain constraint should be satisfied, if old-class was satisfied.
             ;; class direct instances handling
             (setf (class-direct-instances old-class)
               (remove current (class-direct-instances old-class) :test #'eq))
             (push current (class-direct-instances new-class))
             ))))

(defun node-p (x)
  (cl:typep x 'gnode))

(defun bnode-p (node)
  (or (not (slot-value node 'excl::name))
      (not (symbol-package (slot-value node 'excl::name)))))

(defmethod ground? ((node gnode))
  (and (slot-value node 'excl::name)
       (symbol-package (slot-value node 'excl::name))))

(defmethod name ((node symbol))
  node)

(defmethod name ((node gnode))
  "returns a QName or a nodeID of <node>, if it exists. Otherwise nil."
  (let ((name (slot-value node 'excl::name)))
    (when (and name (symbol-package name)) name))) ; name might have uninterned symbol.

(defmethod (setf name) (symbol (node gnode))
  "exports <symbol> for QName."
  (setf (slot-value node 'excl::name) symbol)
  (export-as-QName symbol)
  (setf (symbol-value symbol) node))

(defmethod shared-initialize :after ((instance gnode) slot-names &rest initargs)
  (cond ((and (null slot-names) (null initargs))  ; when change-class
         )
        ((and (consp slot-names) (null initargs)) ; when propagated
         )
        (t                                        ; first or redefinition
         (let ((name (getf initargs :name)))
           (when name
             (when (nodeID? name)
               (setf (slot-value instance 'excl::name) nil))
             (export-as-QName name)
             (setf (symbol-value name) instance))))))