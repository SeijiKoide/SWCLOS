;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Unification with Type
;;;
;;; This program is encoded by Seiji Koide based non-typed unifier of 
;;; "Paradigms of AI Programming: Case Studies in Common Lisp", 
;;; by Peter Norvig, published by Morgan Kaufmann, 1992.
;;;
;;; This program is integrated to SWCLOS, an OWL processor on CLOS.
;;;  2008,2009 (c) Seiji Koide
;;
;; History
;; -------
;; 2008.02.04    File created.

(in-package :gx)

(export '(+no-bindings+ subst-bindings unify))

(defconstant +fail+ nil
  "Indicates unification failure")

(defconstant +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (cdr (assoc var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . val) pair to the head of a binding list."
  (acons var val
         ;; Once we add a binding,
         ;; we can get rid of the dummy +no-bindings+
         (if (eq bindings +no-bindings+)
             nil
           bindings)))

(defun occurs-in? (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-in? var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in? var (cl:first x) bindings)
                       (occurs-in? var (cl:rest x) bindings)))
        (t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings +fail+) +fail+)
        ((eq bindings +no-bindings+) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))

;;; <binding>    := '(' <key> '.' <value> ')'
;;; <key>        := <variable> | <constant> | <skolem function> | <individual>
;;; <value>      := <variable> | <constant> | <skolem function> | <individual>
;;; <cbinding>   := '(' <ckey> '.' <cvalue> ')'
;;; <key>        := <variable> | <constant> | <skolem function> | <class>
;;; <value>      := <variable> | <constant> | <skolem function> | <class>
;;; <kbinding>   := '(' <kkey> '.' <kvalue> ')'
;;; <key>        := <variable> | <constant> | <skolem function> | <metaclass>
;;; <value>      := <variable> | <constant> | <skolem function> | <metaclass>
;;; * <variable> is a lisp symbol with starting '?' character.
;;; * <constant> is a lisp symbol without starting '?' character.
;;; * <skolem function> is a conce cell.
;;; * <individual> is a resource object as individual.
;;; * <class> is a resource object as class.
;;; * <metaclass> is a resource object as metaclass.
;;; This bindings simulate the environment for lisp symbol bindings.
;;; The virtual value of this bindings supersedes the actual symbol value.

(defun symbol-value+ (key bindings)
  "simulates 'symbol-value' in lisp with <bindings>."
  (let* ((binding (assoc key bindings))
         (val (cdr binding)))
    (typecase val
      (null (and (boundp key) (symbol-value key)))
      (cons val)
      (otherwise val))))

(defun name+ (object bindings)
  (or (car (rassoc object bindings))
      (name object)))

(defun class-of+ (x bindings cbindings)
  "simulates class-of but <cbindings> is prior to actual class."
  (let* ((binding (assoc x bindings))
         (val (cdr binding))
         (class+ (lookup x cbindings)))
    (cond (class+) ; even if val has an actual class, this virtual class exceeds.
                   ; this is useful to unify classes and make a subclass virtually.
          ((null binding)
           (cond ((and (symbolp x) (resource? x)) (class-of (symbol-value x)))
                 ((%instance-p x) (class-of x))
                 ((rdf-class-p x) (class-of x))
                 (t (symbol-value 'owl:Thing))))
          ((%instance-p val) (class-of val))
          ((rdf-class-p val) (class-of val))
          ((and (symbolp val) (resource? val)) (class-of (symbol-value val)))
          ((skolem-p val) (error "Not Yet!"))
          ((consp val) (conjunction val))  ; if a list then it should be a conjunction.
          (t (symbol-value 'owl:Thing)))))

;;;
;;; A variable is a symbol whose name starts with character #\?.
;;; See also NNF for <variable?> and <new-variable>.
;;;

(defun make-binding (var val) (cons var val))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable? exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (pushnew tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
        predicate
        (cl:first tree)
        (unique-find-anywhere-if predicate (cl:rest tree)
                                 found-so-far))))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defun rename-variables (x)
  "Replace all variables in x with new ones."
  (sublis (mapcar #'(lambda (var) (make-binding var (new-variable var)))
                  (variables-in x))
          x))

;;;
;;; Followings are simulate <owl-same-p>, <different-p>, <owl-equivalent-p>, 
;;; <subtypep>, <disjoint-p> on <bindings> and <cbindings>. These functions 
;;; are used in the typep unification program.
;;;

(defun owl-same-p+ (x y bindings)
  "can <x> and <y> be regarded as same with <bindings>?"
  (cond ((equal x y) t)
        ((and (rsc-object-p x) (rsc-object-p y))
         (definitely-owl-same-p x y))
        ((and x (symbolp x) (symbol-value+ x bindings))
         (owl-same-p+ (symbol-value+ x bindings) y bindings))
        ((and y (symbolp y) (symbol-value+ y bindings))
         (owl-same-p+ x (symbol-value+ y bindings) bindings))
        ))

(defun different-p+ (x y bindings)
  "can <x> and <y> be regarded as different with <bindings>?"
  (cond ((equal x y) nil)
        ((and (owl-thing-p x) (owl-thing-p y))
         (definitely-owl-different-p x y))
        ((and x (symbolp x) (symbol-value+ x bindings))
         (different-p+ (symbol-value+ x bindings) y bindings))
        ((and y (symbolp y) (symbol-value+ y bindings))
         (different-p+ x (symbol-value+ y bindings) bindings))
        ((variable? x) nil)
        ((variable? y) nil)
        ((and (symbolp x) (symbolp y)) t)))

(defun owl-equivalent-p+ (c d cbindings)
  "can <c> and <d> be regarded as equivalent class with <cbindings>?"
  (cond ((equal c d) t)
        ((and (rdf-class-p c) (rdf-class-p d))
         (definitely-owl-equivalent-p c d))
        ((and c (symbolp c) (symbol-value+ c cbindings)
              (owl-equivalent-p+ (symbol-value+ c cbindings) d cbindings)))
        ((and d (symbolp d) (symbol-value+ d cbindings)
              (owl-equivalent-p+ c (symbol-value+ d cbindings) cbindings)))))

(defun subtypep+ (c d cbindings)
  "is <c> a subtype of <d> with <cbindings>?
   Note that <cl:subtypep> is used."
  (cond ((and (null c) (null d)) nil)
        ((and c (symbolp c)) (subtypep+ (symbol-value+ c cbindings) d cbindings))
        ((and d (symbolp d)) (subtypep+ c (symbol-value+ d cbindings) cbindings))
        ((and c d (cl:subtypep c d)))))

(defun disjoint-class-p+ (c d cbindings)
  (cond ((equal c d) nil)
        ((and (rdf-class-p c) (rdf-class-p d))
         (disjoint-p c d))
        ((and c (symbolp c) (symbol-value+ c cbindings))
         (disjoint-class-p+ (symbol-value+ c cbindings) d cbindings))
        ((and d (symbolp d) (symbol-value+ d cbindings))
         (disjoint-class-p+ c (symbol-value+ d cbindings) cbindings))
        ((skolem-p c) (error "Not Yet!"))
        ((consp c)
         (case (op c)
           (and (error "Not Yet!"))
           (or (error "Not Yet!"))
           (not (error "Not Yet!"))
           (otherwise (disjoint-class-p+ `(and ,@c) d cbindings))))
        ((skolem-p d) (error "Not Yet!"))
        ((consp d)
         (case (op d)
           (and (error "Not Yet!"))
           (or (error "Not Yet!"))
           (not (error "Not Yet!"))
           (otherwise (disjoint-class-p+ c `(and ,@d) cbindings))))
        ))

;;;
;;; The bindings in lisp provide the base level bindings in unification.
;;; The bindings in unification provides situated bindings that change in situation. 
;;; The unify implementation in "Paradigms of AI Programming" is just same as the symbol 
;;; bindings in old lisp system by association list. So, the mechanism of old lisp 
;;; binding is utialized for situated binding and classing in unification.
;;;

;;;
;;;; Typed Unification
;;;

(defun unify (x y &optional (bindings +no-bindings+) (cbindings +no-bindings+) (kbindings +no-bindings+))
  ;; this function deals with only instances.
  (format t "~%Unify:~S ~S" x y)
  (cond ((eq bindings +fail+)
         (values +fail+ cbindings kbindings))
        ((different-p+ x y bindings)
         (values +fail+ cbindings kbindings))
        ((owl-same-p+ x y bindings)
         (values bindings cbindings kbindings))
        ((and (skolem-p x) (skolem-p y))
         (unify-skolem x y bindings cbindings))
        ((and (skolem-p x) (not (variable? y)))
         (multiple-value-bind (newbindings newcbindings newkbindings)
             (unify-xskolem x y bindings cbindings)
           (when (get-binding y newcbindings)
             (rplaca (get-binding y newcbindings) x))
           (values newbindings newcbindings newkbindings)))
        ((and (not (variable? x)) (skolem-p y))
         (unify-yskolem x y bindings cbindings))
        ((and (variable? x) (variable? y)
              (lookup x bindings) (lookup y bindings))
         (let ((xclass (class-of+ x cbindings kbindings))
               (yclass (class-of+ y cbindings kbindings)))
           (multiple-value-bind (newbindings newcbindings newkbindings)
               (unify (lookup x bindings) (lookup y bindings) bindings cbindings kbindings)
             (when (eq newcbindings +fail+)
               (return-from unify (values +fail+ newcbindings newkbindings)))
             (when (get-binding xclass newcbindings)
               (rplaca (get-binding xclass newcbindings) x))
             (when (get-binding yclass newcbindings)
               (rplaca (get-binding yclass newcbindings) y))
             (values newbindings newcbindings newkbindings)
             )))
        ((variable? x)
         (unify-var x y bindings cbindings kbindings))
        ((variable? y)
         (unify-var y x bindings cbindings kbindings))
        ((and (consp x) (consp y))
         (multiple-value-setq (bindings cbindings kbindings)
           (unify (first x) (first y) bindings cbindings kbindings))
         (unify (rest x) (rest y) bindings cbindings kbindings))
        (t ;; unify two objects x and y, when it is not stated different from each other.
         (format t "~&Unified ~S and ~S." x y)
         (unify-object x y bindings cbindings)
         )))

(defun tunify (c d &optional (cbindings +no-bindings+) (kbindings +no-bindings+))
  ;; this function deals with only classes.
  (format t "~%TUnify:~S ~S" c d)
  (cond ((eq cbindings +fail+) (values +fail+ kbindings))
        ((different-p+ c d cbindings) (values +fail+ kbindings))      ; then clash
        ((disjoint-class-p+ c d cbindings) (values +fail+ kbindings)) ; then clash
        ((owl-same-p+ c d cbindings) (values cbindings kbindings))
        ((owl-equivalent-p+ c d cbindings) (values cbindings kbindings))
        ((and (variable? c) (variable? d))
         (let ((cval (symbol-value+ c cbindings))
               (dval (symbol-value+ d cbindings)))
           (multiple-value-bind (newcbindings newkbindings)
               (tunify cval dval cbindings kbindings)
             (when (eq newcbindings +fail+) (return-from tunify (values +fail+ kbindings)))
             (when (get-binding cval newcbindings)
               (rplaca (get-binding cval newcbindings) c))
             (when (get-binding dval newcbindings)
               (rplaca (get-binding dval newcbindings) d))
             (values newcbindings newkbindings))))
        ((and (owl-class-p c) (owl-class-p d))
         (setq kbindings
               (ttunify (class-of+ c cbindings kbindings)
                        (class-of+ d cbindings kbindings)
                        kbindings))
         (cond ((eq kbindings +fail+) (values +fail+ kbindings))
               (t (values (unify-class c d cbindings) kbindings))))
        ((variable? c) (tunify-var c d cbindings kbindings))
        ((variable? d) (tunify-var d c cbindings kbindings))
        ((and (consp c) (consp d))
         (multiple-value-setq (cbindings kbindings)
           (tunify (first c) (first d) cbindings kbindings))
         (tunify (rest c) (rest d) cbindings kbindings))
        ((and (symbolp c) (resource? c))
         (tunify (symbol-value c) d cbindings kbindings))
        ((and (symbolp c) (resource? d))
         (tunify c (symbol-value d) cbindings kbindings))
        (t ;; unify c and d as class
         (format t "~&Unified class ~S and ~S." c d)
         (unify-class c d cbindings))))

(defun ttunify (c d &optional (kbindings +no-bindings+))
  ;; this function deals with only metaclasses.
  (format t "~%TTUnify:~S ~S" c d)
  (cond ((eq kbindings +fail+) +fail+)
        ((different-p+ c d kbindings) +fail+)      ; then clash
        ((disjoint-class-p+ c d kbindings) +fail+) ; then clash
        ((owl-same-p+ c d kbindings) kbindings)
        ((owl-equivalent-p+ c d kbindings) kbindings)
        ((and (variable? c) (variable? d))
         (let ((cval (symbol-value+ c kbindings))
               (dval (symbol-value+ d kbindings)))
           (let ((newkbindings (tunify cval dval kbindings)))
             (when (eq newkbindings +fail+) (return-from ttunify +fail+))
             (when (get-binding cval newkbindings)
               (rplaca (get-binding cval newkbindings) c))
             (when (get-binding dval newkbindings)
               (rplaca (get-binding dval newkbindings) d))
             newkbindings)))
        ((and (owl-class-p c) (owl-class-p d))
         (setq kbindings
               (ttunify (class-of+ c kbindings kbindings)
                        (class-of+ d kbindings kbindings)
                        kbindings))
         (cond ((eq kbindings +fail+) +fail+)
               (t (unify-class c d kbindings))))
        ((variable? c) (ttunify-var c d kbindings))
        ((variable? d) (ttunify-var d c kbindings))
        ((and (consp c) (consp d))
         (ttunify (rest c) (rest d) (ttunify (first c) (first d) kbindings)))
        ((and (symbolp c) (resource? c))
         (ttunify (symbol-value c) d kbindings))
        ((and (symbolp c) (resource? d))
         (ttunify c (symbol-value d) kbindings))
        (t ;; unify c and d as class
         (format t "~&Unified metaclass ~S and ~S." c d)
         (unify-class c d kbindings))))

(defun unify-var (var x bindings cbindings kbindings)
  (let ((var-class (class-of+ var bindings cbindings))
        (val ())
        (val-class ()))
    (format t "~%var:class = ~S:~S" var (name var-class))
    (cond ((get-binding var bindings)
           (setq val (lookup var bindings))
           (cond ((skolem-p val)
                  (multiple-value-bind (pred suc) (unskolemize val)
                    (declare (ignore pred))
                    (setq val suc)
                    (setq val-class (class-of+ suc bindings cbindings))))
                 (t (setq val-class (class-of+ val bindings cbindings))))
           (format t "~%val1:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from unify-var (values +fail+ cbindings kbindings)))
                 ((owl-equivalent-p var-class val-class))
                 ((subtypep+ var-class val-class bindings)
                  (setq cbindings (extend-bindings val `,var-class cbindings)))
                 ((subtypep+ val-class var-class bindings)
                  (setq cbindings (extend-bindings var `,val-class cbindings)))
                 (t (setq cbindings 
                          (extend-bindings x `((and ,var-class ,val-class))
                                           (extend-bindings val `((and ,var-class ,val-class))
                                                            cbindings)))))
           (unify val x bindings cbindings kbindings))
          ((and (variable? x) (get-binding x bindings))
           (setq val (lookup x bindings))
           (cond ((skolem-p val)
                  (multiple-value-bind (pred suc) (unskolemize val)
                    (declare (ignore pred))
                    (setq val suc)
                    (setq val-class (class-of+ suc bindings cbindings))))
                 (t (setq val-class (class-of+ val bindings cbindings))))
           (format t "~%val2:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from unify-var (values +fail+ cbindings kbindings)))
                 ((owl-equivalent-p var-class val-class))
                 ((subsumed-p var-class val-class)
                  (setq cbindings (extend-bindings val var-class cbindings)))  ; <---
                 ((subsumed-p val-class var-class)
                  (setq cbindings (extend-bindings var val-class cbindings)))
                 (t (setq cbindings
                          (extend-bindings val `((and ,var-class ,val-class))  ; <---
                                           (extend-bindings var `((and ,var-class ,val-class))
                                                            cbindings)))))
           (unify var val bindings cbindings kbindings))
          ((occurs-in? var x bindings) +fail+)
          (t (cond ((skolem-p x)
                    (multiple-value-bind (pred suc) (unskolemize x)
                      (declare (ignore pred))
                      (setq x suc)
                      (setq val-class (class-of+ suc bindings cbindings))))
                   (t (setq val-class (class-of+ x bindings cbindings))))
             (format t "~%x:class = ~S:~S" x (name val-class))
             (cond ((disjoint-p var-class val-class)
                    (return-from unify-var (values +fail+ cbindings kbindings)))
                   ((owl-equivalent-p var-class val-class))
                   ((subtypep+ var-class val-class cbindings)
                    (cond ((eql (cdr (assoc val cbindings)) var-class))
                          ((disjoint-p (cdr (assoc val cbindings)) var-class)
                           (return-from unify-var (values +fail+ +fail+ kbindings)))
                          (t (setq cbindings
                                   (extend-bindings val var-class cbindings)))))
                   ((subtypep+ val-class var-class cbindings)
                    (cond ((eql (lookup var cbindings) val-class)) ; nothing done
                          ((disjoint-p (cdr (assoc var cbindings)) val-class)
                           (return-from unify-var (values +fail+ +fail+ kbindings)))
                          (t (setq cbindings
                                   (extend-bindings var val-class cbindings)))))
                   (t (setq cbindings
                            (extend-bindings x `((and ,var-class ,val-class))
                                             (extend-bindings var `((and ,var-class ,val-class))
                                                              cbindings)))))
             (values (extend-bindings var x bindings) cbindings kbindings)))))

(defun tunify-var (var x cbindings kbindings)
  (let* ((var-class (class-of+ var cbindings kbindings))
         (val ())
         (val-class ()))
    (format t "~%tvar:class = ~S:~S" var var-class)
    (cond ((setq val (symbol-value+ var cbindings))
           (setq val-class (class-of+ val cbindings kbindings))
           (format t "~%tval:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from tunify-var (values +fail+ kbindings)))
                 ((owl-equivalent-p var-class val-class))
                 ((subtypep+ var-class val-class kbindings)
                  (setq kbindings (extend-bindings val `,var-class kbindings)))
                 ((subtypep+ val-class var-class kbindings)
                  (setq kbindings (extend-bindings var `,val-class kbindings)))
                 (t (setq kbindings 
                          (extend-bindings x `((and ,var-class ,val-class))
                                           (extend-bindings val `((and ,var-class ,val-class))
                                                            kbindings)))))
           (tunify val x cbindings kbindings))
          ((and (variable? x) (setq val (symbol-value+ x cbindings)))
           (setq val-class (class-of+ val cbindings kbindings))
           (format t "~%tval:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from tunify-var (values +fail+ kbindings)))
                 ((owl-equivalent-p var-class val-class))
                 ((subtypep+ var-class val-class kbindings)
                  (setq kbindings (extend-bindings val `,var-class kbindings)))
                 ((subtypep+ val-class var-class cbindings)
                  (setq kbindings (extend-bindings var `,val-class kbindings)))
                 (t (setq kbindings 
                          (extend-bindings val `((and ,var-class ,val-class))
                                           (extend-bindings var `((and ,var-class ,val-class))
                                                            kbindings)))))
           (tunify var val cbindings kbindings))
          ((occurs-in? var x cbindings) +fail+)
          (t (setq val-class (class-of+ x cbindings kbindings))
             (format t "~%tx:class = ~S:~S" x val-class)
             (cond ((disjoint-p var-class val-class)
                    (return-from tunify-var (values +fail+ kbindings)))
                   ((owl-equivalent-p var-class val-class)
                    (setq kbindings (extend-bindings var x kbindings)))
                   ((subtypep+ var-class val-class kbindings)
                    (setq kbindings 
                          (extend-bindings val `,var-class kbindings)))
                   ((subtypep+ val-class var-class kbindings)
                    (setq kbindings 
                          (extend-bindings var `,val-class kbindings)))
                   (t (setq kbindings 
                            (extend-bindings x `((and ,var-class ,val-class))
                                             (extend-bindings var `((and ,var-class ,val-class))
                                                              kbindings)))))
             (values (extend-bindings var x cbindings) kbindings)))))

(defun ttunify-var (var x kbindings)
  (let* ((var-class (class-of+ var kbindings kbindings))
         (val ())
         (val-class ()))
    (format t "~%ttvar:class = ~S:~S" var var-class)
    (cond ((setq val (symbol-value+ var kbindings))
           (setq val-class (class-of+ val kbindings kbindings))
           (format t "~%ttval:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from ttunify-var +fail+))
                 ((owl-equivalent-p var-class val-class))
                 ((subtypep+ var-class val-class kbindings)
                  (setq kbindings (extend-bindings val `,var-class kbindings)))
                 ((subtypep+ val-class var-class kbindings)
                  (setq kbindings (extend-bindings var `,val-class kbindings)))
                 (t (setq kbindings 
                          (extend-bindings x `((and ,var-class ,val-class))
                                           (extend-bindings val `((and ,var-class ,val-class))
                                                            kbindings)))))
           (ttunify val x kbindings))
          ((and (variable? x) (setq val (symbol-value+ x kbindings)))
           (setq val-class (class-of+ val kbindings kbindings))
           (format t "~%ttval:class = ~S:~S" val val-class)
           (cond ((disjoint-p var-class val-class)
                  (return-from ttunify-var +fail+))
                 ((owl-equivalent-p var-class val-class))
                 ((subtypep+ var-class val-class kbindings)
                  (setq kbindings (extend-bindings val `,var-class kbindings)))
                 ((subtypep+ val-class var-class kbindings)
                  (setq kbindings (extend-bindings var `,val-class kbindings)))
                 (t (setq kbindings 
                          (extend-bindings val `((and ,var-class ,val-class))
                                           (extend-bindings var `((and ,var-class ,val-class))
                                                            kbindings)))))
           (ttunify var val kbindings))
          ((occurs-in? var x kbindings) +fail+)
          (t (setq val-class (class-of+ x kbindings kbindings))
             (format t "~%ttx:class = ~S:~S" x val-class)
             (cond ((disjoint-p var-class val-class) 
                    (return-from ttunify-var +fail+))
                   ((owl-equivalent-p var-class val-class))
                   ((subtypep+ var-class val-class kbindings)
                    (setq kbindings (extend-bindings val `,var-class kbindings)))
                   ((subtypep+ val-class var-class kbindings)
                    (setq kbindings (extend-bindings var `,val-class kbindings)))
                   (t (setq kbindings
                            (extend-bindings x `((and ,var-class ,val-class))
                                             (extend-bindings var `((and ,var-class ,val-class))
                                                              kbindings)))))
             (extend-bindings var x kbindings)))))

(defun unify-skolem (x y bindings cbindings)
  "Both <x> and <y> are skolem function that originate existential quantification.
   In SWCLOS, it represents a successor in a binary relation of (role predecessor successor)."
  (multiple-value-bind (xpred xsuc) (unskolemize x)     ; we assume this role
    (multiple-value-bind (ypred ysuc) (unskolemize y)   ; and this role are the same.
      (let ((kbindings +no-bindings+))
        (multiple-value-setq (bindings cbindings kbindings)
          (unify xpred ypred bindings cbindings))
        (cond ((eq bindings +fail+)
               (values +fail+ cbindings kbindings))
              (t (let ((xclass (class-of+ xsuc bindings cbindings))
                       (yclass (class-of+ ysuc bindings cbindings)))
                   (multiple-value-setq (cbindings kbindings)
                     (tunify xclass yclass cbindings kbindings))
                   (when (get-binding xclass cbindings)
                     (rplaca (get-binding xclass cbindings) xsuc))
                   (when (get-binding yclass cbindings)
                     (rplaca (get-binding yclass cbindings) ysuc)))
                 (setq bindings (extend-bindings xsuc ysuc bindings))
                 (values bindings cbindings kbindings)))))))

(defun unify-xskolem (x y bindings cbindings &optional (kbindings +no-bindings+))
  "Only <x> is a skolem function that originate existential quantification."
  (multiple-value-bind (xpred xsuc) (unskolemize x)
    (declare (ignore xpred))
    ;; we assume both roles from x and y should be the same.
    (let ((xclass (class-of+ xsuc bindings cbindings))
          (yclass (class-of+ y bindings cbindings)))
      (multiple-value-setq (cbindings kbindings)
        (tunify xclass yclass cbindings kbindings))
      (when (get-binding xclass cbindings)
        (rplaca (get-binding xclass cbindings) xsuc))
      (when (get-binding yclass cbindings)
        (rplaca (get-binding yclass cbindings) y)))
    (setq bindings (extend-bindings xsuc y bindings))
    (values bindings cbindings kbindings)))

(defun unify-yskolem (x y bindings cbindings &optional (kbindings +no-bindings+))
  "Only <y> is a skolem function that originate existential quantification."
  (multiple-value-bind (ypred ysuc) (unskolemize y)
    (declare (ignore ypred))
    ;; we assume both roles from x and y should be the same.
    (let ((xclass (class-of+ x bindings cbindings))
          (yclass (class-of+ ysuc bindings cbindings)))
      (multiple-value-setq (cbindings kbindings)
        (tunify xclass yclass cbindings kbindings))
      (when (get-binding xclass cbindings)
        (rplaca (get-binding xclass cbindings) x))
      (when (get-binding yclass cbindings)
        (rplaca (get-binding yclass cbindings) ysuc)))
    (setq bindings (extend-bindings ysuc x bindings))
    (values bindings cbindings kbindings)))

(defun unify-object (x y bindings cbindings)
  (cond ((and (symbolp x) (symbolp x))
         (cond ((and (resource? x) (resource? y))
                (unify (symbol-value x) (symbol-value y) bindings cbindings))
               ((resource? x)
                (unify (symbol-value x) y bindings cbindings))
               ((resource? y)
                (unify x (symbol-value y) bindings cbindings))
               (t (error "Please define ~S and ~S." x y))))
        ((and (rsc-object-p x) (rsc-object-p y))
         (let ((newcbindings (unify-class (class-of x) (class-of y) cbindings)))
           (when (eq newcbindings +fail+)
             (return-from unify-object (values +fail+ +fail+ +fail+)))
           (values (extend-bindings x y bindings) cbindings +no-bindings+)))
        ((symbolp x)
         (cond ((resource? x)
                (unify (symbol-value x) y bindings cbindings))
               ((rsc-object-p y)
                (values (extend-bindings x y bindings) cbindings +no-bindings+))
               (t (error "Please define ~S or ~S." x y))))
        ((symbolp y)
         (cond ((resource? y)
                (unify x (symbol-value y) bindings cbindings))
               ((rsc-object-p x)
                (values (extend-bindings y x bindings) cbindings +no-bindings+))
               (t (error "Please define ~S or ~S." x y))))
        (t (error "Please define ~S and ~S." x y))))

(defun unify-class (c d cbindings)
  (cond ((disjoint-p c d) +fail+)
        ((definitely-owl-equivalent-p c d) cbindings)
        ((subsumed-p c d) (extend-bindings d c cbindings))
        ((subsumed-p d c) (extend-bindings c d cbindings))
        (t (extend-bindings c `(and ,c ,d)
                            (extend-bindings d `(and ,c ,d)
                                             cbindings)))))

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))


#|
(defProperty Knows (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Human))
(defResource Human (rdf:|type| owl:Class))
(defResource Mother (rdf:|type| owl:Class)
  (rdfs:subClassOf Human))
(defResource Pet (rdf:|type| owl:Class)
  (owl:disjointWith Human))
(defResource Cat (rdf:|type| owl:Class)
  (rdfs:subClassOf Pet))
(defIndividual John (rdf:|type| Human)
  (Knows Jane))
(defIndividual Jane (rdf:|type| Human))
(defIndividual Leonid (rdf:|type| Human))
(defIndividual Elizabeth (rdf:|type| Cat))
(unify '(Knows John ?x) '(Knows John Jane))
(unify '(Knows John ?x) '(Knows ?y Leonid))
(unify '(Knows John ?x) '(Knows ?y ?z) +no-bindings+ `((?z . ,Mother)))
(unify '(Knows John ?x) '(Knows ?y Elizabeth))
(unify '(Knows John ?x) '(Knows ?y ?z) `((?z . ,Elizabeth)))
(unify '(Knows John ?x) '(Knows ?y ?z) +no-bindings+ `((?z . ,Cat)))
(unify '(Knows John ?x) '(Knows ?y ?z) +no-bindings+ `((?z . ,Human)))
(unify '(Knows John ?x) '(Knows ?y Elizabeth) +no-bindings+ `((?x . ,Cat)))
(unify '(Knows John ?x) '(Knows ?y Elizabeth) +no-bindings+ `((?x . ,Human))) -> fail
(unify '(Knows John Jane) '(Knows ?y Elizabeth)) -> fail
(unify '(Knows John Jane) '(Knows ?y Leonid))    -> Jane is unified to Leonid.

(unify '(HasChild ?x ?y) '(HasChild John Elizabeth))
(unify '(HasChild John ?y) '(HasChild ?x Elizabeth))
(unify '(HasChild ?x ?y) '(HasChild John Elizabeth) +no-bindings+ `((?x . ,Cat) (?y . ,Cat)))  -> fail
(unify '(HasChild ?x ?y) '(HasChild John Elizabeth) +no-bindings+ `((?x . ,Human) (?y . ,Human)))  -> fail
(unify '(HasChild ?x ?y) '(HasChild John Elizabeth) +no-bindings+ `((?x . ,Cat) (?y . ,Human))) -> fail
|#

;;
;; (unify C(x) C(y)) => C(x/y)
;;
#|
(unify '?x '?y +no-bindings+ `((?x . ,Cat) (?y . ,Cat)))
|#

;;
;; (Intersect C (Not C))           -> clash
;; (Intersect {a b c} (Not {a b})) -> {c}
;; (Intersect {a b c} {a b d})     -> c = d
;; (Intersect {a a1 a2} {a aa1 aa2})     -> (or (and (a1 = aa1) (a2 = aa2)) (and (a1 = aa2) (a2 = aa1)))

#|
(defProperty workAt (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Person)
  (rdfs:range Office))
(defProperty liveAt (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Person)
  (rdfs:range Residence))
(defProperty location (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Building)
  (rdfs:range Address))

(defResource Office (rdfs:subClassOf Building))
(defResource Residence (rdfs:subClassOf Building))

(defIndividual John (rdf:|type| Person)
  (workAt JohnsOffice)
  (liveAt JohnsHome))
(defIndividual JohnsOffice (rdf:|type| Office)
  (location JohnsAddress))
(defIndividual JohnsHome (rdf:|type| Residence)
  (location JohnsAddress))

(=> (and (workAt ?person ?office) (liveAt ?person ?home) (location ?office ?loc) (location ?home ?loc))
    (homeWorker ?person))
|#

#| for AProlog
(require :prolog)
(in-package :prolog)
(<-- (homeWorker ?person)
     (workAt ?person ?office) (liveAt ?person ?home) (location ?office ?loc) (location ?home ?loc))
(<- (workAt John JohnsOffice))
(<- (liveAt John JohnsHome))
(<- (location JohnsOffice JohnsAddress))
(<- (location JohnsHome JohnsAddress))
(?-  (homeWorker ?person))
|#

#| for SNARK
(initialize)
(use-resolution t)
(declare-sort 'Person)
(declare-sort 'Building)
(declare-sort 'Address)
(declare-subsort 'Office 'Building)
(declare-subsort 'Residence 'Building)
;;(declare-sort-partition 'Building 'Office 'Residence)
(declare-constant 'John :sort 'Person)
(declare-constant 'JohnsOffice :sort 'Office)
(declare-constant 'JohnsHome :sort 'Residence)
(declare-constant 'JohnsAddress :sort 'Address)
;;(declare-constant-predicate 'workAt 2 :sort '(boolean Person Office))
;;(declare-constant-predicate 'liveAt 2 :sort '(boolean Person Residence))
;;(declare-constant-predicate 'location 2 :sort '(boolean Building Address))
;;(declare-constant-predicate 'homeWorker 1 :sort '(boolean Person))
(declare-variable '?person :sort 'Person)
(declare-variable '?office :sort 'Office)
(declare-variable '?home :sort 'Residence)
(declare-variable '?loc :sort 'Address)

(assert '(implies
          (and (workAt ?person ?office) (liveAt ?person ?home) (location ?office ?loc) (location ?home ?loc))
          (homeWorker ?person))
        :name 'expressive-restriction-for-OWL)
(assert '(workAt John JohnsOffice) :name 'John-works-at-his-office)
(assert '(liveAt John JohnsHome) :name 'John-lives-at-his-home)
(assert '(location JohnsOffice JohnsAddress) :name 'Johns-office-is-located-at-Johns-address)
(assert '(location JohnsHome JohnsAddress) :name 'Johns-home-is-located-at-Johns-address)
(prove '(homeWorker John))

;; This is with sort.
; Summary of computation:
;        16 formulas have been input or derived (from 8 formulas).
;        15 (94%) were retained.  Of these,
;            5 (33%) were simplified or subsumed later,
;           10 (67%) are still being kept.
; 
; Run time in seconds excluding printing time:
;     0.000   0%   Resolution                   (7 calls)
;     0.000   0%   Condensing                   (8 calls)
;     0.000   0%   Forward subsumption          (8 calls)
;     0.016   1%   Backward subsumption         (8 calls)
;     0.000   0%   Forward simplification       (15 calls)
;     0.000   0%   Backward simplification      (15 calls)
;     0.000   0%   Sortal reasoning             (48 calls)
;     1.767  99%   Other
;     1.783        Total
; Term-hash-array has 18 terms in all.
; Path-index has 20 entries (22 at peak, 22 added, 2 deleted).
; Path-index has 30 nodes (30 at peak, 30 added, 0 deleted).
; Tree-index has 20 entries (22 at peak, 22 added, 2 deleted).
; Tree-index has 30 nodes (32 at peak, 32 added, 2 deleted).
; Retrieved 55 generalization terms in 61 calls.
; Retrieved 43 instance terms in 37 calls.
; Retrieved 19 unifiable terms in 14 calls.
; 
; The agenda of other wffs to process has 1 entry:
;     1 with value (2 12)
; The agenda of everything else has 4 entries:
;     1 with value (4 3)           2 with value (4 15)           1 with value (4 16)
:proof-found

;; This is not sorted.
; Run time in seconds excluding printing time:
;     0.000   0%   Resolution                   (7 calls)
;     0.000   0%   Condensing                   (11 calls)
;     0.000   0%   Forward subsumption          (11 calls)
;     0.000   0%   Backward subsumption         (11 calls)
;     0.000   0%   Forward simplification       (18 calls)
;     0.000   0%   Backward simplification      (18 calls)
;     0.000   0%   Sortal reasoning             (40 calls)
;     1.094 100%   Other
;     1.094        Total
; Term-hash-array has 28 terms in all.
; Path-index has 24 entries (32 at peak, 32 added, 8 deleted).
; Path-index has 32 nodes (32 at peak, 32 added, 0 deleted).
; Tree-index has 24 entries (32 at peak, 32 added, 8 deleted).
; Tree-index has 32 nodes (35 at peak, 35 added, 3 deleted).
; Retrieved 97 generalization terms in 81 calls.
; Retrieved 60 instance terms in 47 calls.
; Retrieved 19 unifiable terms in 14 calls.
; 
; The agenda of other wffs to process has 1 entry:
;     1 with value (2 12)
; The agenda of everything else has 6 entries:
;     1 with value (4 3)           4 with value (4 15)           1 with value (4 16)
:proof-found
|#

#|
(defProperty locatedIn (rdf:|type| owl:TransitiveProperty)
  (rdfs:range Region))
(defIndividual EU (rdf:|type| Region))
(defIndividual German (rdf:|type| Region)
  (locatedIn EU))
(defIndividual French (rdf:|type| Region)
  (locatedIn EU))
(defIndividual UK (rdf:|type| Region)
  (locatedIn EU))

(defResource EUCity (rdf:|type| owl:Class)
  (owl:intersectionOf City
                      (owl:Restriction (owl:onProperty locatedIn)
                                       (owl:hasValue EU))))

(defResource GermanCity (rdf:|type| owl:Class)
  (owl:intersectionOf City
                      (owl:Restriction (owl:onProperty locatedIn)
                                       (owl:hasValue German))))
(defResource FrenchCity (rdf:|type| owl:Class)
  (owl:intersectionOf City
                      (owl:Restriction (owl:onProperty locatedIn)
                                       (owl:hasValue French))))
(defResource UKCity (rdf:|type| owl:Class)
  (owl:intersectionOf City
                      (owl:Restriction (owl:onProperty locatedIn)
                                       (owl:hasValue UK))))

(defResource City (rdfs:subClassOf Region))

(defIndividual Frankfurt (rdf:|type| City)
  (hasTrainTo Paris)
  (hasFrightTo Paris)
  (locatedIn German))
(defIndividual Paris (rdf:|type| City)
  (hasTrainTo Frankfurt)
  (hasTrainTo London)
  (hasFrightTo Frankfurt)
  (locatedIn French))
(defIndividual London (rdf:|type| City)
  (hasTrainTo Paris)
  (hasFrightTo Paris)
  (hasFrightTo Frankfurt)
  (locatedIn UK))

(defProperty withVehicle (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Travel)
  (rdfs:range Vehcle))
(defProperty departFrom (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Travel)
  (rdfs:range Region))
(defProperty departTo (rdf:|type| owl:ObjectProperty)
  (rdfs:domain Travel)
  (rdfs:range Region))

(defProperty withTrain (rdf:|type| owl:ObjectProperty)
  (rdfs:subPropertyOf withVehicle))
(defResource withAirPlane (rdf:|type| owl:ObjectProperty)
  (rdfs:subPropertyOf withVehicle))

|#
