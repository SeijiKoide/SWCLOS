;;;-*- mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;; NNF transformation module
;;;
;;; This program is borrowed from AIMA
;;;  2008 (c) Seiji Koide
;;;
;;; History
;;; -------
;;; 2008.03.15    File created.

(cl:defpackage :gx
  (:export "op" "args" "arg1" "arg2" "variable?")
  )

(in-package :gx)

;;;.............................................................................................
;;;
;;; Convert Expressions to Normal Form (Conjunctive, Implicative), from AIMA
;;;

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) "Operator of an expression" (if (listp exp) (cl:first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (cl:rest exp) cl:nil))
(defun arg1 (exp) "First argument" (cl:first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))

(defun conjunction (args)
  "Form a conjunction with these args."
  (case (length args)
    (0 't)                   ; seiji
    (1 (cl:first args))
    (t (cons 'and args))))

(defun Conjunction (args)
  "Form a conjunction with these args."
  (case (length args)
    (0 't)                   ; seiji
    (1 (cl:first args))
    (t (cons 'Intersect args))))

(defun disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 'cl:nil)              ; seiji
    (1 (cl:first args))
    (t (cons 'or args))))

(defun Disjunction (args)
  "Form a disjunction with these args."
  (case (length args)
    (0 'cl:nil)              ; seiji
    (1 (cl:first args))
    (t (cons 'UnionOf args))))

(defun conjuncts (sentence)
  "Return a list of the conjuncts in this sentence."
  (cond ((eq (op sentence) 'and) (args sentence))
        ((eq sentence 't) cl:nil)
        (t (list sentence))))

(defun disjuncts (sentence)
  "Return a list of the disjuncts in this sentence."
  (cond ((eq (op sentence) 'or) (args sentence))
        ((eq sentence 'false) cl:nil)
        (t (list sentence))))

(defun move-not-inwards (p)
  "Given P, return ~P, but with the negation moved as far in as possible."
  (case (op p)
    ((t) 'cl:nil)             ; seiji
    ((cl:nil) 't)             ; seiji
    (not (arg1 p))
    (and (disjunction (mapcar #'move-not-inwards (args p))))
    (or  (conjunction (mapcar #'move-not-inwards (args p))))
    (forall (make-exp 'exists (arg1 p) (move-not-inwards (arg2 p))))
    (exists (make-exp 'forall (arg1 p) (move-not-inwards (arg2 p))))
    (t (make-exp 'not p))))

(defconstant +logical-connectives+ '(and or not => <=> Not))       ; Seiji for tableau
(defconstant +logical-quantifiers+ '(forall exists Forall Exists)) ; Seiji for tableau

(defun atomic-clause? (sentence)
  "An atomic clause has no connectives or quantifiers."
  (not (or (cl:member (op sentence) +logical-connectives+)
           (cl:member (op sentence) +logical-quantifiers+))))

(defun literal-clause? (sentence)
  "A literal is an atomic clause or a negated atomic clause."
  (or (atomic-clause? sentence)
      (and (negative-clause? sentence) (atomic-clause? (arg1 sentence)))))

(defun negative-clause? (sentence)
  "A negative clause has NOT as the operator."
  (or (eq (op sentence) 'not)
      (eq (op sentence) 'Not)))                                   ; Seiji for tableau

(defun merge-disjuncts (disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  ;; The second argument is a list of conjuncts built so far.
  (case (length disjuncts)
    (0 'cl:nil)                      ; seiji
    (1 (cl:first disjuncts))
    (t (conjunction
        (let ((result ()))
          (loop for y in (conjuncts (merge-disjuncts (cl:rest disjuncts))) do
                (loop for x in (conjuncts (cl:first disjuncts)) do
                      (push (disjunction (append (disjuncts x) (disjuncts y)))
                            result)))
          (nreverse result))))))

(defun MergeDisjuncts (disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  ;; The second argument is a list of conjuncts built so far.
  (case (length disjuncts)
    (0 'cl:nil)                      ; seiji
    (1 (cl:first disjuncts))
    (t (Conjunction
        (let ((result ()))
          (loop for y in (conjuncts (MergeDisjuncts (cl:rest disjuncts))) do
                (loop for x in (conjuncts (cl:first disjuncts)) do
                      (push (Disjunction (append (disjuncts x) (disjuncts y)))
                            result)))
          (nreverse result))))))

(defun eliminate-implications (p)
  (if (literal-clause? p) p
    (case (op p)
      (=>  (cond ((has-variable? (arg1 p)) `(or ,(arg2 p) (not ,(arg1 p))))
                 (t `(UnionOf ,(arg2 p) (Not ,(arg1 p))))))
      (<=> (cond ((has-variable? (arg1 p)) `(and (or ,(arg1 p) (not ,(arg2 p)))
                                                 (or (not ,(arg1 p)) ,(arg2 p))))
                 (t `(Intersect (UnionOf ,(arg1 p) (Not ,(arg2 p)))
                                (UnionOf (Not ,(arg1 p)) ,(arg2 p))))))
      (t   (cons (op p) (mapcar #'eliminate-implications (args p)))))))

(defun has-variable? (p)
  (cond ((null p) cl:nil)
        ((consp p) (some #'has-variable? p))
        (t (variable? p))))

;; in order to check the disjointness, conjuncitive normal form should be.
(defun ->cnf (p &optional vars)
  "Convert a sentence p to conjunctive normal form [p 279-280]."
  ;; That is, return (and (or ...) ...) where 
  ;; each of the conjuncts has all literal disjuncts.
  ;; VARS is a list of universally quantified variables that P is in scope of.
  (setf p (eliminate-implications (logic p)))
  (case (op p)
    (not (let ((p2 (move-not-inwards (arg1 p))))
           (if (literal-clause? p2) p2 (->cnf p2 vars))))
    (and (conjunction (mappend #'(lambda (q) (conjuncts (->cnf q vars))) (args p))))
    (or  (merge-disjuncts (mapcar #'(lambda (q) (->cnf q vars)) (args p))))
    (forall (let ((new-vars (mapcar #'new-variable  (mklist (arg1 p)))))
              (->cnf (sublis (mapcar #'cons  (mklist (arg1 p)) new-vars)
                             (arg2 p))
                     (append new-vars vars))))
    (exists (->cnf (skolemize (arg2 p) (arg1 p) vars) vars))
    (t   p) ; p is atomic
    ))

(defun ->inf (p)
  "Convert a sentence p to implicative normal form [p 282]."
  (conjunction (mapcar #'cnf1->inf1 (conjuncts (->cnf p)))))

(defun cnf1->inf1 (p)
  ;; P is of the form (or (not a) (not b) ... c d ...)
  ;; Convert to: (=> (and a b ...) (or c d ...))
  ;; where a,b,c,d ... are positive atomic clauses
  (let ((lhs (mapcar #'arg1 (remove-if-not #'negative-clause? (disjuncts p))))
        (rhs (remove-if #'negative-clause? (disjuncts p))))
    `(=> ,(conjunction lhs) ,(disjunction rhs))))

(defun logic (sentence)
  "Canonicalize a sentence into proper logical form."
  (cond ((stringp sentence) (->prefix sentence))
        (t sentence)))

;;;
;;;
;;;

(defvar *new-variable-counter* 0)

(defun variable? (x)
  "Is x a variable (a symbol starting with ?)?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun new-variable (var)
  "Create a new variable.  Assumes user never types variables of form $X.9"
  (concat-symbol (if (variable? var) "" "?")
                 var "." (incf *new-variable-counter*)))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format cl:nil "~{~a~}" args)))

(defun skolemize (p vars outside-vars)
  "Within the proposition P, replace each of VARS with a skolem constant,
  or if OUTSIDE-VARS is non-null, a skolem function of them."
  (sublis (mapcar #'(lambda (var)
                      (cons var (if (null outside-vars)
                                    (skolem-constant var)
                                  (cons (skolem-constant var) outside-vars))))
            (mklist vars))
          p))

(defun skolem-constant (name)
  "Return a unique skolem constant, a symbol starting with '$'."
  (intern (format cl:nil "$~A_~D" name (incf *new-variable-counter*))))
