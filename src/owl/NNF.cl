;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; NNF transformation module
;;;
;;; This program is borrowed from AIMA
;;;  2008, 2009 (c) Seiji Koide
;;;
;; History
;; -------
;; 2008.08.18    move-not-inwards, conjunction, disjunction, make-exp, op, args, arg1, arg2 are moved to GxType
;; 2008.03.15    File created.

(cl:defpackage :gx
  (:export "variable?")
  )

(in-package :gx)

;;.............................................................................................
;;
;; Convert Expressions to Normal Form from AIMA
;;

(defun conjuncts (sentence)
  "Return a list of the conjuncts in this sentence."
  (cond ((eq (op sentence) 'and) (args sentence))
        ((eq sentence 't) nil)
        (t (list sentence))))

(defun disjuncts (sentence)
  "Return a list of the disjuncts in this sentence."
  (cond ((eq (op sentence) 'or) (args sentence))
        ((eq sentence nil) nil)
        (t (list sentence))))

(defconstant +logical-connectives+ '(and or not => <=>))
(defconstant +logical-quantifiers+ '(forall exists))

(defun atomic-clause? (sentence)
  "An atomic clause has no connectives or quantifiers."
  (not (or (member (op sentence) +logical-connectives+)
           (member (op sentence) +logical-quantifiers+))))

(defun literal-clause? (sentence)
  "A literal is an atomic clause or a negated atomic clause."
  (or (atomic-clause? sentence)
      (and (negative-clause? sentence) (atomic-clause? (arg1 sentence)))))

(defun negative-clause? (sentence)
  "A negative clause has NOT as the operator."
  (eq (op sentence) 'not))

(defun merge-disjuncts (disjuncts)
  "Return a CNF expression for the disjunction."
  ;; The argument is a list of disjuncts, each in CNF.
  ;; The second argument is a list of conjuncts built so far.
  (case (length disjuncts)
    (0 'nil)                      ; seiji
    (1 (first disjuncts))
    (t (conjunction
        (let ((result ()))
          (loop for y in (conjuncts (merge-disjuncts (rest disjuncts))) do
                (loop for x in (conjuncts (first disjuncts)) do
                      (push (disjunction (append (disjuncts x) (disjuncts y)))
                            result)))
          (nreverse result))))))

(defun eliminate-implications (p)
  (if (literal-clause? p) p
    (case (op p)
      (=>  `(or ,(arg2 p) (not ,(arg1 p))))
      (<=> `(and (or ,(arg1 p) (not ,(arg2 p)))
                 (or (not ,(arg1 p)) ,(arg2 p))))
      (t   (cons (op p) (mapcar #'eliminate-implications (args p)))))))

(defun has-variable? (p)
  (cond ((null p) nil)
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

;;
;;
;;

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
  (intern (format nil "~{~a~}" args)))

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
  (intern (format nil "$~A_~D" name (incf *new-variable-counter*))))

;;;
;;; Followings are by Seiji
;;;

(defun skolem-constant-p (x)
  (and (symbolp x) (char= (char (string x) 0) #\$)))

(defun skolem-p (x)
  (or (and (consp x) (skolem-constant-p (first x)))
      (skolem-constant-p x)))

(defun unskolemize (sk)
  (cond ((consp sk)
         (let ((skstr (string (car sk))))
           (values (cdr sk) (intern (subseq skstr 1 (position #\_ skstr))))))
        (t (let ((skstr (string sk)))
             (values nil (intern (subseq skstr 1 (position #\_ skstr))))))))
