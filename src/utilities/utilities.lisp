;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;;; Basic utility functions and macros, used throughout the code. 
;;; This file is copied from AIMA utilities/utilities.lisp by Seiji.
;;; Loop macro and related macros for control are removed and all of 
;;; codes are revised for Common Lisp loop macros. 

(cl:defpackage :utils
  (:export deletef reuse-cons mklist length>1 length=1 last1 make-exp mappend concat-symbol
           op args arg1 arg2 starts-with stringify true print-repeated 
           same-element-p unfiablly-same-type-p length-in-1diff
           dprint)
  )
(in-package :utils) ; by Seiji

;; Removed allegro package-lock override code that was here.

;;;; Control Flow Macros

(#+excl excl:without-package-locks
        #-excl progn
        (defmacro while (test do &body body)
          "Execute body while the test is true."
          (assert (eq do 'do))
          `(do () ((not ,test) nil) ,@body)))

(defmacro deletef (item sequence &rest keys &environment env)
  "Destructively delete item from sequence, which must be SETF-able."
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion sequence env)
    (assert (= (length stores) 1))
    (let ((item-var (gensym "ITEM")))
      `(let* ((,item-var ,item)
              ,@(mapcar #'list temps vals)
              (,(first stores) (delete ,item-var ,access-form ,@keys)))
         ,store-form))))

(defmacro define-if-undefined (&rest definitions)
  "Use this to conditionally define functions, variables, or macros that
  may or may not be pre-defined in this Lisp.  This can be used to provide
  CLtL2 compatibility for older Lisps."
  `(progn
     ,@(mapcar #'(lambda (def)
                   (let ((name (second def)))
                     `(when (not (or (boundp ',name) (fboundp ',name)
                                     ;; 5oct05 charley cox
                                     ;; this was just a call to special-form-p
                                     ;; which has been replaced by
                                     ;; special-operator-p in ANS.
                                     (if (fboundp 'special-operator-p)
                                         (funcall 'special-operator-p ',name)
                                       (funcall 'special-form-p ',name))
                                     (macro-function ',name)))
                        ,def)))
         definitions)))

;;;; List Utilities

(defun mklist (x)
  "If <x> is a list, return it; otherwise return a singleton list, ( <x> )."
  (if (listp x) x (list x)))

(defun length>1 (list)
  "Is this a list of two or more elements?"
  (and (consp list) (cdr list)))

(defun length=1 (list)
  "Is this a list of exactly one element?"
  (and (consp list) (null (cdr list))))

(defun length-in-1diff (lst1 lst2)
  (let ((len1 (length lst1))
        (len2 (length lst2)))
    (or (= len1 len2)
        (= len1 (1- len2))
        (= (1- len1) len2))))

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

(defun mappend (fn &rest lists)
  "Apply <fn> to respective elements of list(s), and append results."
  (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun starts-with (list element)
  "Is this a list that starts with the given element?"
  (and (consp list) (eq (first list) element)))

(defun last1 (list)
  "Return the last element of a list."
  (first (last list)))

(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun right-rotate (list)
  "Move the last element to the front of the list."
  (append (last list) (butlast list)))

(defun transpose (list-of-lists)
  "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => ((a d) (b e) (c f))."
  (apply #'mapcar #'list list-of-lists))

(defun reuse-cons (x y x-y)
  "Return (cons <x> <y>), or reuse <x-y> if it is equal to (cons <x> <y>)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
    (cons x y)))

"An expression is a list consisting of a prefix operator followed by args,
Or it can be a symbol, denoting an operator with no arguments.
Expressions are used in Logic, and as actions for agents."

(defmethod make-exp (op &rest args) (cons op args))
(defgeneric op (exp) (:documentation "Operator of an expression"))
(defmethod op (exp) exp)
(defmethod op ((exp cons)) (first exp))
(defgeneric args (exp) (:documentation "Arguments of an expression"))
(defmethod args (exp)
  (declare (ignore exp))
  nil)
(defmethod args ((exp cons)) (rest exp))
(defgeneric arg1 (exp) (:documentation "First argument"))
(defmethod arg1 ((exp cons)) (second exp))
(defgeneric arg2 (exp) (:documentation "Second argument"))
(defmethod arg2 ((exp cons)) (third exp))

(defsetf args (exp) (new-value)
  `(setf (cdr ,exp) ,new-value))

(defun prefix->infix (exp)
  "Convert a fully parenthesized prefix expression into infix notation."
  (cond ((atom exp) exp)
        ((length=1 (args exp)) exp)
        (t (insert-between (op exp) (mapcar #'prefix->infix (args exp))))))

(defun insert-between (item list)
  "Insert item between every element of list."
  (if (or (null list) (length=1 list))
      list
    (list* (first list) item (insert-between item (rest list)))))

;;;; Numeric Utilities

(defconstant infinity most-positive-single-float)
(defconstant minus-infinity most-negative-single-float)

(defun average (numbers)
  "Numerical average (mean) of a list of numbers."
  (/ (sum numbers) (length numbers)))

(defun running-average (avg new n)
  "Calculate new average given previous average over n data points"
  (/ (+ new (* avg n)) (1+ n)))

(defun square (x) (* x x))

(defun sum (numbers &optional (key #'identity))
  "Add up all the numbers; if KEY is given, apply it to each number first."
  (if (null numbers)
      0
    (+ (funcall key (first numbers)) (sum (rest numbers) key))))

(defun between (x y z)
  "Predicate; return t iff number x is between numbers y and z."
  (or (<= y x z) (>= y x z)))

(defun rms-error (predicted target)
  "Compute root mean square error between predicted list and target list"
  (sqrt (ms-error predicted target)))

(defun ms-error (predicted target &aux (sum 0))
  "Compute mean square error between predicted list and target list"
  (mapc #'(lambda (x y) (incf sum (square (- x y)))) predicted target)
  (/ sum (length predicted)))

(defun boolean-error (predicted target)
  (if (equal predicted target) 0 1))

(defun dot-product (l1 l2 &aux (sum 0)) ;;; dot product of two lists
  (mapc #'(lambda (x1 x2) (incf sum (* x1 x2))) l1 l2)
  sum)

(defun iota (n &optional (start-at 0))
  "Return a list of n consecutive integers, by default starting at 0."
  (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))

(defun random-integer (from to)
  "Return an integer chosen at random from the given interval."
  (+ from (random (+ 1 (- to from)))))

(defun normal (x mu sigma)
  (/ (exp (/ (- (square (- x mu))) (* 2 (square sigma)))) 
     (* (sqrt (* 2 pi)) sigma)))

(defun sample-with-replacement (n population)
  (let ((result nil))
    (dotimes (i n) (push (random-element population) result))
    result))

(defun sample-without-replacement (n population &optional
                                     (m (length population)))
  ;; Assumes that m = (length population)
  (cond ((<= n 0) nil)
        ((>= n m) population)
        ((>= (/ n m) (random 1.0))
         (cons (first population) (sample-without-replacement
                                   (- n 1) (rest population) (- m 1))))
        (t (sample-without-replacement n (rest population) (- m 1)))))

(defun fuzz (quantity &optional (proportion .1) (round-off .01))
  "Add and also subtract a random fuzz-factor to a quantity."
  (round-off (+ quantity
                (* quantity (- (random (float proportion))
                               (random (float proportion)))))
             round-off))

(defun round-off (number precision)
  "Round off the number to specified precision. E.g. (round-off 1.23 .1) = 1.2"
  (* precision (round number precision)))

;;;; Trivial Functions

(defun nothing (&rest args)
  "Don't do anything, and return nil."
  (declare (ignore args))
  nil)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(defun true (&rest args) "Always return true." (declare (ignore args)) t)

#-(or MCL Lispworks) ;; MCL, Lispworks already define this function
(defun false (&rest args) "Always return false." (declare (ignore args)) nil)

(defun required (&optional (msg "A required argument is missing.") &rest args)
  "If this ever gets called, it means something that was required was not
  supplied.  Use as default value for &key args or defstruct slots."
  (apply #'error msg args))

;;;; Utilities for strings and symbols and printing

(defun stringify (exp)
  "Coerce argument to a string."
  (cond ((stringp exp) exp)
        ((symbolp exp) (symbol-name exp))
        (t (format nil "~A" exp))))

(defun concat-symbol (&rest args)
  "Concatenate the args into one string, and turn that into a symbol."
  (intern (format nil "~{~a~}" args)))


;;;; Assorted conversion utilities and predicates

(defun copy-array (a &aux (dim (array-dimensions a))
                     (b (make-array dim)))
  "Make a copy of an array."
  (copy-subarray a b nil dim)
  b)

(defun copy-subarray (a b indices dim)
  (if dim
      (dotimes (i (first dim))
        (copy-subarray a b (append indices (list i)) (rest dim)))
    (setf (apply #'aref (cons b indices))
      (apply #'aref (cons a indices)))))

(defun array->vector (array)
  "Convert a multi-dimensional array to a vector with the same elements."
  (make-array (array-total-size array) :displaced-to array))


(defun plot-alist (alist file)
  (with-open-file (stream file :direction :output :if-does-not-exist :create
                          :if-exists :supersede)
    (dolist (xy alist)
      (format stream "~&~A ~A~%" (car xy) (cdr xy)))))

(defun copy-hash-table (H1 &optional (copy-fn #'identity))
  (let ((H2 (make-hash-table :test #'equal)))
    (maphash #'(lambda (key val) (setf (gethash key H2) (funcall copy-fn val)))
             H1)
    H2))

(defun hash-table->list (table)
  "Convert a hash table into a list of (key . val) pairs."
  (maphash #'cons table))

(defun hprint (h &optional (stream t)) 
  "prints a hash table line by line"
  (maphash #'(lambda (key val) (format stream "~&~A:~10T ~A" key val)) h)
  h)

(defun compose (f g)
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(defun the-biggest (fn l)
  (let ((biggest (first l))
        (best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (let ((val (funcall fn x)))
        (when (> val best-val)
          (setq best-val val)
          (setq biggest x))))
    biggest))

(defun the-biggest-random-tie (fn l)
  (random-element
   (let ((biggest (list (first l)))
         (best-val (funcall fn (first l))))
     (dolist (x (rest l))
       (let ((val (funcall fn x)))
         (cond ((> val best-val)
                (setq best-val val)
                (setq biggest (list x)))
               ((= val best-val)
                (push x biggest)))))
     biggest)))

(defun the-biggest-that (fn p l)
  (let ((biggest (first l))
        (best-val (funcall fn (first l))))
    (dolist (x (rest l))
      (when (funcall p x)
        (let ((val (funcall fn x)))
          (when (> val best-val)
            (setq best-val val)
            (setq biggest x)))))
    biggest))

(defun the-smallest (fn l)
  (the-biggest (compose #'- fn) l))

(defun the-smallest-random-tie (fn l)
  (the-biggest-random-tie (compose #'- fn) l))

(defun the-smallest-that (fn p l)
  (the-biggest-that (compose #'- fn) p l))

;;;; Same element ?

(defun same-element-p (fn sequence)
  (not (not (reduce #'(lambda (x y)
                        (if (funcall fn x y) x
                          (return-from same-element-p nil)))
                    sequence))))

(defun unfiablly-same-type-p (sequence)
  (not (not (reduce #'(lambda (x y)
                        (if (eql (type-of x) (type-of y)) x
                          (return-from unfiablly-same-type-p nil)))
                    sequence))))

;;;; Utilities for strings and symbols and printing

(defun print-repeated (string n &optional (stream t))
  "Print the string n times."
  (dotimes (i n)
    (format stream "~A" string)))

;;;; Debugging tool

(defvar *debugging* nil)

(defun dprint (&rest args)
  "Echo all the args when *debugging* is true.  Return the first one."
  (when *debugging* (format t "~&~{~S ~}~%" args))
  (first args))
