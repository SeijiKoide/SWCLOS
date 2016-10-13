;;;-*- Mode: common-lisp; syntax: common-lisp; package: :gx; base: 10 -*-
;;;
;;;; Literal in SWCLOS RDF Subsystem
;;;
;; History
;; -------
;; 2010.12.08    This file is copied from Literal in SWCLOS.
;;; ==================================================================================

(cl:provide :rdfliteral)

(cl:eval-when (:execute :load-toplevel :compile-toplevel)
  (cl:require :swclospackages)

  ) ; end of eval-when

(in-package :gx)

;;;
;;;; Plane Literal
;;;
;;; A plane literal without language tag is internalized to string in lisp.
;;; A plane literal with language tag is internalized to an instance of class rdf:inLang.

(defun lang? (x)
  "<x> is 2-letter language code as keyword symbol, e.g., :ja for Japanese, :en for English.
   See http://www.loc.gov/standards/iso639-2/php/code_list.php, but :en-US also accepted."
  (when (symbolp x)
    (or (eq x :ja)
        (eq x :en)
        (eq x :fr)
        (eq x :de)
        (eq x :bg) ;Bulgarian
        (eq x :da) ;Danish
        (eq x :nl) ;Dutch
        (eq x :el) ;Greek
        (eq x :es) ;Spanish
        (eq x :fi) ;Finnish
        (eq x :it) ;Italian
        (eq x :ko) ;Korean
        (eq x :no) ;Norwegian
        (eq x :pl) ;Polish
        (eq x :pt) ;Portuguese
        (eq x :ru) ;Russian
        (eq x :sw) ;Swedish
        (eq x :th) ;Thai
        (eq x :tr) ;Turkish
        (eq x :zh) ;Chinese
        (eq x :en-us)
        (eq x :en-US))))
;; See, http://www.loc.gov/standards/iso639-2/php/code_list.php

;;;
;;;; Plane Literal with Lang
;;; A Lang object has <lang> and <content>.
;;; It is print out such as "Vine@fr".

(defclass rdf:|inLang| ()
  ((lang :initarg :lang :accessor lang)
   (content :initarg :content :accessor content)))

(defmethod print-object ((object rdf:|inLang|) stream)
  (format stream "\"~A\"@~A" (content object) (lang object)))

(defmethod equals ((obj1 rdf:|inLang|) (obj2 rdf:|inLang|))
  "Two plane literal with lang are equal if langs are equal and contents are equal." 
  (and (equal (lang obj1) (lang obj2))
       (equal (content obj1) (content obj2))))

(defun @ (content lang)
  "returns an instance of rdf:inLang structure. <content> must be a string.
   the string of <lang> may be any string or symbol and it must designate language tag."
  (assert (stringp content))
  ;; lang is internalized downcased keyword
  (make-instance 'rdf:|inLang|
    :lang (intern (string-downcase (string lang)) :keyword)
    :content content))
