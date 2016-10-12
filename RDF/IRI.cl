;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; IRI module
;;;

(in-package :gx)

(export '(iri boundp bound-value iri-escape-for-symbol-name))

;;;
;;;; IRI in SWCLOS system
;;;
;;; Any IRI must be unique in SWCLOS, because any IRI or URI is unique in the WWW. 
;;; The uniqueness of IRI is assured by interning it in SWCLOS system. 
;;;
;;; IRIs are defined similarly to URIs, but the set of unreserved characters is 
;;; extended by adding the characters of UNICODE. Thus, using Allegro Lisp non-alisp8 
;;; system, IRI library that allows UNICODE characters are naturally equivalent to URI 
;;; library in ACL.
;;;
;;; A triple subject/predicate/object in RDF is embodied as CLOSobject/slotname/slotvalue
;;; in SWCLOS, and subjective CLOSobject is bound to the subjective IRI. Precisely, 
;;; a subjective IRI is an instance of class <iri> in SWCLOS that is a subclass 
;;; of <uri> in ACL library. 
;;;
;;; Read macro `\<' reads a uri string and produces an <iri>.
;;; An IRI reference in files and on listener windows is internalized to an instance of 
;;; class <iri>. An instance of class <iri> is externalized (printed by `%W') as the same 
;;; appearance of input IRI data.
;;; ----------------------------------------------------------------------------------
;;; <http://www.w3.org/2000/01/rdf-schema#Resource>    -> 
;;;                                   <http://www.w3.org/2000/01/rdf-schema#Resource>
;;; rdfs:Resource (if defined as node)                 -> rdfs:Resource
;;; (eq <http://somewhere> <http://somewhere>)         -> true
;;; (eq <http://some%20where> <http://some%20where>)   -> true
;;; (eq <http://somewhere> <http://some%20where>)      -> false
;;; ----------------------------------------------------------------------------------
;;;
;;; An instance of <iri> has an extra slot for its value just like lisp symbol. <iri-boundp> 
;;; and <iri-value> is used for an <iri> just like <boundp> and <symbol-value> for symbols.
;;;
;;; Two trailing characters '\<\<' invokes the evaluation of the instance of <iri> and 
;;; returns a value bound to the <iri>.
;;; See, reader macro <gx::double-angle-bracket-reader>.

(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(inline iri-p boundp iri-value bound-value)))

(defclass iri (uri)
  ((value :accessor iri-value))
  (:documentation "iri in SWCLOS that is a subclass of uri and able to bind a value to, 
just like lisp symbol. The accessor <iri-value> allows to get and set the bound value of an <iri>."))

(defmethod print-object ((iri iri) stream)
  (if *print-escape*
      (format stream "<~a>" (render-uri iri nil))
    (render-uri iri stream)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (proclaim '(inline iri-p iri-boundp iri-value)))

(defun iri-boundp (x)
  "Is <x> an iri and bound at its value slot?"
  (etypecase x
    (string (iri-boundp (iri x)))
    (iri (slot-boundp x 'value))))

(defmethod iri-value ((str string))
  "returns bound value of iri value from <str>."
  (iri-value (iri str)))

(defmacro %iri-value (uri)
  "This macro should be used by programmers, when <uri> is definitely <uri> here."
  (slot-value uri 'value))

(defun iri-p (x)
  "Is <x> an instance of iri?"
  (cl:typep x 'iri))

#|
(defmethod iri ((sym symbol))
  (cond ((cl:boundp sym)
         (let ((val (symbol-value sym)))
           (cond ((cl:typep val 'node) (slot-value val 'iri))
                 (t (error "~S is not in RDF universe." sym)))))
        (t (error "Not defined symbol ~S." sym))))

(defun boundp (x)
  "Is <x> a symbol or a iri and bound at its value slot?"
  (etypecase x
    (symbol (cl:boundp x))
    (iri (slot-boundp x 'value))
    (node t)))

(defun bound-value (x)
  "returns a bound value of <x>."
  (etypecase x
    (symbol (symbol-value x))
    (iri (slot-value x 'value))
    (node x)))

(defmethod (setf bound-value) (value (x symbol))
  (setf (symbol-value x) value))
(defmethod (setf bound-value) (value (x iri))
  (setf (iri-value x) value))
|#
;;;
;;;; IRI Escaping
;;;
;;; See, rfc2396 for URI escaping

(defun iri-reserved-char-p (char)
  "Is this <char> reserved for iri?"
  (or (char= char #\:)
      (char= char #\/)
      (char= char #\?)
      (char= char #\#)
      (char= char #\[)
      (char= char #\])
      (char= char #\@)   ; up to here gen-delims
      (char= char #\!)
      (char= char #\$)
      (char= char #\&)
      (char= char #\')
      (char= char #\()
      (char= char #\))
      (char= char #\*)
      (char= char #\+)
      (char= char #\,)
      (char= char #\;)
      (char= char #\=)   ; up to here sub-delims
      ))

(defun iri-gen-delims-p (char)
  (or (char= char #\:)
      (char= char #\/)
      (char= char #\?)
      (char= char #\#)
      (char= char #\[)
      (char= char #\])
      (char= char #\@)))

(defun iri-sub-delims-p (char)
  (or (char= char #\!)
      (char= char #\$)
      (char= char #\&)
      (char= char #\')
      (char= char #\()
      (char= char #\))
      (char= char #\*)
      (char= char #\+)
      (char= char #\,)
      (char= char #\;)
      (char= char #\=)))

(defun iri-delimiter-p (char)
  (or (char= char #\<)
      (char= char #\>)
      (char= char #\#)
      (char= char #\%)
      (char= char #\")))

;;;(defun iri-marked-char-p (char)
;;;  "Is this <char> marked for iri?"
;;;  (or (char= char #\-)
;;;      (char= char #\_)
;;;      (char= char #\.)
;;;      (char= char #\~)
;;;      (char= char #\!)
;;;      (char= char #\')
;;;      (char= char #\()
;;;      (char= char #\))
;;;      (char= char #\*)
;;;      ))

;;;(defun iri-unwise-p (char)
;;;  (or (char= char #\{)
;;;      (char= char #\})
;;;      (char= char #\|)
;;;      (char= char #\\)
;;;      (char= char #\^)
;;;      (char= char #\[)
;;;      (char= char #\])
;;;      (char= char #\`)))

(defun iri-escape-for-symbol-name (symbol-name)
  "<symbol-name> is a string of symbol. It may turn out to iri fragment or a tail of path in IRI. So, 
it must be escaped for gen-delims characters except #\: and #\@. In this version, a space is also escaped."
  (cond ((and (> (length symbol-name) 5) (string= "http:" (subseq symbol-name 0 5)))
         symbol-name) ; this is for ontology URIs
        (t (flet ((escape-p (c)
                            (declare (optimize (speed 3) (safety 1)))
                            (or (char= c #\/)
                                (char= c #\?)
                                (char= c #\#)
                                (char= c #\[)
                                (char= c #\])
                                (eq (char-code c) #x20))))
             (labels ((escape (str)
                              (let ((pos 0))
                                (cond ((setq pos (position-if #'escape-p str)) ; found
                                       (let ((c (char str pos)))
                                         (concatenate 'cl:string
                                           (subseq str 0 pos)
                                           (format nil "%~X" (char-code c))
                                           (escape (subseq str (1+ pos))))))
                                      (t str)))))
               (escape symbol-name))))))

(defun iri-de-escape (str)
  "This function decodes Percent-Encoding to characters."
  (let ((pos 0))
    (cond ((setq pos (position #\% str :test #'char=))
           (let ((c (code-char (parse-integer (subseq str (1+ pos) (min (length str) (+ pos 3)))
                                              :radix 16))))
             (concatenate 'cl:string 
               (subseq str 0 pos)
               (string c)
               (iri-de-escape (subseq str (+ pos 3))))))
          (t str))))

;;;
;;;; URI APIs fixes
;;;

(defun null-iri-p (uri)
  "returns true if <uri> is nil, null string, or <uri> is a uri and its rendered string is null."
  (etypecase uri
    (null t)
    (string (string= "" uri))
    (uri (string= "" (render-uri uri nil)))))

;;;
;;;; IRI Methods
;;;
;;; Three methods are defined for generic function <iri>: when <thing> is an iri (instance of 
;;; class iri), its interned value is returned. When <thing> is a uri but not iri, the class is 
;;; changed to <iri> and interned. When <thing> is a string, an instance of <iri> is created and 
;;; interned. 

(defmethod iri ((thing iri))
  "returns interned <thing> for class <gx:iri>."
  (intern-uri thing))

(defmethod iri ((thing uri))
  "change class <uri> of <thing> to <iri> and returns interned <thing>."
  (when (and (uri-host thing) (null (uri-path thing)))
    (setf (uri-path thing) "/"))
  (unless (cl:typep thing 'iri)
    (change-class thing 'iri))
  (intern-uri thing))

;;;
;;; See also, 
;;; \<a href='http://www.franz.com/support/documentation/8.1/doc/operators/uri/parse-uri.htm\>parse-uri'\</a\>
;;; in ACL document.
;;;

(defmethod iri ((thing cl:string))
  "when iri host exists but no iri path on the iri in <thing>, this method adds '/' to iri path. 
   This is required for the namespace interning."
  ;(setq str (substitute-pattern "&" "&#38;" str))   ; "&#38;" is converted to "&"
  (let ((parsed (parse-uri thing :class 'iri)))
    (when (and (uri-host parsed) (null (uri-path parsed)))
      (setf (uri-path parsed) "/"))
;;;    (unless (cl:typep parsed 'iri)
;;;      (change-class parsed 'iri))
    (intern-uri parsed)
    ))

(defmethod iri ((thing t))
  "signals an error."
  (error "Cannot coerce ~S to a gx:iri"))

;;;
;;; To list all iris for resource, call <list-all-entity-uris>. See gxutils module.
;;;

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Nov-15-2010
;;;

(cl:provide :iri)
