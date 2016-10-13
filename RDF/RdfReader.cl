;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; RDF/XML parser module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright (c) 2002,2004 by Galaxy Express Corporation, Japan.
;;; Copyright (c) 2008-2009 Seiji Koide.
;;;
;; History
;; -------
;; 2009.11.01    RdfParser is renamed to RdfReader.
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2004.05.09    File created and contents are copied from Rdf.cl

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :rdfparser)
  )

(in-package :gx)

(export '(read-rdf-file lang-tag-char-p read-lang-tag read-type-tag))

;;
;; Description structure to S-expression form
;;

(defun make-form (x)
  "<x> may be a null list, string, number, literal, an instance of description, comment, or cons.
This function returns a S-expression of <x>. If <x> is a comment, nil is returned."
  (etypecase x
    (null nil)
    (string (list x))
    (number (list x))
    (xsd:|anySimpleType| (list x))
    (rdfs:Literal (list x))
    (rdf:Description (list (Description-form x)))
    (comment nil) ; depress comment
    (cons (mapcan #'make-form x))))

(defun prop-form (prop)
  (let ((name (prop-name prop))
        (atts (prop-att&vals prop))
        (value (prop-value prop)))
    (let ((resource (getf atts 'rdf:resource))
          (nodeID (getf atts 'rdf:nodeID))
          (datatype (getf atts 'rdf:datatype))
          (lang (getf atts 'xml:lang)))
      (cond (nodeID
             (assert (null resource) () "resource cannot be placed with nodeID.")
             (assert (null value))
             (list name (nodeID2symbol nodeID)))
            (resource
             (cond ((char= (char resource 0) #\#)
                    (list name (copy-uri
                                   (iri
                                    (render-uri (or *base-uri* *default-namespace*) nil))
                                   :fragment (subseq resource 1))))
                   (t (let ((rsc (iri resource)))
                        (cond ((uri-scheme rsc) (list name rsc))
                              (*base-uri*
                               (let ((base (render-uri *base-uri* nil)))
                                 (cond ((char= #\# (last-char base))
                                        (list name
                                              (iri (concatenate 'cl:string base resource))))
                                       ((char= #\/ (last-char base))
                                        (list name
                                              (iri (concatenate 'cl:string base resource))))
                                       (t (warn "~S must be \"#~A\". Check it." resource resource)
                                          (list name rsc)))))
                              (*default-namespace*
                               (let ((base (render-uri *base-uri* nil)))
                                 (cond ((char= #\# (last-char base))
                                        (list name
                                              (iri (concatenate 'cl:string base resource))))
                                       ((char= #\/ (last-char base))
                                        (list name
                                              (iri (concatenate 'cl:string base resource))))
                                       (t (list name rsc)))))
                              (t (error "Not Yet!")))))))
            (datatype
             (setq datatype (uri2symbol (iri datatype)))
             (assert (null (cdr value)))
             (list name (read-as-datatype (car value) datatype)))
            (lang
             (cons name (mapcar #'(lambda (val) (cons '@ (list val (intern lang "keyword"))))
                          (make-form value))))
            (t (cons name (make-form value)))))))

(defun Description-form (description)
  "generates S-exression of <description>."
  (%Description-form (Description-tag description)
                     (Description-att&vals description)
                     (Description-elements description)))

(defun %Description-form (class attrs props)
  "generates S-expression from <class>, <attrs>, and <props>.
   <class> is a QName symbol that indicates type tag in RDF/XML.
   <attrs> are a attribute/value list for attributes in RDF/XML.
   <props> are a property/value list for properties in RDF/XML."
  (let ((about (getf attrs 'rdf:about))
        (id (getf attrs 'rdf:ID))
        (nodeID (getf attrs 'rdf:nodeID))
        (lang (getf attrs 'xml:lang))
        (slots (loop for prop in props when (prop-p prop) collect (prop-form prop))))
    (when about
      (setq about (cond (*base-uri*
                         (merge-uris (parse-uri about) 
                                             (parse-uri
                                              (render-uri *base-uri* nil))))
                        (*default-namespace* (unless (typep *default-namespace* 'uri-namedspace)
                                               (set-uri-namedspace
                                                (merge-uris (parse-uri about)
                                                                    (parse-uri
                                                                     #+:mswindows
                                                                     (let ((path (pathname *default-namespace*)))
                                                                       (substitute
                                                                        #\/ #\\
                                                                        (namestring
                                                                         (make-pathname :directory (pathname-directory path)
                                                                                        :name (pathname-name path)
                                                                                        :type (pathname-type path)))))
                                                                     #-:mswindows
                                                                     (namestring *default-namespace*))
                                                                    ))))
                        (t (parse-uri about))))
      (remf attrs 'rdf:about))
    ;(format t "~%about:~S" about)
    (when id
      (setq id (copy-uri (parse-uri
                                  (render-uri
                                   (or *base-uri* *default-namespace*) nil))
                                 :fragment id))
      (remf attrs 'rdf:ID))
    (when nodeID
      (setq nodeID (nodeID2symbol nodeID))
      (remf attrs 'rdf:nodeID))
    (when lang
      (when (stringp lang) (setq lang (intern lang "keyword")))
      (remf attrs 'xml:lang))
    (setq attrs (loop for (prop val) on attrs by #'cddr
                    collect (cond ((and (boundp prop) (cl:typep (symbol-value prop) 'rdf:Property))
                                   (let ((range (get-range (symbol-value prop))))
                                     (cond ((null range) (list prop val))
                                           ((and (symbolp range)
                                                 (boundp range)
                                                 (cl:typep (symbol-value range) 'rdfs:Datatype))
                                            (list prop (read-as-datatype val range)))
                                           (t (list prop val)))))
                                  (t (list prop val)))))
    (when lang 
      (setq attrs (cons `(xml:lang ,lang) attrs)))
    ;(format t "~%attrs:~S" attrs)
    ;(when (and (stringp about) (zerop (length about)))
    ;  (setq about *base-uri*))
    (cons class (cond (about (cons `(rdf:about ,about) (append attrs slots)))
                      (id (cons `(rdf:ID ,(uri2symbol id)) (append attrs slots)))
                      (t (append attrs slots))))))

;;;; Producer-Consumer Model
;;; It is much better to incrementaly process data in each small fragment from input stream, without 
;;; waiting until whole contents of file are read. In this case, a fragment from input stream is 
;;; parsed by a parser and the result is handed to an interpreter that processes parsed data. If two 
;;; processes are concurrently cooperate through pipe-like data connection, we call such computation 
;;; model producer-consumer model. In Unix, it will be implemented with two processes and pipe. 
;;; In Scheme, it will be implemented as coroutine with continuation. Unfortunately, Common Lisp is
;;; not equiped such a native model.Then, following <make-rdfxml-reader> and <make-accepter-for-rdf> 
;;; create a producer and a consumer dedicated to parsing and interpreting RDF file.
;;;
;;; 

(defun make-rdfxml-reader (stream)
  "This function creates a creater of a producer in producer-consumer model.
   The returned function must take a consumer."
  #'(lambda (writer)
      (labels ((read-loop (writer)
                 (skipbl stream)
                 (cond ((RDFdeclEndp? stream)     ; eat up "</RDF>"
                        (throw 'RDF-completed t)) ; stop reading
                       ((RDFdecl? stream)         ; eat up "<RDF"
                        (skipbl stream)
                        (let ((attributes
                               (loop until (match-pattern-p ">" stream)
                                   append (read-Attribute-in-RDF stream)
                                   do (skipbl stream))))
                          (skip-pattern ">" stream)
                          (funcall writer
                                   attributes
                                   #'(lambda (wrtr) (read-loop wrtr)))))
                       ((match-pattern-p "<?xml " stream)
                        (skip-pattern "<?xml " stream)
                        (funcall writer
                                 (parse-XMLDecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((match-pattern-p "<!DOCTYPE" stream)
                        (skip-pattern "<!DOCTYPE" stream)
                        (funcall writer
                                 (parse-doctypedecl stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       ((Comment? stream)
                        (funcall writer
                                 (parse-Comment stream)
                                 #'(lambda (wrtr) (read-loop wrtr))))
                       (t ; Description
                        (funcall writer
                                 (parse-Description stream)
                                 #'(lambda (wrtr) (read-loop wrtr)))))))
        (read-loop writer))))

;;;
;;;; Auto-epistemic Local Closed World Assumption
;;;
;;; Offically, Semantic Webs are regarded as an open world. Namely, it is regarded that you cannot know all 
;;; knowledge about the world. The limited capability of agent cannot exhaust almost infinite WWW world.  
;;; It is called Open World Assumption (OWA) in Sematic Webs. However, this assumption produces less results 
;;; in reasoning, especially with respect to the existential value restriction of property or owl:someValuesFrom. 
;;; Rigorous OWA does not infer anything on existential restrictions, even if you add a slot value that does 
;;; not satisfy an existenitially restriction into the slot in OWL, because a satisfiable value may be defined at 
;;; another place in WWW, where you do not know. 
;;;
;;; Such rigorous OWA is not enjoyable for ontologies that you and your colleague are building. So, we 
;;; have introduced the notion of auto-epistemic Local Closed World Assumption. In this idea, an agent 
;;; and you can introspectively check their knowledge within their boundaries and be able to intrinsically 
;;: check if you know something or not. So, when an agent want to conclude something with respect to the 
;;; world the agent knows, the agent stands at locally closed world around it. This is called 
;;; auto-epistemic Local Closed World Assumption. The global variable <*autoepistemic-local-closed-world*> 
;;; is set true as default in SWCLOS, and the satisfiability for existential restriction for slot value 
;;; is aggressively checked. Namely, if an existential restriction is not satisfied, 
;;; the interpretation is not satisfied. If you want to completely regard Open World Assumption, 
;;; set <*autoepistemic-local-closed-world*> false.
;;;
;;; In the Tableau Method of Description Logics, an instance that satisfies the existential restriction 
;;; is virtually created and the satisfiability as a whole ontology is checked. We would like to claim 
;;; it is basically the same as setting auto-epistemic Local Closed World Assumption. 
;;;

(defvar *autoepistemic-local-closed-world* t
  "default value is true, namely the world is locally closed within knowledge of agent.")

(defun make-accepter-for-rdf (accepter-fun)
  "This function creates a creater of a consumer function in producer-consumer model.
   The returned function must take a producer."
  (let ((*autoepistemic-local-closed-world* nil))
    #'(lambda (reader)
        (labels ((accept-loop (element reader)
                              ;(format t "~%Reading ... ~S" element)
                              (cond ((null element)     ; initial calling and null attributes
                                     (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                                    ((consp element)    ; attributes in top envelop
                                     (let ((ontouri (loop for (key val) on element  by #'cddr
                                                        when (string= key "owlx:name")
                                                        return val)))
                                       (when ontouri (name-ontology ontouri))))
                                    ((XMLDecl-p element)
                                     nil)
                                    ((doctypedecl-p element)
                                     nil)
                                    ((comment-p element)
                                     nil)
                                    ((Description-p element)
                                     (let ((*base-uri* *base-uri*)
                                           (*default-namespace* *default-namespace*))
                                       (funcall accepter-fun element)))
                                    ((error "Can't happen")))))
          (accept-loop nil reader)))))

(defvar *defined-resources* nil "accumulating list for defined resource with the line where the entity appeared.")
(defvar *referenced-resources* nil "accumulating list for referenced resource with the line where the entity appeared.")

(eval-when (:execute :load-toplevel)
  (setq *defined-resources*
        (mapcar #'(lambda (x) `(,x line nil))
          '(rdfs:Resource rdfs:Literal rdf:Property rdfs:label 
            rdfs:comment rdfs:isDefinedBy rdfs:domain rdfs:range rdfs:subClassOf 
            rdfs:subPropertyOf rdfs:seeAlso rdfs:isDefinedBy rdfs:Class rdf:type rdfs:Container 
            rdf:predicate rdf:subject rdf:object rdf:Statement rdfs:Datatype rdf:XMLLiteral 
            rdf:List rdf:nil rdf:first rdf:rest rdf:value xsd:|anySimpleType| xsd:|boolean| 
            xsd:|anyURI| xsd:|string| xsd:|float| xsd:|double| xsd:|unsignedByte| xsd:|unsignedShort|  
            xsd:|unsignedInt| xsd:|unsignedLong| xsd:|decimal| xsd:|integer| xsd:|long| xsd:|int| 
            xsd:|short| xsd:|byte| xsd:|nonNegativeInteger| xsd:|positiveInteger| 
            xsd:|nonPositiveInteger| xsd:|negativeInteger|
            xsd:|duration|
            owl:DataRange owl:DeprecatedProperty owl:DeprecatedClass owl:incompatibleWith owl:backwardCompatibleWith
                         owl:priorVersion owl:versionInfo owl:imports owl:OntologyProperty owl:Ontology 
                         owl:AnnotationProperty owl:InverseFunctionalProperty owl:FunctionalProperty owl:SymmetricProperty 
                         owl:TransitiveProperty owl:inverseOf owl:DatatypeProperty owl:ObjectProperty 
                         owl:cardinality owl:maxCardinality owl:minCardinality owl:someValuesFrom owl:hasValue 
                         owl:allValuesFrom owl:onProperty )))
  )

(defun read-rdf-file (accepter-fun &optional (file (ask-user-rdf-file)) (code :default))
  "reads an rdf <file> and hands parsed data to <accepter-fun>. If XMLDecl in <file> 
   includes any code statement, it is set as character-code of this <file>. If no statement
   for character-code in <file> and <code> supplied, then this <file> is parsed in <code>.
   In parsing, QNames of referenced resources and defined resources are stored into 
   <*referenced-resources*> and <*defined-resources*>. At the end this procedure, 
   the set difference of <*referenced-resources*> and <*defined-resources*> is printed."
  (unless file (return-from read-rdf-file nil))
  (setq code (or (peep-XMLDecl-code-from-file file) code))
  ;(format t "~%Encoding:~S" code)
  (with-open-file (stream (pathname file) :external-format (excl:find-external-format code))
    (let ((*line-number* 1)
          (*line-pos* 0)
          (*pos* 0)
          (*default-namespace* nil)
          (*base-uri* *base-uri*)
          (reader (make-rdfxml-reader stream))
          (accept (make-accepter-for-rdf accepter-fun)))
      (flush-buf)
      (catch 'RDF-completed
             (loop (funcall accept reader)))))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  :done)

(defun read-rdf-from-string (accepter-fun rdf-string)  ; no code optional parameter for string-stream
  (unless rdf-string (return-from read-rdf-from-string nil))
  (when (equal rdf-string "") (return-from read-rdf-from-string nil))
  (unless (stringp rdf-string) (return-from read-rdf-from-string nil))
  (let ((code (peep-XMLDecl-code-from-string rdf-string)))
    (when code (warn "external code is directed in RDF/XML string, but it has no effect for string input."))
    (with-input-from-string (stream rdf-string)
      (let ((*line-number* 1)
            (*base-uri* *base-uri*)
            (*default-namespace* nil)
            (reader (make-rdfxml-reader stream))
            (accept (make-accepter-for-rdf accepter-fun)))
        (flush-buf)
        (catch 'RDF-completed
               (loop (funcall accept reader)))))
    (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
      ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
      ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
      (when refered
        (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
    'done))

(defun ask-user-rdf-file ()
  "asks an rdf file to user."
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("RDF/XML format file" . "*.rdf")
                       ("OWL/XML format file" . "*.owl")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%RDF/XML format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))
;;;
;;;; Reader Macro '<' for URI, and '_' for nodeID
;;;
;;; This mechanism is similar to lisp symbol evaluation, but note that the form is not any 
;;; symbol. So, the evaluation for such a form cannot be suppressed, even if you quoted it.

(defparameter cl::*standard-readtable* (copy-readtable nil))
(defparameter rdf::*standard-readtable* (copy-readtable))

(defun rdf::read-string (stream closech)
  (let ((str (excl::read-string stream closech)))
    (let ((nxtch (peek-char nil stream nil nil t)))
      (cond ((char= nxtch #\@)
             (read-char stream)
             (cond ((lang-tag-char-p (setq nxtch (peek-char nil stream nil nil t)))
                    ;; read lang tag
                    (list '@ str (read-lang-tag stream nxtch)))
                   (t (unread-char #\@ stream)
                      str)))
            ((char= nxtch #\^)
             (read-char stream nil nil t)
             (cond ((char= #\^ (setq nxtch (peek-char nil stream nil nil t)))
                    (read-char stream nil nil t)
                    (list '^^ str (read-type-tag stream (read-char stream nil nil t))))
                   (t (unread-char #\^ stream)
                      str)))
            (t str)))))

(defun read-lang-tag (stream firstchar)
  (let ((lang-tag nil)
        (sub-tag nil)
        (char firstchar))
    (setq lang-tag
          (loop while (lang-tag-char-p char)
              collect char
              do (read-char stream nil nil t)
                (setq char (peek-char nil stream nil nil t))))
    (when (char= #\- char)
      (read-char stream nil nil t)
      (setq char (peek-char nil stream nil nil t))
      (setq sub-tag
            (loop while (sub-tag-char-p char)
                collect char
                do (read-char stream nil nil t)
                  (setq char (peek-char nil stream nil nil t)))))
    (cond (sub-tag
           (concatenate 'cl:string (append lang-tag (list #\-) sub-tag)))
          (t (concatenate 'cl:string lang-tag)))))

(defun lang-tag-char-p (char)
  (and char (alpha-char-p char) ;(lower-case-p char)
       ))
(defun sub-tag-char-p (char)
  (and char (or (and (alpha-char-p char) ;(lower-case-p char)
                     )
                (digit-char-p char))))

(defun read-type-tag (stream firstchar)
  (let ((char firstchar))
    (cond ((char= #\< char)
           (second (double-angle-bracket-reader stream char)))
          (t (excl::read-token stream char)))))

(defun single-underscore-reader (stream char)
  (let ((nc (peek-char nil stream t nil t)))
    (cond ((char= nc #\:)
           (read-char stream t nil t)
           (when (char= (peek-char nil stream t nil t) #\:)
             (read-char stream t nil t)
             (when (char= (peek-char nil stream t nil t) #\:)
               (error "Too more semicolons for nodeID ~S" stream)))
           (let ((id-str
                  (coerce 
                   (loop with char while (NCNameChar-p (setq char (read-char stream t nil t))) collect char)
                   'cl:string)))
             (unread-char char stream)
             (cond ((and (find-symbol id-str :_) (boundp (find-symbol id-str :_)))
                    (find-symbol id-str :_))
                   ((find-symbol id-str :_)
                    (export (find-symbol id-str :_) :_)
                    (setf (symbol-value (find-symbol id-str :_)) (make-instance '|rdfs:Resource|))
                    (find-symbol id-str :_))
                   (t ;; no definition for nodeID, create an anonymous object but bind it to id symbol
                    (export (intern id-str :_) :_)
                    (setf (symbol-value (find-symbol id-str :_)) (make-instance '|rdfs:Resource|))
                    (find-symbol id-str :_)))))
          (t (excl::read-token stream char)
             ))))

(set-macro-character #\< #'double-angle-bracket-reader t rdf::*standard-readtable*)
(set-macro-character #\_ #'single-underscore-reader    t rdf::*standard-readtable*)
(set-macro-character #\" #'rdf::read-string            nil rdf::*standard-readtable*)

#|
(in-package :gx)
:cd C:\allegro-projects\SWCLOS2\RDFS\
(with-open-file (p "Intro.rdf") (parse-rdf p))
->
(<?xml version="1.0" ?> 
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:gxpr="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#"
         xmlns="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#">
  <rdf:Property rdf:ID="likes"/>
  <rdf:Description rdf:ID="Kim">
    <gxpr:likes rdf:resource="#Robin" />
  </rdf:Description>
  <rdf:Description rdf:ID="Sandy">
    <gxpr:likes rdf:resource="#Lee" />
    <gxpr:likes rdf:resource="#Kim" />
  </rdf:Description>
  <rdf:Description rdf:ID="Robin">
    <gxpr:likes rdf:resource="#cats" />
  </rdf:Description>
</rdf:RDF>)

:cd C:\allegro-projects\SWCLOS2\RDFS\IntroExample
(with-open-file (p "Example.rdf") (parse-rdf p))
|#

;; End of module
;; --------------------------------------------------------------------

(cl:provide :rdfreader)
