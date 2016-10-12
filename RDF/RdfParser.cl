;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf Parser module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code was encoded by Seiji Koide at Galaxy Express Corporation, Japan.
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002, 2004 
;;;    Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007--2011
;;;    Seiji Koide
;;;
;; History
;; -------
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2007.12.18    RdfShare is separated from here in order to share routines with RDFGate program
;; 2007.08.18    Rdf I/O module is separated.
;; 2005.12.09    OntologySpace and Ontology definition is added.
;; 2004.07.07    Rdf2Sexpr is separated from here.
;; 2004.07.07    Rdf2Nt is separated from here.
;; 2004.01.09    parse-rdf for RDF parser
;; 2002.08.15    File created
;;; ==================================================================================

(cl:provide :rdfparser)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  (require :rdfshare)
  ) ; end of eval-when

(in-package :gx)

(export '(*NameSpaces* *default-namespace* *base-uri* set-uri-namedspace
           name uri2symbol line Description-p Description-tag Description-att&vals Description-elements
           parse-rdf lang content
           parse-XMLDecl read-AttValue read-plane-text
           read-as-datatype ^^))

(define-condition rdf-parse-error (error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "invalid RDF parse: ~A: ~S at line ~S"
         (apply #'format nil fcont args) (expose-buf) (line-count stream))))))

(define-condition no-Eq-error (rdf-parse-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((fcont (simple-condition-format-control condition))
           (args (simple-condition-format-arguments condition)))
       (format stream "No Eq in RDF parsing: ~A: ~S at line ~S"
         (apply #'format nil fcont args) (expose-buf) (line-count stream))))))

;;;
;;;; XML Structures in RDF and Parser
;;;
;;; RDF/XML file contents can be read and transformed into Lisp structures of XMLDecl, doctypedecl, 
;;; Comment, RDF, rdf:Description, and prop. For example, in the followings, <parse-rdf> returns 
;;; a list that includes a structure of XMLDecl and RDF. Those structure printing functions 
;;; print structures like XML serialized.
;;; ----------------------------------------------------------------------------------
;;; :cd C:\allegro-projects\SWCLOS\RDFS\           -> C:\allegro-projects\SWCLOS\RDFS\
;;; (with-open-file (p "Intro.rdf") (parse-rdf p)) ->
;;; (<?xml version="1.0" ?> 
;;; <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
;;;          xmlns:gxpr="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#"
;;;          xmlns="http://galaxy-express.co.jp/MEXT/RDF/0.1/Prolog#">
;;;   <rdf:Property rdf:ID="likes"/>
;;;   <rdf:Description rdf:ID="Kim">
;;;     <gxpr:likes rdf:resource="#Robin" />
;;;   </rdf:Description>
;;;   <rdf:Description rdf:ID="Sandy">
;;;      <gxpr:likes rdf:resource="#Lee" />
;;;      <gxpr:likes rdf:resource="#Kim" />
;;;   </rdf:Description>
;;;   <rdf:Description rdf:ID="Robin">
;;;     <gxpr:likes rdf:resource="#cats" />
;;;   </rdf:Description>
;;; </rdf:RDF>)
;;; ----------------------------------------------------------------------------------
;;;
;;;; XMLDecl
;;; XMLDecl is composed of version, encoding, and stadalone slots.

(defstruct (XMLDecl (:print-function print-XMLDecl)) version encoding standalone)
(defun print-XMLDecl (r s k)
  "prints out XMLDecl string that contains version of <r>, encoding of <r>, 
   and standalone of <r> to stream <s>.  This function is not intended to be used by user."
  (declare (ignore k))
  (format s "<?xml~@[ version=~W~]~@[ encoding=~W~]~@[ standalone=~W~] ?>" 
    (XMLDecl-version r) (XMLDecl-encoding r) (XMLDecl-standalone r)))

;;;
;;;; Doctypedecl
;;; doctypedecl is a structure that has slot <name>, <external>, and <values>.

(defstruct (doctypedecl (:print-function print-doctypedecl)) name external values)
(defun print-doctypedecl (r s k)
  "a doctypedecl is printed in abreviated form '#<doctypedecl ... >'. 
   This function is not intended to be used by user."
  (declare (ignore r k))
  ;(format s "~<<!DOCTYPE ~A [~4I~:@_~{~W~^~:@_~}~2I~:@_] >~:>"
  ;  (list (doctypedecl-name r) (doctypedecl-values r)))
  (format s "#<doctypedecl ... >")
  )

;;;
;;;; RDF Structures
;;; An RDF structure has slots <att&vals> and <body>.

(defstruct (RDF (:print-function print-RDF)) att&vals body)
(defun print-RDF (r s k)
  "prints out an RDF structure <r>. This function is not intended to be used by user."
  (declare (ignore k))
  (let ((*print-level* nil)
        (*print-length* nil))
    (if (RDF-body r)
        (if (RDF-att&vals r)
            (format s "~&~<<rdf:RDF ~:I~<~@{~A=~W~^ ~_~}~:>>~2I~{~_~W~}~I~_</rdf:RDF>~:>" 
              (list (RDF-att&vals r) (RDF-body r)))
          (format s "~&~<<rdf:RDF> ~:I~{~W~^ ~_~}</rdf:RDF>~:>" (list (RDF-body r))))
      (if (RDF-att&vals r)
          (format s "~&~<<rdf:RDF ~:I~{~A=~W~^ ~_~} />~:>" (list (RDF-att&vals r)))
        (format s "<rdf:RDF />")))))

;;;
;;;; Description
;;; An RDF structure includes one description that contains nested elements.

(defstruct (rdf:Description (:print-function print-Description)) tag att&vals elements)
(defun print-Description (r s k)
  "prints out a Description <r>. This function is not intended to be used by user."
  (declare (ignore k))
  (if (Description-elements r)
      (if (Description-att&vals r)
          (format s "~<<~W ~:I~<~@{~W=~W~^ ~_~}~:>>~2I~{~:@_~W~}~I~_</~W>~:>" 
            (list (Description-tag r) (Description-att&vals r) (Description-elements r) (Description-tag r)))
        (format s "~<<~W>~2I~{~:@_~W~}~I~_</~W>~:>" 
          (list (Description-tag r) (Description-elements r) (Description-tag r))))
    (if (Description-att&vals r)
        (format s "~<<~W ~:I~<~@{~W=~W~^ ~:_~}~:>~I~:_/>~:>"
          (list (Description-tag r) (Description-att&vals r)))
      (format s "<~S />" (Description-tag r)))))

;;;
;;;; Property
;;; A structure property has <name>, <att&val>, and <value>.

(defstruct (prop (:print-function print-prop)) name att&vals value)
(defun print-prop (p s k)
  "prints out a property <p>. This function is not intended to be used by user."
  (declare (ignore k))
  (if (prop-value p)
      (if (prop-att&vals p)
          (cond ((and (consp (prop-value p)) (null (cdr (prop-value p))) (stringp (car (prop-value p))))
                 (format s "~<<~W ~:I~{~W=~W~^ ~_~}>~2I~A</~W>~:>" 
                   (list (prop-name p) (prop-att&vals p) (car (prop-value p)) (prop-name p))))
                ((and (consp (prop-value p)) (null (cdr (prop-value p))) (Description-p (car (prop-value p))))
                 (format s "~<<~W ~:I~{~W=~W~^ ~_~}>~2I~_~W~I~_</~W>~:>" 
                   (list (prop-name p) (prop-att&vals p) (car (prop-value p)) (prop-name p))))
                ((and (consp (prop-value p)) (cdr (prop-value p)))
                 (format s "~<<~W ~:I~{~W=~W~^ ~_~}>~2I~{~:@_~W~}~I~_</~W>~:>" 
                   (list (prop-name p) (prop-att&vals p) (prop-value p) (prop-name p))))
                (t (format s "~<<~W ~:I~{~W=~W~^ ~_~}>~2I~{~_~W~}~I~_</~W>~:>" 
                     (list (prop-name p) (prop-att&vals p) (prop-value p) (prop-name p)))))
        (cond ((and (consp (prop-value p)) (null (cdr (prop-value p))) (stringp (car (prop-value p))))
               (format s "~<<~W>~A</~W>~:>" 
                 (list (prop-name p) (car (prop-value p)) (prop-name p))))
              ((and (consp (prop-value p)) (null (cdr (prop-value p))) (Description-p (car (prop-value p))))
               (format s "~<<~W>~2I~_~W~I~_</~W>~:>" 
                 (list (prop-name p) (car (prop-value p)) (prop-name p))))
              ((and (consp (prop-value p)) (cdr (prop-value p)))
               (format s "~<<~W>~2I~{~:@_~W~}~I~_</~W>~:>" 
                 (list (prop-name p) (prop-value p) (prop-name p))))
              (t (format s "~<<~W>~2I~_~{~W~}~I~_</~W>~:>" 
                   (list (prop-name p) (prop-value p) (prop-name p))))))
    (if (prop-att&vals p)
        (format s "~<<~W~{ ~W=~W~} />~:>" (list (prop-name p) (prop-att&vals p)))
      (format s "<~S />" (prop-name p)))))

;;;
;;;; Parser
;;;

(defun parse-XMLDecl (stream)
  "reads XMLDecl and returns XMLDecl structure. This function should be called 
  just after '&lt;?xml '"
  (multiple-value-bind (version encoding standalone) (read-XMLDecl stream)
    (make-XMLDecl :version version :encoding encoding :standalone standalone)))

(defun parse-doctypedecl (stream)
  "reads doctypedecl from <stream> and returns it as structure."
  (multiple-value-bind (name external values) (read-doctypedecl stream)
    (make-doctypedecl :name name :external external :values values)))

(defun parse-RDFdecl (stream)
  "reads RDF/XML from <stream> and returns RDF structure."
  (skipbl stream)
  (let ((attributes (loop until (or (match-pattern-p ">" stream)
                                    (match-pattern-p "/>" stream))
                        append (read-Attribute-in-RDF stream)
                        do (skipbl stream))))
    (cond ((match-pattern-p ">" stream)
           (read-pattern-p ">" stream)
           (skipbl stream)
           (let ((body (loop until (or (match-pattern-p "</RDF>" stream)
                                       (match-pattern-p "</rdf:RDF>" stream))
                           collect (cond ((Comment? stream) (parse-Comment stream))
                                         (t (parse-Description stream)))
                           do (skipbl stream))))
             (make-RDF :att&vals attributes :body body)))
          ((match-pattern-p "/>" stream)
           (make-RDF :att&vals attributes :body nil)))))

(defun parse-Description (stream &optional (depth 0))
  "reads and parses a description from <stream> and returns a text or a structure Description."
  (cond ((CDStart-p stream)
         (read-CData-to-CDEnd stream))
        ((match-pattern-p "<" stream)
         (multiple-value-bind (tag attributes contents) (read-Description stream depth)
           (make-Description :tag tag :att&vals attributes :elements contents)))
        (t (read-plane-text stream))))

(defun parse-property (stream depth)
  "reads from <stream> and parse a property element, and returns a property structure."
  (cond ((not (match-pattern-p "<" stream))
         (coerce 
          (loop for c = (getnext-char stream)
              while (not (char= c #\<))
              collect c
              finally (putback-char c stream))
          'cl:string))
        (t (multiple-value-bind (tag attributes contents) (read-property stream depth)
             ;(format t "~%Property:tag=~S atts=~S con=~S" tag attributes contents)
             ;(unless (lower-case-p (char (string tag) 0))
             ;  (warn "Tag ~S starts with a upper-case letter. It might be wrong as property." tag))
             (make-prop :name tag :att&vals attributes :value contents)))))

;;;
;;;; Reader
;;;

(defun read-Description (stream &optional (depth 0))
  "reads a description from <stream> and returns tag, attributes, and contents.
   Note that contents are a list of instances of property structure."
  (multiple-value-bind (tag attributes) (read-STag-or-EmptyElemTag-in-Description stream)
    #|(let ((about (getf attributes 'rdf:about))
          (id (getf attributes 'rdf:ID)))
      (when (and (find-package :db.allegrocache)
                 (gx::rdf-db-open-p))
        (rdf-db-register (or about id) *line-pos* depth)))|#
    (when (lower-case-p (char (string (QNameString2symbol tag)) 0))
      (warn "Tag ~S might be wrong as Description." tag)
      )
    (cond ((match-pattern-p ">" stream)
           (read-pattern-p ">" stream)
           (skipbl stream)  ; this line might be deleted to be exact in case of leaf nodes, but need at intermediate node
           (let ((contents (loop until (ETag-p-with-eat-up tag stream)
                               collect (cond ((Comment? stream) (parse-Comment stream))
                                             (t (parse-property stream depth)))
                               do (skipbl stream))))
             (values (QNameString2symbol tag) attributes contents)))
          ((match-pattern-p "/>" stream)
           (read-pattern-p "/>" stream)
           (values (QNameString2symbol tag) attributes nil))
          ((error "Cant happen!")))))

(defun read-property (stream depth)
  "reads a property from <stream> and returns tag, attributes, and its contents."
  (multiple-value-bind (tag attributes) (read-STag-or-EmptyElemTag-in-property stream)
    (cond ((match-pattern-p ">" stream)
           (read-pattern-p ">" stream)
           (skipbl stream)
           (let ((parse nil))
             (cond ((setq parse (getf attributes 'rdf:parseType))
                    (cond ((string= parse "Resource")
                           (remf attributes 'rdf:parseType)
                           (let ((contents (loop until (ETag-p-with-eat-up tag stream)
                                               collect (cond ((Comment? stream) (parse-Comment stream))
                                                             (t (parse-property stream depth)))
                                               do (skipbl stream))))
                             (values (QNameString2symbol tag) attributes
                                     (list (make-Description :tag 'rdf:Description :elements contents)))))
                          ((string= parse "Literal")
                           (remf attributes 'rdf:parseType)
                           (let ((contents (^^ (read-string-until-Etag-with-eat-up tag stream)
                                               (symbol-value 'rdf:XMLLiteral))))
                             (values (QNameString2symbol tag) attributes contents)))
                          ((string= parse "Collection")
                           (remf attributes 'rdf:parseType)
                           (let ((contents (loop until (ETag-p-with-eat-up tag stream)
                                               collect (cond ((Comment? stream) (parse-Comment stream))
                                                             (t (parse-Description stream (1+ depth))))
                                               do (skipbl stream))))
                             (values (QNameString2symbol tag) attributes contents)))
                          ((error "Not Yet parseType ~A" (getf attributes 'rdf:parseType)))))
                   (t (let ((contents (loop until (ETag-p-with-eat-up tag stream)
                                          collect (cond ((Comment? stream) (parse-Comment stream))
                                                        (t (parse-Description stream (1+ depth))))
                                          do (skipbl stream))))
                        (values (QNameString2symbol tag) attributes contents))))))
          ((match-pattern-p "/>" stream)
           (read-pattern-p "/>" stream)
           (values (QNameString2symbol tag) attributes nil))
          ((error "Cant happen!")))))

(defun read-Attribute-in-RDF (stream)
  "returns a list of Name symbol and attribute value."
  (let (Name NCName AttValue)
    (cond ((match-pattern-p "xmlns:" stream) ; PrefixedAttName
           (read-pattern-p "xmlns:" stream)
           (setq NCName (read-NCName stream))
           (read-Eq stream
                    :format-control " after '~A'" 
                    :format-arguments `(,NCName))
           (setq AttValue (read-AttValue stream))
           (let* ((ns (set-uri-namedspace AttValue))
                  (pkg (uri-namedspace-package ns)))
             (cond ((and pkg (string= NCName (package-name pkg)))) ; nothing done
                   ((and pkg (member NCName (package-nicknames pkg) :test #'string=))) ; nothing done
                   (pkg ; multiple abreviations for same uri-namedspace
                    (eval `(defpackage ,(package-name pkg)
                             (:nicknames ,@(cons NCName (package-nicknames pkg))))))
                   (t (setq pkg (or (find-package NCName) (make-package NCName :use nil)))  ; by smh
                      (unless (documentation pkg t) (setf (documentation pkg t) AttValue))
                      (setf (uri-namedspace-package ns) pkg))))
           (list (concatenate 'cl:string "xmlns:" NCName) AttValue))
          ((match-pattern-p "xmlns" stream) ; DefaultAttName
           (read-pattern-p "xmlns" stream)
           (read-Eq stream :format-control " after 'xmlns'")
           (setq AttValue (read-AttValue stream))
           (setq *default-namespace* (set-uri-namedspace AttValue))
           (list "xmlns" AttValue))
          ((match-pattern-p "xml:base" stream)
           (read-pattern-p "xml:base" stream)
           (read-Eq stream :format-control " after 'xml:base'")
           (setq AttValue (read-AttValue stream))
           (setf *base-uri* (parse-iri AttValue))
           (list "xml:base" AttValue))
          (t ;; QName
           (setq Name (read-QName stream))
           (read-Eq stream
                    :format-control " after '~A'" 
                    :format-arguments `(,Name))
           (list Name (read-AttValue stream))))))

;;; ----------------------------------------------------------------------------------
;;; [66]    CharRef    ::=    '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';' 
;;; ----------------------------------------------------------------------------------
(defun read-CharRef (stream)
  "reads CharRef from <stream> and returns a translated character"
  (cond ((char= (peeknext-char stream) #\x)
         (getnext-char stream)
         (code-char
          (parse-integer
           (coerce 
            (loop for c = (getnext-char stream)
                until (char= c #\;)
                do (assert (or (<= #x0030 (char-code c) #x0039)
                               (<= #x0041 (char-code c) #x0046)
                               (<= #x0061 (char-code c) #x0076))
                           () "Illegal CharRef character ~W at line ~S"
                           c (line-count stream))
                collect c)
            'cl:string)
           :radix 16)))
        (t (code-char
            (parse-integer
             (coerce 
              (loop for c = (getnext-char stream)
                  until (char= c #\;)
                  do (assert (<= #x0030 (char-code c) #x0039)
                             () "Illegal CharRef character ~W at line ~S"
                             c (line-count stream))
                  collect c)
              'cl:string))))))

;;; ----------------------------------------------------------------------------------
;;; [68]    EntityRef    ::=    '&' Name ';' 
;;; ----------------------------------------------------------------------------------
(defun read-EntityRef (stream)
  "reads EntityRef and returns a translated string."
  (assert (char= (getnext-char stream) #\&))
  (let ((name (read-Name stream)))
    (assert (char= (getnext-char stream) #\;) ()
            "missing semicolon at the end of EntityRef ~A in line ~S"
            name (line-count stream))
    (let ((traslated (cdr (assoc name *entity-decls* :test #'string=))))
      (assert traslated () "No Entity Referencing: ~A" name)
      traslated)))

;;; ----------------------------------------------------------------------------------
;;; [69]    PEReference    ::=    '%' Name ';' 
;;; ----------------------------------------------------------------------------------
(defun read-PEReference (stream)
  "Not implemented yet. reads PEReference and returns a translated string."
  (declare (ignore stream))
  (error "Not Yet"))

;;; ----------------------------------------------------------------------------------
;;; [67]    Reference    ::=    EntityRef | CharRef 
;;; ----------------------------------------------------------------------------------
(defun read-Reference (stream)
  "reads EntityRef or CharRef from <stream>."
  (cond ((char= (peeknext-char stream) #\&)
         (read-EntityRef stream))
        ((char= (peeknext-char stream) #\%)
         (read-PEReference stream))
        ((error 'rdf-parse-error
           :format-control "Illegal Reference"))))

;;; ----------------------------------------------------------------------------------
;;; [10]    AttValue    ::=    '"' ([^&lt;&"] | Reference)* '"'  
;;;                         |  "'" ([^&lt;&'] | Reference)* "'" 
;;; ----------------------------------------------------------------------------------
(defun read-AttValue (stream)
  "reads AttValue from <stream> and returns the string."
  ;; read from opening '"'" to closing '"'
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (let ((chars
           (loop for c = (peeknext-char stream)
               until (char= c q)
               collect
                 (cond ((char= c #\<)
                        (error "Illegal character '<' in AttValue"))
                       ((char= c #\&)                 ; EntityRef or CharRef
                        (getnext-char stream)         ; discard &
                        (cond ((char= #\# (peeknext-char stream))    ; CharRef
                               (getnext-char stream)  ; discard #
                               (read-CharRef stream))
                              (t                      ; EntityRef
                               (let ((pat (loop for cc = (getnext-char stream)
                                              until (or (char= cc #\;) (char= cc q))
                                              collect cc)))
                                 (cond ((char= (car (last pat)) q) pat)
                                       (t (setq pat (coerce pat 'cl:string))
                                          (let ((found (cdr (assoc pat *entity-decls* :test #'string=))))
                                            (cond (found)
                                                  (t (error "There is no entity definition for ~A at line ~S." pat (line-count stream))
                                                     )))))))))
                       (t (getnext-char stream))))))
      (getnext-char stream) ; discard " or '
      (when chars
        (setq chars (mapcar #'(lambda (c) (if (stringp c) (coerce c 'list) c)) chars))
        (setq chars (squash chars)))
      (coerce chars 'cl:string))))

(defun read-URI (stream)
  "reads URI string from <stream>."
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (%read-URI stream q)))
(defun %read-URI (stream q)
  (let ((value
         (loop for c = (getnext-char stream)
             until (char= c q)
             collect (cond ((char= c #\&)
                            (let ((pat (loop for cc = (getnext-char stream)
                                           until (char= cc #\;)
                                           collect cc)))
                              (setq pat (coerce pat 'cl:string))
                              (let ((found (cdr (assoc pat *entity-decls* :test #'string=))))
                                (cond (found)
                                      (t (error "There is no entity definition for ~A at line ~S." pat (line-count stream)))))))
                           ((char= c #\<)
                            (error "Illegal AttValue with '<' at line number ~S" (line-count stream)))
                           (t c)))))
    (when value
      (setq value
            (loop for c in value
                collect (etypecase c
                          (character c)
                          (string (coerce c 'list)))))
      (setq value (squash value)))
    (coerce value 'cl:string)))
    
(defun white-space-p (decl)
  "Is this decl is all white space?."
  (declare (optimize (speed 3) (safety 1)))
  (and (stringp decl)
       (= (length (string-trim '(#\space #\Newline #\Tab) decl)) 0)))

(defun skip-pattern-delimited-string (pattern stream)  ;; this code can be more efficient
  "just skips and discards <pattern> in <stream>."
  (loop until (match-pattern-p pattern stream)
      do (getnext-char stream)))

;;;
;;;; Parsing Attribute
;;;
;;; ----------------------------------------------------------------------------------
;;; namespace[1]    NSAttName         ::=    PrefixedAttName | DefaultAttName 
;;; namespace[2]    PrefixedAttName   ::=    'xmlns:' NCName 
;;; namespace[3]    DefaultAttName    ::=    'xmlns' 
;;; namespace[15]   Attribute         ::=    NSAttName Eq AttValue | QName Eq AttValue 
;;; ----------------------------------------------------------------------------------

(defun read-Attribute-in-Description (stream)
  "returns a list of Name symbol and attribute value."
  (let (Name)
    (cond ((match-pattern-p "xmlns:" stream) ; PrefixedAttName
           (error "'xmlns:' in description at line ~S" (line-count stream)))
          ((match-pattern-p "xmlns" stream) ; DefaultAttName
           (error "'xmlns' in description at line ~S" (line-count stream)))
          ((match-pattern-p "rdf:nodeID" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:nodeID'")
           (list Name (read-quoted-NCName stream)))
          ((match-pattern-p "rdf:about" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:about'")
           (list Name (read-URI stream)))
          ((match-pattern-p "rdf:resource" stream)
           (error "RDF syntax error: rdf:resource is used in RDF description. It may be a misuse of rdf:about."))
          ;; this does not match exact syntax but someone uses it.
          ((match-pattern-p "about" stream)
           (read-QName stream)
           (setq Name 'rdf:about)
           (read-Eq stream :format-control " after 'about'")
           (list Name (read-URI stream)))
          ((match-pattern-p "rdf:ID" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:ID'")
           (list Name (read-quoted-NCName stream)))
          ;; this does not match exact syntax but someone uses it.
          ((match-pattern-p "ID" stream)
           (read-QName stream)
           (setq Name 'rdf:ID)
           (read-Eq stream :format-control " after 'ID'")
           (list Name (read-quoted-NCName stream)))
          ((match-pattern-p "rdf:datatype" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:datatype'")
           (list Name (read-URI stream)))
          (t ;; QName
           (setq Name (read-QName stream))
           (read-Eq stream
                    :format-control " after '~A'" 
                    :format-arguments `(,Name))
           (list Name (read-AttValue stream))))))
(defun read-Attribute-in-property (stream)
  "returns a list of Name symbol and attribute value."
  (let (Name)
    (cond ((match-pattern-p "xmlns:" stream) ; PrefixedAttName
           (error "'xmlns:' in description at line ~S" (line-count stream)))
          ((match-pattern-p "xmlns" stream) ; DefaultAttName
           (error "'xmlns' in description at line ~S" (line-count stream)))
          ((match-pattern-p "rdf:nodeID" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:nodeID'")
           (list Name (read-quoted-NCName stream)))
          ((match-pattern-p "rdf:about" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:about'")
           (list Name (read-URI stream)))
          ((match-pattern-p "rdf:resource" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:resource'")
           (list Name (read-URI stream)))
          ((match-pattern-p "rdf:datatype" stream)
           (setq Name (read-QName stream))
           (read-Eq stream :format-control " after 'rdf:datatype'")
           (list Name (read-URI stream)))
          (t ;; QName
           (setq Name (read-QName stream))     ; rdf:parseType and others
           (read-Eq stream
                    :format-control " after '~A'" 
                    :format-arguments `(,Name))
           (list Name (read-AttValue stream))))))

;;;
;;;; Element in XML
;;; ----------------------------------------------------------------------------------
;;; [39]    element       ::=    EmptyElemTag | STag content ETag 
;;; namespace[14]    EmptyElemTag  ::=    '<' QName (S Attribute)* S? '/>' 
;;; namespace[12]    STag          ::=    '<' QName (S Attribute)* S? '>'
;;; [43] content ::= CharData? ((element | Reference | CDSect | PI | Comment) CharData?)* 
;;; ----------------------------------------------------------------------------------
;;;
;;; Read-element in XML general is defined as above. However, a Description element (node element)
;;; and a property element in RDF must be alternately appear in XML nest structure. See, 
;;; http://www.w3.org/TR/rdf-syntax-grammar/#example1. 
;;;
;;; Therefore, two element reader are coded, dedicated to node elements and property elements repectively.
;;; Then, <read-STag-or-EmptyElemTag-in-Description> calls <read-STag-or-EmptyElemTag-in-property>, and
;;; <read-STag-or-EmptyElemTag-in-property> calls <read-STag-or-EmptyElemTag-in-Description>, if the 
;;; substructure exists.

(defun read-STag-or-EmptyElemTag-in-Description (stream)
  "returns a start tag and attributes. attributes are a property list."
  (cond ((read-pattern-p "<" stream)
         (let* ((tag (prog1 (read-QNameString stream) (skipbl stream)))
                (attributes (loop until (or (match-pattern-p ">" stream)
                                            (match-pattern-p "/>" stream))
                                append (read-Attribute-in-Description stream)
                                do (skipbl stream))))
           (values tag attributes)))
        (t (error "Illegal STag at line number ~S." (line-count stream)))))

(defun read-STag-or-EmptyElemTag-in-property (stream)
  "returns a start tag and attributes. attributes are a property list.
   Note that returned value for tag is a string."
  (cond ((read-pattern-p "<" stream)
         (let* ((tag (prog1 (read-QNameString stream) (skipbl stream)))
                (attributes (loop until (or (match-pattern-p ">" stream)
                                            (match-pattern-p "/>" stream))
                                append (read-Attribute-in-property stream)
                                do (skipbl stream))))
           (values tag attributes)))
        (t (error "Illegal STag at line number ~S." (line-count stream)))))

;;; ----------------------------------------------------------------------------------
;;; namespace[13]    ETag    ::=    '</' QName S? '>'  
;;; ----------------------------------------------------------------------------------
(defun read-string-until-Etag-with-eat-up (QName stream)
  "reads <stream> up to Etag of <QName> and returns the string read.
   <QName> must be a string."
  (let ((pattern (concatenate 'string "</" QName)))
    (prog1
        (coerce
         (loop until (match-pattern-p pattern stream)
             for c = (getnext-char stream)
             collect c)
         'cl:string)
      (assert-pattern pattern stream))))

(defun ETag-p-with-eat-up (STag stream)
  "tests a end tag for STag. STag should be a string.
   This function eats up the end tag with '</' & '>', if true.
   Otherwise it does not eat, and returns false."
  (cond ((match-pattern-p "</" stream)
         (skip-pattern "</" stream)
         (cond ((match-pattern-p STag stream)
                (skip-pattern STag stream)
                (skipbl stream)
                (assert (eq #\> (getnext-char stream)))
                t)
               ((error "Illegal End Tag for ~A" STag))))))

(defun read-plane-text (stream)
  (let ((contents
         (loop for c = (getnext-char stream)
             while (not (char= c #\<))
             collect (cond ((char= c #\&)
                            (let ((pat (loop for cc = (getnext-char stream)
                                           until (char= cc #\;)
                                           collect cc)))
                              (setq pat (coerce pat 'cl:string))
                              (let ((found (cdr (assoc pat *entity-decls* :test #'string=))))
                                (cond (found found)
                                      (t (warn "There is no entity definition for ~A." pat))))))
                           (t c))
             finally (putback-char c stream))))
    (setq contents
          (loop for c in contents
              collect (etypecase c
                        (character c)
                        (string (coerce c 'list)))))
    (setq contents (squash contents))
    (string-trim '(#\Space #\Tab #\Newline) (coerce contents 'cl:string))))

(defun read-as-datatype (value datatype)
  "<value> is a string. <datatype> is a symbol that indicates any of xsd:Datatypes. 
This function creates an instance of <datatype> from <value>. 
Note that white spaces of head and tail of <value> are trimed and read to a lisp object 
from <value>. For example, if <datatype> is xsd:integer '010' as <value> is translated to 
10, an instance of cl:integer, then 10^^xsd:integer is made, in which the value is 10."
  (setq value (string-trim '(#\Space #\Tab #\Newline) value))
  (ecase datatype
    ((xsd:float xsd:decimal xsd:integer 
                xsd:long xsd:int xsd:short xsd:byte
                xsd:nonPositiveInteger xsd:negativeInteger 
                xsd:nonNegativeInteger xsd:unsignedLong xsd:unsignedInt xsd:unsignedShort xsd:unsignedByte 
                xsd:positiveInteger)
     (^^ (read-from-string value) (symbol-value datatype)))
    (xsd:double (^^ (read-from-string (format nil "~Ad0" value)) (symbol-value datatype)))
    (xsd:decimal (^^ (rational (read-from-string (format nil "~Ad0" value))) (symbol-value datatype)))
    (xsd:string (make-instance (symbol-value datatype) :value value))
    (xsd:anyURI (iri value))
    (xsd:boolean (cond ((string= value "1") t)
                       ((string= value "0") nil)
                       ((string-equal value "true") t)
                       ((string-equal value "false") nil)
                       ((error "Illegal value for datatype ~S:~S" datatype value))))
    (xsd:duration (parse-as-duration value nil))
    (xsd:anySimpleType (error "Did you define new xsd type?"))
    (rdf:XMLLiteral (error "Not Yet!"))))

(defun ^^ (value type)
  "makes an XML typed data with <value> of <type>.
   If <value> is a string, <read-as-datatype> is called, otherwise makes typed data of <value> and <type>.
   This function returns an instance of ill-structured-XMLLiteral when <value> does not comform to <type>."
  (flet ((name (x) (if (symbolp x) x (class-name x))))
    (typecase value
      (cl:string (read-as-datatype value (name type)))
      (otherwise
       (case (name type)
         (xsd:float (make-instance type :value (float value 1.0)))
         (xsd:double (make-instance type :value (float value 1.0d0)))
         (xsd:decimal (make-instance type :value (rational value)))
         (otherwise
          (if (cl:typep value (name type)) (make-instance type :value value)
            (make-instance type :value (coerce value (name type))))))))))

(defun RDFdeclEndp? (stream)
  "Is RDFdecl closing?"
  (cond ((match-pattern-p "</RDF>" stream) (skip-pattern "</RDF>" stream) t)
        ((match-pattern-p "</rdf:RDF>" stream) (skip-pattern "</rdf:RDF>" stream) t)
        ((match-pattern-p "</owlx:Ontology>" stream) (skip-pattern "</owlx:Ontology>" stream) t)
        (t nil)))

(defun RDFdecl? (stream)
  "Is RDFdecl opening up?"
  (cond ((match-pattern-p "<RDF" stream)
         (skip-pattern "<RDF" stream)
         (cond ((space-p stream) t)
               ((match-pattern-p ">" stream) t)
               (t nil)))
        ((match-pattern-p "<rdf:RDF" stream)
         (skip-pattern "<rdf:RDF" stream)
         (cond ((space-p stream) t)
               ((match-pattern-p ">" stream) t)
               (t nil)))
        ((match-pattern-p "<owlx:Ontology" stream)
         (skip-pattern "<owlx:Ontology" stream)
         (cond ((space-p stream) t)
               ((match-pattern-p ">" stream) t)
               (t nil)))
        (t nil)))

(defun parse-element (stream)
  "parses XMLDecl, or doctypedecl, or comment, or RDFdecl."
  (skipbl stream)
  (cond ((match-pattern-p "<?xml " stream)
         (assert-pattern "<?xml " stream)
         (prog1 (parse-XMLDecl stream) (skipbl stream)))
        ((match-pattern-p "<!DOCTYPE" stream)
         (read-pattern-p "<!DOCTYPE" stream)
         (prog1 (parse-doctypedecl stream) (skipbl stream)))
        ((Comment? stream) (prog1 (parse-Comment stream) (skipbl stream)))
        ((RDFdecl? stream) (parse-RDFdecl stream))))

;;;
;;;; Producer & Consumer model for RDF parsing
;;;

(defun make-rdf/xml-parser (stream)
  "makes a parser for RDF/XML."
  #'(lambda (-C-)
      (labels ((read-something (=C=)
                               (let ((result (parse-element stream)))
                                 (cond ((null result) nil) ; and stop continuation
                                      (t (funcall =C=         ; invoke accept with element and cont.
                                                  result
                                                  #'(lambda (*C*)
                                                      (read-something *C*))))))))
        (read-something -C-))))

(defun parse-rdf (stream)
  "parses RDF/XML from <stream> and makes the internal representation.
   This function returns a list of XML structures.
   Note that *base-uri* and *default-namespace* are updated through 
   content from <stream>."
  (flush-buf)
  (let ((results '()))
    (labels ((accept (element -C-)
                     (when element
                       (push element results)
                       (unless (RDF-p element) (funcall -C- #'accept)))))
      (funcall (make-rdf/xml-parser stream) #'accept)
      (nreverse results))))

#|
(defstruct (elementdecl (:print-function print-elementdecl))
  name content)
(defun print-elementdecl (r s k)
  (declare (ignore k))
  (format s "<!ENTITY ~A ~W >" (elementdecl-name r) (elementdecl-content r)))

(defun parse-edecl (edecl)
  (assert (eq (car edecl) :ENTITY))
  (make-elementdecl :name (second edecl) :content (third edecl)))

(defun make-markupdecl (value)
  (when (consp value)
    (case (car value)
      (:ENTITY (parse-edecl value))
      (otherwise (error "NOT YET")))))

(defun parse-ddecl (ddecl)
  "parses a doctypedecl part in LXML that is returned by parse-rdf 
   makes a doctypedecl structure and returns it."
  (assert (eq (car ddecl) :DOCTYPE))
  (let ((name (second ddecl))
        (values (third ddecl)))
    (when (and (consp values) (eq (car values) :[))
      (setq values (cdr values)))
    (make-doctypedecl :name name :values (loop for val in values collect (make-markupdecl val)))))
  )
;;
;;
;;

(defun make-sexpr-writer (stream)
  #'(lambda (element reader)
      (labels ((write-loop (element reader)
                 (cond ((eq element :EOF) nil)
                       ((null element)
                        (funcall reader #'(lambda (elm rdr) (write-loop elm rdr))))
                       ((Description-p element)
                        (write (description->sexpr element) :stream stream)
                        (funcall reader #'(lambda (elm rdr) (write-loop elm rdr))))
                       ((comment-p element)
                        (write (comment->sexpr element) :stream stream)
                        (funcall reader #'(lambda (elm rdr) (write-loop elm rdr))))
                       ((doctypedecl-p element)
                        (write element :stream stream)
                        (funcall reader #'(lambda (elm rdr) (write-loop elm rdr))))
                       ((XMLDecl-p element)
                        (write element :stream stream)
                        (funcall reader #'(lambda (elm rdr) (write-loop elm rdr))))
                       (t (write element :stream stream)
                          (funcall reader #'(lambda (elm rdr) (write-loop elm rdr)))))))
        (write-loop element reader))))

(defun accept-slot (subj prop accept-fun)
  (%accept-slot subj (prop-name prop) (prop-att&vals prop) (prop-value prop) accept-fun))

(defun %accept-slot (subj name attvals value accept-fun)
  (cond ((and (consp attvals) (eq (car attvals) 'rdf:datatype))
         (apply accept-fun
                (list subj (symbol2uri name)
                         (make-XMLSchema-data (car value) (uri2symbol (net.uri:uri (second attvals)))))))
        (t (apply accept-fun (list subj (symbol2uri name) (car value))))))

(declaim (inline EncodingDecl? SDDecl?
                 NameChar-p uri2package ETag-p))

;;
;; Parse RDF/XML, reads RDF/XML and makes an RDF structure
;;

(defun parse-rdecl (rdecl)
  "parses a RDFdecl part in LXML that is returned by parse-rdf 
   makes a RDFdecl structure and returns it."
  (let ((tag (car rdecl))
        (attvals nil)
        (body (cdr rdecl)))
    (when (consp tag)
      (setq attvals (cdr tag))
      (setq tag (car tag)))
    (assert (or (string= (cl:string tag) "rdf:RDF")
                (string= (cl:string tag) "RDF")))
    (make-RDF :att&vals attvals
              :body (loop for decl in body
                        unless (white-space-p decl)
                        collect (LXML2DOM decl)))))

(defun parse-slot (slot)
  (let ((prop (car slot))
        (attvals nil)
        (values (cdr slot))
        (parseType nil))
    (when (consp prop)
      (setq attvals (cdr prop))
      (setq prop (car prop)))
    (cond ((setq parseType (getf attvals 'rdf:parseType))
           (cond ((string= parseType "Resource")
                  (make-prop :name prop :att&vals attvals 
                             :value (loop for value in values
                                        unless (white-space-p value)
                                        collect (parse-slot value))))
                 ((string= parseType "Collection")
                  (make-prop :name prop :att&vals attvals 
                             :value (loop for value in values
                                        unless (white-space-p value)
                                        collect (LXML2DOM value))))
                 ((string= parseType "Literal")
                  (make-prop :name prop :att&vals attvals 
                             :value (loop for value in values
                                        unless (white-space-p value)
                                        collect (parse-slot value))))
                 ((error "Not Yet Implemented parseType=~S." parseType))))
          (t (make-prop :name prop :att&vals attvals 
                        :value (loop for value in values
                                   unless (white-space-p value)
                                   collect (LXML2DOM value)))))))

|#

(defun rdf-db-register (name pos depth)
  "hook for external database."
  (declare (ignore name pos depth))
  )
(defun rdf-db-open-p ()
  "hook for external database."
  )

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Sep-13-2008
;;;
