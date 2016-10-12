;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf Share module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This module is separated from original Rdf module in order to separate sharable parts from Rdf module.
;;;
;;; Copyright (c) 2002, 2004 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2007, 2008 Seiji Koide
;;;
;; History
;; -------
;; 2009.10.02    This file is copied from the original and modified for Turtle.
;; 2008.09.11    URI and Namespace part is separated from this file to NameSpace module.
;; 2008.09.10    The definition of duration is moved here from the file duration.
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2007.12.18    RdfShare is separated from Rdf module in order to share routines with RDFGate program
;;; ==================================================================================

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  (require :rdfio)
  (require ::namespace)
  ) ; end of eval-when

(in-package :gx)

(shadow '(uri parse-uri))
(export '(*entity-decls* NameStartChar-p NameChar-p NCNameStartChar-p NCNameChar-p
           make-unique-nodeID
           parse-iri read-Eq
           get-uri-namedspace uri-namedspace
           comment-p))

(declaim (inline %read-Name %read-NCName %read-Nmtoken %read-EncName 
                 NameStartChar-p NCNameStartChar-p NameChar-p NCNameChar-p EncName-p CDStart-p
                 peep-QNameString EntityDecl? markupdecl?
                 ))

;;;
;;;; Sharable Functions for XML 1.1 or 1.0 and NameSpace
;;;
;;; In the followings, each specification from XML 1.1 (http://www.w3.org/TR/xml11/)
;;; or 1.0 (http://www.w3.org/TR/xml/) or Namespaces in XML 1.1 (http://www.w3.org/TR/xml-names11/) 
;;; is listed before the definition in Lisp.

;;; ----------------------------------------------------------------------------------
;;; [4]    NameStartChar    ::=
;;;         ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | 
;;;         [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | 
;;;         [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | 
;;;         [#x10000-#xEFFFF] 
;;; ----------------------------------------------------------------------------------
(defun NameStartChar-p (char)
  "returns true if <char> is a NameStartChar."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (= code #x003A)   ; ':'
        (<= #x0041 code #x005A)
        (= code #x005F)
        (<= #x0061 code #x007A)
        (<= #x00C0 code #x00D6) (<= #x00D8 code #x00F6) (<= #x00F8 code #x02FF)
        (<= #x0370 code #x037D) (<= #x037F code #x1FFF) (<= #x200C code #x200D) (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF) (<= #x3001 code #xD7FF) (<= #xF900 code #xFDCF) (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))
;;;
;;; ----------------------------------------------------------------------------------
;;; Namespaces[6]    NCNameStartChar    ::=    NameStartChar - ':'  
;;; ----------------------------------------------------------------------------------
(defun NCNameStartChar-p (char)
  "returns true if <char> is an NCNameStartChar."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (<= #x0041 code #x005A)
        (= code #x005F)
        (<= #x0061 code #x007A)
        (<= #x00C0 code #x00D6) (<= #x00D8 code #x00F6) (<= #x00F8 code #x02FF)
        (<= #x0370 code #x037D) (<= #x037F code #x1FFF) (<= #x200C code #x200D) (<= #x2070 code #x218F)
        (<= #x2C00 code #x2FEF) (<= #x3001 code #xD7FF) (<= #xF900 code #xFDCF) (<= #xFDF0 code #xFFFD)
        (<= #x10000 code #xEFFFF))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [4a]    NameChar    ::=
;;;          NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] |
;;;          [#x203F-#x2040] 
;;; ----------------------------------------------------------------------------------
(defun NameChar-p (char)
  "returns true if <char> is a NameChar."
  (declare (optimize (speed 3) (safety 1)))
  (or (NameStartChar-p char)
      (let ((code (char-code char)))
        (or (= #x002D code)
            (= #x002E code)
            (<= #x0030 code #x0039)
            (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; Namespaces[5]    NCNameChar    ::=    NameChar - ':' 
;;; ----------------------------------------------------------------------------------
(defun NCNameChar-p (char)
  "returns true if <char> is an NCNameChar."
  (declare (optimize (speed 3) (safety 1)))
  (or (NCNameStartChar-p char)
      (let ((code (char-code char)))
        (or (= #x002D code)
            (= #x002E code)
            (<= #x0030 code #x0039)
            (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')* 
;;; ----------------------------------------------------------------------------------
(defun EncName-p (char)
  "returns true if <char> is an EncName."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (<= #x0041 code #x005A)         ;[A-Z]
        (<= #x0061 code #x007A)         ;[a-z]
        (<= #x0030 code #x0039)         ;[0-9]
        (= code #x002D)                 ; -
        (= code #x002E)                 ; .
        (= code #x005F)                 ; _
        )))
;;;
;;; ----------------------------------------------------------------------------------
;;; [5]    Name    ::=    NameStartChar (NameChar)* 
;;; ----------------------------------------------------------------------------------
(defun read-Name (stream)
  "reads a name from <stream> and returns it as string."
  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-Name stream) 'cl:string))

(defun %read-Name (stream)
  "reads a name from <stream> and returns characters in list."
  (declare (optimize (speed 3) (safety 1)))
  (assert (NameStartChar-p (peeknext-char stream)) () "Illegal NameStartChar ~W at line ~S"
          (peeknext-char stream) (line-count stream))
  (let (c name)
    (setq c (getnext-char stream))
    (setq name
          (loop while (NameChar-p c)
              collect c
              do (setq c (getnext-char stream))))
    (putback-char c stream)
    name))
;;;
;;; ----------------------------------------------------------------------------------
;;; [4]    NCName    ::=    NCNameStartChar NCNameChar* /* An XML Name, minus the ":" */ 
;;; ----------------------------------------------------------------------------------
(defun read-NCName (stream)
  "reads a NCname from <stream> and returns it as string. The token must be NCName."
  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-NCName stream) 'cl:string))

(defun %read-NCName (stream)
  "reads a NCName from <stream> and returns chars in list."
  (declare (optimize (speed 3) (safety 1)))
  (assert (NCNameStartChar-p (peeknext-char stream)) () "Illegal NCNameStartChar ~W at line ~S"
          (peeknext-char stream) (line-count stream))
  (let (c name)
    (setq c (getnext-char stream))
    (setq name
          (loop while (NCNameChar-p c)
              collect c
              do (setq c (getnext-char stream))))
    (putback-char c stream)
    (coerce name 'cl:string)))

(defun read-quoted-NCName (stream)
  "reads a quoted NCName from <stream> and returns the NCName as string."
  (declare (optimize (speed 3) (safety 1)))
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (let ((NCName (%read-NCName stream)))
      (assert (char= q (getnext-char stream)))
      (coerce NCName 'cl:string))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [7]    Nmtoken    ::=    (NameChar)+ 
;;; ----------------------------------------------------------------------------------
(defun read-Nmtoken (stream)
  "reads a Nmtoken from <stream> and returns it as string."
  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-Nmtoken stream) 'cl:string))

(defun %read-Nmtoken (stream)
  "reads a Nmtoken from <stream> and returns chars in list."
  (declare (optimize (speed 3) (safety 1)))
  (assert (NameChar-p (peeknext-char stream)) () "Illegal NameChar ~W at line ~S"
          (peeknext-char stream) (line-count stream))
  (let (c name)
    (setq name
          (loop while (NameChar-p (setq c (getnext-char stream)))
              collect c))
    (putback-char c stream)
    name))
;;;
;;; ----------------------------------------------------------------------------------
;;; [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')* 
;;; ----------------------------------------------------------------------------------
(defun read-EncName (stream)
  "reads a EncName from <stream> and returns it as string."
  (declare (optimize (speed 3) (safety 1)))
  (coerce (%read-EncName stream) 'cl:string))

(defun %read-EncName (stream)
  "reads a EncName from <stream> and returns chars in list."
  (declare (optimize (speed 3) (safety 1)))
  (let (c name)
    (setq c (peeknext-char stream))
    (assert (or (<= #x0041 (char-code c) #x005A) (<= #x0061 (char-code c) #x007A))
            () "Illegal EncName ~W at line ~S" c (line-count stream))
    (setq c (getnext-char stream))
    (setq name
          (loop while (EncName-p c)
              collect c
              do (setq c (getnext-char stream))))
    (putback-char c stream)
    name))

;;
;; XML Reader
;;

;;; ----------------------------------------------------------------------------------
;;; [14]    CharData    ::=    [<&]* - ([<&]* ']]>' [<&]*) 
;;; [18]    CDSect    ::=    CDStart CData CDEnd  
;;; [19]    CDStart    ::=    '<![CDATA[' 
;;; [20]    CData    ::=    (Char* - (Char* ']]>' Char*))  
;;; [21]    CDEnd    ::=    ']]>' 
;;; ----------------------------------------------------------------------------------
(defun CDStart-p (stream)
  "Does CDStart characters come from <stream> next?"
  (declare (optimize (speed 3) (safety 1)))
  (when (match-pattern-p "<![CDATA[" stream)
    (skip-pattern "<![CDATA[" stream)
    t))

(defun read-CData-to-CDEnd (stream)
  "reads string from <stream> up to CDEnd, and returns CData."
  (declare (optimize (speed 3) (safety 1)))
  (prog1 (parse-pattern-delimited-string "]]>" stream)
    (skip-pattern "]]>" stream)))

;;;
;;;; Read-entity-decls
;;;
;;; <read-entity-decls> tranforms a sequence of characters from stream that are declared as character 
;;; entity to the designated character. For example,
;;;  * &amp;amp; -> '&amp;'
;;;  * &amp;lt;  -> '&lt;'
;;;  * &amp;gt;  -> '&gt;'
;;;  * &amp;apos; -> "'"
;;;  * &amp;quot; -> '&quot;'

(defparameter *entity-decls*
  (acons "quot" #\"
         (acons "apos" #\'
                (acons "gt" #\>
                       (acons "lt" #\<
                              (acons "amp" #\& nil)))))
  "storage for entity-decls")

(defun read-entity-decls (stream)
  "reads entity delcs and returns mapped char. This function should be called just after '&'."
  (declare (optimize (speed 3) (safety 1)))
  (let ((pat (loop for cc = (getnext-char stream)
                 until (char= cc #\;)
                 collect cc)))
    (setq pat (coerce pat 'cl:string))
    (let ((found (cdr (assoc pat *entity-decls* :test #'string=))))
      (cond (found)
            (t (error "There is no entity definition for ~A at line ~S." pat (line-count stream)))))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [25]    Eq    ::=    S? '=' S? 
;;; ----------------------------------------------------------------------------------
(defun read-Eq (stream &key format-control format-arguments)
  "reads Eq and returns character '='"
  (declare (optimize (speed 3) (safety 1)))
  (skipbl stream)
  (cond ((char= (getnext-char stream) #\=)
         (skipbl stream)
         #\=)
        ((error 'no-Eq-error 
           :format-control format-control
           :format-arguments format-arguments))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [23]    XMLDecl    ::=    '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>' 
;;; ----------------------------------------------------------------------------------
(defun read-XMLDecl (stream)
  "reads XMLDecl just after '&lt;?xml'."
  (let* ((version (parse-VersionInfo stream))
         (encoding (when (EncodingDecl? stream) (read-EncodingDecl stream)))
         (standalone (when (SDDecl? stream) (read-SDDecl stream))))
    (skipbl stream)
    (assert-pattern "?>" stream)
    (values version encoding standalone)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [24]    VersionInfo    ::=
;;;                      S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"') 
;;; ----------------------------------------------------------------------------------
(defun parse-VersionInfo (stream)
  (skipbl stream)
  (assert-pattern "version" stream)
  (read-Eq stream :format-control " after 'version'")
  (read-quoted-string stream))
;;;
;;; ----------------------------------------------------------------------------------
;;; [80]    EncodingDecl    ::=
;;;                         S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" ) 
;;; ----------------------------------------------------------------------------------
(defun EncodingDecl? (stream)
  "returns true if character sequence 'enconding' is detected from <stream>."
  (skipbl stream)
  (match-pattern-p "encoding" stream))
(defun read-EncodingDecl (stream)
  "reads EncodingDecl and returns EncName that follows '='."
  (skipbl stream)
  (assert-pattern "encoding" stream)
  (read-Eq stream :format-control " after 'encoding'")
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (prog1 (read-EncName stream)
      (assert (char= (getnext-char stream) q)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [32]    SDDecl    ::=    S 'standalone' Eq (("'" ('yes' | 'no') "'") |
;;;                          ('"' ('yes' | 'no') '"'))  
;;; ----------------------------------------------------------------------------------
(defun SDDecl? (stream)
  "returns true if character sequence 'standalone' is detected from <stream>."
  (skipbl stream)
  (match-pattern-p "standalone" stream))
(defun read-SDDecl (stream)
  "Not Yet implemented."
  (declare (ignore stream))
  (error "Not Yet!"))

;;;
;;;; Comment
;;; ----------------------------------------------------------------------------------
;;;  [15]    Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->' 
;;; ----------------------------------------------------------------------------------
(defun Comment? (stream)
  "returns true if character sequence '<!--' is detected from <stream>."
  (declare (optimize (speed 3) (safety 1)))
  (match-pattern-p "<!--" stream))

(defun read-Comment (stream)
  "reads commented string from <stream> using <parse-pattern-delimited-string>."
  (assert-pattern "<!--" stream)
  (prog1 (parse-pattern-delimited-string "-->" stream)
    (assert-pattern "-->" stream)))

;;;
;;; In SWCLOS, comment is a structure that has slot <body>.

(defstruct (comment (:print-function print-comment)) body)
(defun print-comment (r s k)
  "prints out Comment string. This function is not intended to be used by user."
  (declare (ignore k))
  (format s "~&<!--~A-->" (comment-body r)))

(defun parse-Comment (stream)
  "reads Comment from <stream> and returns a comment structure."
  (make-comment :body (read-Comment stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [11]    SystemLiteral    ::=    ('"' [^"]* '"') | ("'" [^']* "'") 
;;; ----------------------------------------------------------------------------------
(defun read-SystemLiteral (stream)
  "reads SystemLiteral and returns the string."
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (coerce 
     (loop for c = (getnext-char stream)
         until (char= c q)
         collect c)
     'cl:string)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [13]    PubidChar    ::=    #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%] 
;;; ----------------------------------------------------------------------------------
(defun PubidChar-p (char)
  "returns true if <char> is a PubidChar."
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code char)))
    (or (= code #x20)
        (= code #xD)
        (= code #xA)
        (<= #x0041 code #x005A)
        (<= #x0061 code #x007A)
        (<= #x0030 code #x0039)
        (= code #x002D) ; -
        (= code #x0027) ; '
        (= code #x0028) ; (
        (= code #x0029) ; )
        (= code #x002B) ; +
        (= code #x002C) ; ,
        (= code #x002E) ; .
        (= code #x002F) ; /
        (= code #x003A) ; :
        (= code #x003D) ; =
        (= code #x003F) ; ?
        (= code #x003B) ; ;
        (= code #x0021) ; !
        (= code #x002A) ; *
        (= code #x0023) ; #
        (= code #x0040) ; @
        (= code #x0024) ; $
        (= code #x005F) ; _
        (= code #x0025) ; %
        )))
;;;
;;; ----------------------------------------------------------------------------------
;;; [12]    PubidLiteral    ::=    '"' PubidChar* '"' | "'" (PubidChar - "'")* "'" 
;;; ----------------------------------------------------------------------------------
(defun read-PubidLiteral (stream)
  "reads PubidLiteral from <stream> and returns the string."
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (coerce 
     (loop for c = (getnext-char stream)
         until (char= c q)
         do (assert (PubidChar-p c))
         collect c)
     'cl:string)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral 
;;; ----------------------------------------------------------------------------------
(defun ExternalID? (stream)
  "Is the next pattern in <stream> SYSTEM or PUBLIC?"
  (declare (optimize (speed 3) (safety 1)))
  (or (match-pattern-p "SYSTEM" stream)
      (match-pattern-p "PUBLIC" stream)))

(defun parse-ExternalID (stream)
  "If the next pattern is SYSTEM or PUBLIC, then reads and parse the ExternalID from <stream>."
  (cond ((match-pattern-p "SYSTEM" stream)
         (read-pattern-p "SYSTEM" stream)
         (skipbl stream)
         (read-SystemLiteral stream))
        ((match-pattern-p "PUBLIC" stream)
         (read-pattern-p "PUBLIC" stream)
         (skipbl stream)
         (list (prog1 (read-PubidLiteral stream) (skipbl stream))
                  (read-SystemLiteral stream)))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [29]    markupdecl    ::=    elementdecl | AttlistDecl | EntityDecl | 
;;;                              NotationDecl | PI | Comment 
;;; ----------------------------------------------------------------------------------
(defun markupdecl? (stream)
  "this function only detects EntityDecl."
  (declare (optimize (speed 3) (safety 1)))
  (or (EntityDecl? stream)))
(defun parse-markupdecl (stream)
  "this function only parses EntityDecl."
  (declare (optimize (speed 3) (safety 1)))
  (cond ((EntityDecl? stream) (parse-EntityDecl stream))
        ((error "Not Yet!"))))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28b]    intSubset    ::=    (markupdecl | DeclSep)* 
;;; ----------------------------------------------------------------------------------
(defun intSubset? (stream)
  "returns true if intSubset is detected from <stream>."
  (or (markupdecl? stream)
      (DeclSep? stream)))
(defun parse-intSubset (stream)
  "reads markupdecl or Comment and parse it."
  (skipbl stream)
  (loop while (or (intSubset? stream) (Comment? stream))
      do (cond ((markupdecl? stream) (parse-markupdecl stream))
               ((Comment? stream) (parse-Comment stream))
               ((DeclSep? stream) (error "Not Yet!"))
               ((error "Cant happen!")))
        (skipbl stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28a]    DeclSep    ::=    PEReference | S 
;;; ----------------------------------------------------------------------------------
(defun DeclSep? (stream)
  "returns true if PEReference is detected."
  (char= #\% (peeknext-char stream)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [28]    doctypedecl    ::=
;;;             '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>' 
;;; ----------------------------------------------------------------------------------
(defun read-doctypedecl (stream)
  "reads doctypedecl."
  (skipbl stream)
  (let* ((Name (read-Name stream)) 
         (eID (when (ExternalID? stream) (parse-ExternalID stream)))
         (values nil))
    (assert (or (string= Name "rdf:RDF") (string= Name "RDF")
                (string= Name "owl")
                (string= Name "uridef"))) ; uridef is for service.owl
    (skipbl stream)
    (cond ((char= #\[ (peeknext-char stream))
           (getnext-char stream)
           (setq values (parse-intSubset stream))
           (assert (char= #\] (getnext-char stream)))
           ))
    (skipbl stream)
    (assert (char= #\> (getnext-char stream)))
    (values Name eID values)))
;;;
;;; ----------------------------------------------------------------------------------
;;; [6]     QName            ::=    PrefixedName 
;;;                                 | UnprefixedName 
;;; [6a]    PrefixedName     ::=    Prefix ':' LocalPart  
;;; [6b]    UnprefixedName   ::=    LocalPart  
;;; [7]     Prefix           ::=    NCName 
;;; [8]     LocalPart        ::=    NCName 
;;; ----------------------------------------------------------------------------------

(defun read-QNameString (stream)
  "reads a Qname from <stream> and returns it as string. The first token in <stream> must be NCName."
  (declare (optimize (speed 3) (safety 1)))
  (let (Prefix LocalPart)
    (setq Prefix (read-NCName stream))
    (cond ((match-pattern-p ":" stream)
           (read-pattern-p ":" stream)
           (setq LocalPart (read-NCName stream))
           (concatenate 'string Prefix ":" LocalPart))
          (t Prefix ; returns a string
             ))))

(defun peep-QNameString (stream)
  "peeps QName in <stream> and returns the string. The first token in <stream> must be NCName."
  (declare (optimize (speed 3) (safety 1)))
  (let ((QName (read-QNameString stream)))
    (putback-pattern QName stream)
    QName))

(defun read-QName (stream)
  "reads a Qname from <stream> and returns it as symbol if possible. The first token in <stream> must 
be NCName. If there is no package named Prefix, the package is created. If QName has no Prefix, 
LocalPart is interned in <*default-namespace*> package, or <*base-uri*> package. If no Prefix and 
no <*default-namespace*> and no <*base-uri*>, the string is returned."
  (declare (optimize (speed 3) (safety 1)))
  (let (Prefix LocalPart QName pkg)
    (setq Prefix (read-NCName stream))
    (cond ((match-pattern-p ":" stream)
           (read-pattern-p ":" stream)
           (setq LocalPart (read-NCName stream))
           (setq pkg (find-package Prefix)) ; nicknames availabel
           (when (null pkg)
             (warn "There is no package for ~A." Prefix)
             (setq pkg (make-package Prefix :use nil))  ; by smh
             (warn "~W created." pkg))
           (shadow LocalPart pkg)
           (setq QName (intern LocalPart pkg))
           (export QName pkg))
          (*default-namespace*
           (setq pkg (uri-namedspace-package *default-namespace*))
           (shadow Prefix pkg)
           (setq QName (intern Prefix pkg))
           (export QName pkg))
          (*base-uri*
           (setq pkg (uri-namedspace-package *base-uri*))
           (cond (pkg (shadow Prefix pkg)
                      (setq QName (intern Prefix pkg))
                      (export QName pkg))
                 (t (setq QName Prefix)))) ; returns a string
          (t (setq QName Prefix)))         ; returns a string
    QName))
;;;
;;; ----------------------------------------------------------------------------------
;;; [70]    EntityDecl   ::=    GEDecl | PEDecl 
;;; [71]    GEDecl       ::=    '<!ENTITY' S Name S EntityDef S? '>' 
;;; [72]    PEDecl       ::=    '<!ENTITY' S '%' S Name S PEDef S? '>' 
;;; [73]    EntityDef    ::=    EntityValue | (ExternalID NDataDecl?) 
;;; ----------------------------------------------------------------------------------
(defun EntityDecl? (stream)
  "returns true if EntityDecl is detected from <stream>."
  (declare (optimize (speed 3) (safety 1)))
  (match-pattern-p "<!ENTITY" stream))

(defun parse-EntityDecl (stream)
  "reads EntityDecl from <stream> and registers the declaration."
  (declare (optimize (speed 3) (safety 1)))
  (assert-pattern "<!ENTITY" stream)
  (skipbl stream)
  (cond ((match-pattern-p "% " stream)
         (skipbl stream)
         (read-Name stream)
         (skipbl stream)
         (error "Not Yet!"))
        (t (let ((Name (read-Name stream)) quoted)
             (skipbl stream)
             (setq quoted (read-quoted-string stream))
             (unless (and (cdr (assoc Name *entity-decls* :test #'string=))
                          (string= (cdr (assoc Name *entity-decls* :test #'string=)) quoted))
               (setq *entity-decls* (acons Name quoted *entity-decls*))))))
  (skipbl stream)
  (assert (char= #\> (getnext-char stream))))

;;;
;;;; Peeping File Coding
;;;
;;; Even though a file includes the character encoding information in XMLDecl part, 
;;; we cannot know it without opening and peeping it.
;;; The following functions allow us to peep a file, looking for character encoding.
;;;

(defun peep-XMLDecl-code-from-file (file)
  "peeps <file> and returns a character code declared in XMLDecl."
  (with-open-file (stream (pathname file))
    (%peep-XMLDecl-code stream)))

(defun peep-XMLDecl-code-from-string (rdf-string)
  "peeps <rdf-string> and returns a character code declared in XMLDecl."
  (with-input-from-string (stream rdf-string)
    (%peep-XMLDecl-code stream)))

(defun %peep-XMLDecl-code (stream)
  "peeps <stream> and returns a character code declared in XMLDecl."
  (when (match-pattern-p "<?xml " stream)
    (skip-pattern  "<?xml " stream)
    (multiple-value-bind (version encoding standalone) (read-XMLDecl stream)
      (declare (ignore version standalone))
      (cond ((null encoding) nil)
            ((string-equal encoding "") nil)
            ((string-equal encoding "Shift_JIS") :932)
            ((string-equal encoding "ISO-8859-1") :latin1)
            ((string-equal encoding "UTF-8") :utf-8)      ; by smh
            ((error "Please update %peep-XMLDecl-code for ~A" encoding))))))

;;;
;;;; Data Type duration
;;;
;;; PnYnMnDTnHnMnS, ex. P1Y2M3DT10H30M
;;;

(defun parse-as-duration (str stream)
  (assert (char= (char str 0) #\P))
  (let ((pos 1)
        (num nil)
        yy mo dd hh mi ss)
    (flet ((parse-preT ()
                       (loop while (multiple-value-setq (num pos)
                                     (parse-integer str :start pos :junk-allowed t))
                           do (let ((c (char str pos)))
                                (cond ((char= c #\Y) (setq hh num))
                                      ((char= c #\M) (setq mi num))
                                      ((char= c #\D) (setq ss num))
                                      ((error "Illegal duration format at line ~S" (line-count stream)))))))
           (parse-postT ()
                        (loop while (multiple-value-setq (num pos)
                                      (parse-integer str :start pos :junk-allowed t))
                            do (let ((c (char str pos)))
                                 (cond ((char= c #\H) (setq yy num))
                                       ((char= c #\M) (setq mo num))
                                       ((char= c #\S) (setq dd num))
                                       ((error "Illegal duration format at line ~S" (line-count stream))))))))
      (cond ((char= (char str pos) #\T)
             (incf pos)
             (parse-postT))
            (t (parse-preT)
               (when (char= (char str pos) #\T)
                 (incf pos)
                 (parse-postT)))))
    (make-instance 'xsd:duration
      :year yy
      :month mo
      :day dd
      :hour hh
      :minute mi
      :second ss)))

(defun parse-duration (stream)
  (assert (char= (peeknext-char stream) #\P))
  (getnext-char stream)
  (let (yy mo dd hh mi ss)
    (flet ((read-digits () (loop while (char= #\0 (peeknext-char stream) #\9)
                               collect (getnext-char stream))))
      (flet ((parse-preT ()
               (let ((dlst (read-digits))
                     (num nil))
                 (when dlst (setq num (cl:parse-integer (coerce dlst 'string)))
                   (let ((c (getnext-char stream)))
                     (cond ((char= c #\H) (setq yy num))
                           ((char= c #\M) (setq mo num))
                           ((char= c #\S) (setq dd num))
                           ((error "Illegal duration format at line ~S" (line-count stream))))))))
             (parse-postT ()
               (let ((dlst (read-digits))
                     (num nil))
                 (when dlst (setq num (cl:parse-integer (coerce dlst 'string)))
                   (let ((c (getnext-char stream)))
                     (cond ((char= c #\Y) (setq hh num))
                           ((char= c #\M) (setq mi num))
                           ((char= c #\D) (setq ss num))
                           ((error "Illegal duration format at line ~S" (line-count stream)))))))))
        (cond ((char= (peeknext-char stream) #\T) (parse-postT))
              (t (parse-preT)))))
    (make-instance 'xsd:duration
      :year yy
      :month mo
      :day dd
      :hour hh
      :minute mi
      :second ss)))

(defun double-angle-bracket-reader (stream char)
  (let ((nc (peek-char nil stream t nil t)))
    (cond ((char= nc #\<)          ; double #\<
           (read-char stream)      ; discard it
           (let* ((uri-str 
                   (coerce 
                    (loop with char until (char= #\> (setq char (read-char stream))) collect char)
                    'cl:string))
                  (uri (iri uri-str))
                  (symbol nil))
             (when (char= #\> (peek-char nil stream t nil t))
               (read-char stream))  ; discard one more #\>
             (cond ((iri-boundp uri)
                    (list 'quote (iri-value uri)))
                   ((setq symbol (uri2symbol uri))
                    (let ((obj nil))
                      (cond ((boundp symbol)
                             (setf (iri-value uri) (setq obj (symbol-value symbol))))
                            (t (setf (iri-value uri)
                                 (setq obj (make-instance '|rdfs:Resource| :name symbol)))))
                      (setf (slot-value obj 'rdf:about) uri)
                      (list 'quote obj)))
                   (t ;; blank node
                    (setf (iri-value uri) (make-instance '|rdfs:Resource|))
                    (list 'quote (iri-value uri))))))
          ((NCNameStartChar-p nc)
           (let* ((uri-str 
                   (coerce 
                    (loop with char until (char= #\> (setq char (read-char stream))) collect char)
                    'cl:string))
                  (uri (iri uri-str)))
             uri))
          (t (excl::read-token stream char)
             ))))

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Sep-13-2008
;;;

(cl:provide :rdfshare)
