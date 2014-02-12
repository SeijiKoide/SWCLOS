;;;-*- Mode: common-lisp; syntax: common-lisp; package: ttl; base: 10 -*-
;;;
;;; Turtle Parser Module
;;; Programed by Seiji Koide
;;; See, http://www.w3.org/TR/turtle/

(cl:provide :ttlparser)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :ttl 
    (:use #:common-lisp)
    (:import-from :gx #:*base-uri* #:*default-namespace* #:nodeID2symbol
                  #:*uri2symbol-package-mapping-fun* #:*uri2symbol-name-mapping-fun*
                  #:*line-pos* #:*line-number* #:*pos*)
    (:export #:*base-uri*)
  )
) ; end of eval-when

(in-package :ttl)

;;;
;;;
;;;

(defun ws-p (char)
  (declare (optimize (speed 3) (safety 1)))
  "WS ::= #x20 | #x9 | #xD | #xA"
  (let ((code (char-code char)))
    (declare (type fixnum code))
    (or (= code #x20) (= code #x9) (= code #xD) (= code #xA))))

(defun skipbl (stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((code (char-code (peek-char nil stream))))
    (cond ((eq code #x20) (read-char stream) (incf *pos*) (skipbl stream))
          ((eq code #x9) (read-char stream) (incf *pos*) (skipbl stream))
          ((eq code #xA) (read-char stream) (incf *pos*)
           (setq *line-pos* *pos*) 
           (incf *line-number*)
           (skipbl stream))
          ((eq code #xD) (read-char stream) (incf *pos*)
           (cond ((eq (char-code (peek-char nil stream)) #xA)
                  ;; then CR+LF
                  (read-char stream) (incf *pos*)
                  (setq *line-pos* *pos*)
                  (incf *line-number*)
                  (skipbl stream))
                 (t (error "Check Newline!")))))))

(defun read-period (stream)
  (skipbl stream)
  (unless (char= (peek-char nil stream) #\.)
    (warn "Syntax error: the period missing.")))

(defun pnCharsBase-p (char)
  (declare (optimize (speed 3) (safety 1)))
  "PN_CHARS_BASE ::= [A-Z] | [a-z] | [#00C0-#00D6] | [#00D8-#00F6] | [#00F8-#02FF] | [#0370-#037D] | [#037F-#1FFF] |
                     [#200C-#200D] | [#2070-#218F] | [#2C00-#2FEF] | [#3001-#D7FF] | [#F900-#FDCF] | [#FDF0-#FFFD] |
                     [#10000-#EFFFF]"
  (or (char<= #\A char #\Z)
      (char<= #\a char #\z)
      (let ((code (char-code char)))
        (declare (type fixnum code))
        (or (<= #x00C0 code #x00D6)
            (<= #x00D8 code #x00F6)
            (<= #x00F8 code #x02FF)
            (<= #x0370 code #x037D)
            (<= #x037F code #x1FFF)
            (<= #x200C code #x200D)
            (<= #x2070 code #x218F)
            (<= #x2C00 code #x2FEF)
            (<= #x3001 code #xD7FF)
            (<= #xF900 code #xFDCF)
            (<= #xFDF0 code #xFFFD)
            (<= #x10000 code #xEFFFF)))))

(defun pnCharsU-p (char)
  "PN_CHARS_U ::= PN_CHARS_BASE | '_' | ':'"
  (or (pnCharsBase-p char)
      (char= char #\_)
      (char= char #\:)))

(defun pnChars-p (char)
  "PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #00B7 | [#0300-#036F] | [#203F-#2040]"
  (or (pnCharsU-p char)
      (char= char #\-)
      (char<= #\0 char #\9)
      (let ((code (char-code char)))
        (or (= code #x00B7)
            (<= #x0300 code #x036F)
            (<= #x203F code #x2040)))))

(defun hex-p (char)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type char char))
  "HEX ::= [0-9] | [A-F] | [a-f]"
  (or (char<= #\0 char #\9) (char<= #\A char #\F) (char<= #\a char #\f)))

(defun string-literal-quote-p (str pos)
  "STRING_LITERAL_QUOTE ::= '\"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '\"'"
  (unless (char= (char str pos) #\") (return-from string-literal-quote-p (values nil pos)))
  (let ((end (position #\" str :test #'char= :start (1+ pos))))
    (unless end (error "Closing double quote missing in literal"))
    (loop with next = (1+ pos) and echar? and enext and uchar? and unext
        for char = (char str next)
        do (when (or (= (char-code char) #x22)
                     (= (char-code char) #x5C)
                     (= (char-code char) #xA)
                     (= (char-code char) #xD))
             (return-from string-literal-quote-p (values nil next)))
          (multiple-value-setq (echar? enext) (echar-p str next))
          (unless echar?
            (multiple-value-setq (uchar? unext) (uchar-p st next))
            (unless uchar?
              (return-from string-literal-quote-p (values nil next)))
            ;; uchar
            (setq next unext)
            )
          ;; echar
          (setq next enext)
        finally (return-from string-literal-quote-p (values t (1+ end))))))

(defun uchar-p (str pos)
  "UCHAR ::= '\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX"
  (unless (char= (char str pos) #\\) (return-from uchar-p (values nil pos)))
  (incf pos)
  (cond ((char= (char str pos) #\u)
         (incf pos)
         (if (and (hex-p (char str pos))
                  (hex-p (char str (1+ pos)))
                  (hex-p (char str (+ pos 2)))
                  (hex-p (char str (+ pos 3))))
             (values t (+ pos 4))
           (values nil pos)))
        ((char= (char str pos) #\U)
         (incf pos)
         (if (and (hex-p (char str pos))
                  (hex-p (char str (1+ pos)))
                  (hex-p (char str (+ pos 2)))
                  (hex-p (char str (+ pos 3)))
                  (hex-p (char str (+ pos 4)))
                  (hex-p (char str (+ pos 5)))
                  (hex-p (char str (+ pos 6)))
                  (hex-p (char str (+ pos 7))))
             (values t (+ pos 8))
           (values nil pos))
         )
        (t (return-from uchar-p (values nil pos)))))

(defun echar-p (str pos)
  "ECHAR ::= '\' [tbnrf\"']"
  (if (char= (char str pos) #\\)
      (if (member (char str (1+ pos)) "tbnrf\"" :test #'char=)
          (values t (+ pos 2))
        (values nil pos))
    (values nil pos)))

(defun NIL-p (str pos)
  "NIL ::= '(' WS* ')'"
  (if (char= (char str pos) #\( )
      (loop for next = (1+ pos) then (1+ next)
          while (ws-p (char str next))
          do (nothing)
          finally (if (char= (char str next) #\)) (values t (1+ next)) (values nil pos)))
    (values nil pos)))

(defun anon-p (str pos)
  "ANON ::= '[' WS* ']'"
  (unless (char= (char str pos) #\[) (return-from anon-p (values nil pos)))
  (loop for next = (1+ pos) then (1+ next)
      while (ws-p (char str next))
      do (nothing)
      finally (if (char= (char str next) #\]) (values t (1+ next)) (values nil pos))))

(defun pnPrefix-p (str pos)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type string str))
  (declare (type fixnum pos))
  "PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?"
  (if (pnCharsBase-p (char str pos))
      (let ((pn_chars-pos pos))
        ;; eat up to period
        (loop for next = (1+ pos) then (1+ next)
            for char = (char str next)
            while (or (pnChars-p char) (char= char #\.))
            do (when (char= char #\.) (setq pn_chars-pos next)))
        ;; comfirm pn_chars?
        (if (pnChars-p (char str (1+ pn_chars-pos)))
            (loop for next = (1+ pn_chars-pos) then (1+ next)
                for char = (char str next)
                while (pnChars-p char)
                finally (return-from pnPrefix-p (values t next)))
          (values nil pos)))
    (values nil pos)))

(defun pnLocal-p (str pos end)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type string str))
  (declare (type fixnum pos))
  ;; PN_LOCAL ::= (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))? 
  (let (result (char (char str pos)))
    (if (or (pnCharsU-p char) (char<= #\0 char #\9)
            pn_chars-pos)
        (let ((pn_chars-pos pos))
          ;; eat up to period
          (loop for next = (1+ pos) then (1+ next)
              for char = (char str next)
              while (or (pnChars-p char) (char= char #\.)
                        (multiple-value-setq (result pos) (plx-p str next)))
              do (when (char= char #\.) (setq pn_chars-pos next)))
          ;; comfirm pn_chars or plx?
          (if (or (pnChars-p (char str (1+ pn_chars-pos)))
                  (multiple-value-setq (result pos) (plx-p str (1+ pn_chars-pos))))
              (loop for next = (1+ pn_chars-pos) then (1+ next)
                  for char = (char str next)
                  while (or (pnChars-p char) (plx-p str next))
                  finally (return-from pnLocal-p (values t next)))
            (values nil pos)))
      (values nil pos))))

(defun plx-p (str pos)
  "PLX ::= PERCENT | PN_LOCAL_ESC"
  (multiple-value-bind (percent new-pos) (percent-p str pos)
    (when percent (return-from plx-p (value t new-pos))))
  (multiple-value-bind (pnlocalesc new-pos) (pnLocalEsc-p str pos)
    (when pnlocalesc (return-from plx-p (value t new-pos))))
  (values nil pos))

(defun percent-p (str pos)
  (declare (optimize (speed 3) (safety 1)))
  "PERCENT ::= '%' HEX HEX"
  (if (and (char= (char str pos) #\%)
           (hex-p (char str (1+ pos)))
           (hex-p (char str (+ pos 2))))
      (values t (+ pos 3))
    (values nil pos)))

(defun pnLocalEsc-p (str pos)
  (declare (optimize (speed 3) (safety 1)))
  (declare (type string str))
  (declare (type fixnum pos))
  "PN_LOCAL_ESC ::= '\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | \"'\" | '(' | ')' | 
                         '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')"
  (if (and (char= (char str pos) #\\) (member (char str (1+ pos)) "_~.-!$&'()*+,;=/?#@%" :test #'char=))
      (values t (+ pos 2))
    (values nil pos)))

;;;
;;;
;;;

(defun read-prefixed-name (stream)
  ;; PrefixedName ::= PNAME_LN | PNAME_NS 
  ;; PNAME_LN ::= PNAME_NS PN_LOCAL 
  ;; PNAME_NS ::= PN_PREFIX? ':' 
  ;; PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)? 
  (skipbl stream)
  (let ((prefixedname
         (coerce 
          (loop for c = (read-char stream)
              until (ws-p c)
              collect c)
          'cl:string)))
    prefixedname))

(defun read-iriref (stream)
  "reads a string up to '>' from <stream>."
  ;;IRIREF ::= '<' ([^#x00-#x20<>\"{}|^`\] | UCHAR)* '>' "
  (skipbl stream)
  (assert (char= (read-char stream) #\<))
  (let ((iristr
         (coerce 
          (loop for c = (read-char stream)
              until (char= c #\>)
              do (when (find c "<>\"{}|^`\\" :test #'char=)
                   (error "Illegal iriref."))
              collect c)
          'cl:string)))
    iristr))

(defun read-blank-node-label (stream)
  ;; BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)? 
  (assert (char= (read-char stream) #\_))
  (assert (char= (read-char stream) #\:))
  (let ((bnodestr
         (coerce 
          (loop for c = (read-char stream)
              until (ws-p c)
              collect c)
          'cl:string)))
    bnodestr))

(defun read-collection (stream)
  (error "Not Yet!"))

(defun read-subject (stream)
  "subject := iri | BlankNode | collection"
  (skipbl stream)
  (let ((ch (peek-char nil stream)))
    (cond ((char= ch #\<) (read-iriref stream))
          ((char= ch #\_) (read-blank-node-label stream))
          ((char= ch #\() (read-collection stream))
          ((error "Illegal subject.")))))

(defun read-predicate (stream)
  ;; predicate ::= iri 
  (read-iri stream))

(defun read-verb (stream)
  (skipbl stream)
  (let ((ch (peek-char nil stream)))
    (cond ((char= ch #\<) (read-iri stream))
          (t (read-prefixed-name stream)))))

(defun read-object (stream)
  ;; object ::= iri | BlankNode | collection | blankNodePropertyList | literal 
  ;; iri ::= IRIREF | PrefixedName 
  ;; literal ::= RDFLiteral | NumericLiteral | BooleanLiteral 
  (skipbl stream)
  (let ((ch (peek-char nil stream)))
    (format t "~%CH:~S" ch)
    (cond ((char= ch #\<) (read-iriref stream))
          ((char= ch #\_) (read-blank-node-label stream))
          ((char= ch #\() (read-collection stream))
          ((char= ch #\[) (read-char stream) (read-predicate-object-list stream))
          ((char= ch #\") (read-rdf-literal stream))
          ((or (char<= #\0 ch #\9) (char= ch #\-) (char= ch #\+))
           (read-numeric-literal stream))
          (t (coerce 
              (loop for c = (read-char stream)
                  until (or (ws-p c) (char= c #\,) (char= c #\;) (char= c #\.))
                  collect c)
              'cl:string)))))

(defun read-object-list (stream)
  (skipbl stream)
  (let ((object (read-object stream)))
    (skipbl stream)
    (let ((ch (peek-char nil stream)))
      (cond ((char= ch #\,)
             (read-char stream)
             (cons object (read-object-list stream)))
            (t (list object))))))

(defun read-predicate-object-list (stream)
  (let ((verb (read-verb stream)))
    (let ((objectList (read-object-list stream)))
      (skipbl stream)
      (let ((ch (peek-char nil stream)))
        (cond ((char= ch #\;)
               (read-char stream)
               (cons (cons verb objectList) (read-predicate-object-list stream)))
              (t (list (cons verb objectList))))))))

(defun read-triple (stream)
  ;; triples ::= subject predicateObjectList | blankNodePropertyList predicateObjectList? 
  (skipbl stream)
  (let ((ch (peek-char nil stream))
        subject pred-obj-list)
    (setq subject
          (cond ((char= ch #\<) (read-iriref stream))
                ((char= ch #\_) (read-blank-node-label stream))
                ((char= ch #\[) (read-char stream) (read-predicate-object-list stream))
                ((char= ch #\() (read-collection stream))
                ((error "Illegal subject."))))
    (setq pred-obj-list (read-predicate-object-list stream))
    (cons subject pred-obj-list)))

(defun read-term (stream)
  (let ((term
         (coerce 
          (loop for c = (read-char stream)
              until (ws-p c)
              collect c)
          'cl:string)))
    term))

(defun read-pname-ns (stream)
  (skipbl stream)
  (let ((term
         (coerce 
          (loop for c = (read-char stream)
              until (or (char= c #\:) (ws-p c))
              collect c)
          'cl:string)))
    term))

(defun read-turtle (stream)
  (let ((ch (peek-char t stream)))
    (cond ((char= ch #\Newline)
           (read-line stream)) ; discard Newline
          ((char= ch #\#)      ; comment in turtle
           (read-line stream)) ; discard one line
          ((char= ch #\@)
           (let ((term (read-term stream)))
             (cond ((string= term "@prefix")
                    (let ((pnamens (read-pname-ns stream)))
                      (let ((iriref (read-iriref stream)))
                        (read-period stream)
                        (read-line stream)
                        (let (pkg)
                          (cond ((setq pkg (find-package pnamens))
                                 (assert (string= (documentation pkg t) iriref)))
                                (t (setq pkg (make-package pnamens))
                                   (let ((prefixiri (gx:set-uri-namedspace (gx:iri iriref))))
                                     (setf (uri-namedspace-package prefixiri) pkg)
                                     (setf (documentation pkg t) pnamens))))))))
                   ((string= term "@base")
                    (let ((iriref (read-iriref stream)))
                      (setq gx:*base-uri* (gx:set-uri-namedspace (gx:iri iriref)))))
                   ((error "Illegal directive")))))
          (t (read-triple stream)))))

(defun read-turtle-file (accepter-fun &optional (file (ask-user-rdf-file)))
  (unless file (return-from read-turtle-file nil))
  (with-open-file (stream (pathname file) :external-format (excl:find-external-format :utf-8))
    (let ((*line-number* 1)
          (*line-pos* 0)
          (*pos* 0)
          (*default-namespace* (or *default-namespace*
                                   (cond ((stringp file) file)
                                         ((pathnamep file) (namestring file))
                                         ((error "Cant happen!")))))
          (*base-uri* *base-uri*)
          (reader (make-turtle-reader stream))
          (accept (make-accepter-for-turtle accepter-fun)))
      (catch 'turtle-completed
             (loop (funcall accept reader)))))
  (let ((refered (set-difference *referenced-resources* *defined-resources* :key #'car)))
    ;(warn "Defined resources: ~{~S ~}" *defined-resources*)
    ;(warn "Referenced resources: ~{~S ~}" *referenced-resources*)
    (when refered
      (warn "REFERENCED BUT NOT DEFINED RESOURCES: ~{~S ~}" refered)))
  :done)

(defun make-turtle-reader (stream)
  #'(lambda (writer)
      (labels ((read-loop (writer)
                 (let ((ch (peek-char t stream nil :eof)))
                   (cond ((eq ch :eof)
                          (throw 'turtle-completed t))
                         ((char= ch #\#)      ; comment in turtle
                          (funcall writer
                                   (read-line stream)
                                   #'(lambda (wrtr) (read-loop wrtr))))
                         ((char= ch #\@)
                          (let ((term (read-term stream)))
                            (cond ((string= term "@prefix")
                                   (let ((pnamens (read-pname-ns stream)))
                                     (let ((iriref (read-iriref stream)))
                                       (read-period stream)
                                       (read-line stream)
                                       (funcall writer
                                                `(@prefix ,pnamens ,iriref)
                                                #'(lambda (wrtr) (read-loop wrtr)))
;;;                                       (let (pkg)
;;;                                         (cond ((setq pkg (find-package pnamens))
;;;                                                (assert (string= (documentation pkg t) iriref)))
;;;                                               (t (setq pkg (make-package pnamens))
;;;                                                  (let ((prefixiri (gx:set-uri-namedspace (gx:iri iriref))))
;;;                                                    (setf (uri-namedspace-package prefixiri) pkg)
;;;                                                    (setf (documentation pkg t) pnamens)))))
                                       )))
                                  ((string= term "@base")
                                   (let ((iriref (read-iriref stream)))
                                     (read-period stream)
                                     (read-line stream)
                                     (funcall writer
                                              `(@base ,iriref)
                                              #'(lambda (wrtr) (read-loop wrtr)))
;;;                                     (setq gx:*base-uri* (gx:set-uri-namedspace (gx:iri iriref)))
                                     ))
                                  ((error "Illegal directive")))))
                         (t (funcall writer
                                 (read-triple stream)   ; list form for turtle
                                 #'(lambda (wrtr) (read-loop wrtr))))))))
        (read-loop writer))))

(defun make-accepter-for-turtle (accepter-fun)
  "This function creates a creater of a consumer function in producer-consumer model.
   The returned function must take a producer."
  (let ((*autoepistemic-local-closed-world* nil))
    #'(lambda (reader)
        (labels ((accept-loop (element reader)
                              (cond ((null element)     ; initial calling and null attributes
                                     (funcall reader #'(lambda (elm rdr) (accept-loop elm rdr))))
                                    ((consp element)
                                     (let ((*base-uri* *base-uri*)
                                           (*default-namespace* *default-namespace*))
                                       (funcall accepter-fun element)))
                                    ((stringp element)  ; comment
                                     nil)
                                    
                                    ((error "Can't happen")))))
          (accept-loop nil reader)))))

#|
(in-package :ttl)
(read-turtle-file #'print "../../allegro-projects/Ontologies/qb/cube.ttl")
|#
#|

(defun read-resource (instr line)
  ;; this also reads @prefix, @base, and ?x as lisp symbol.
  (cond ((char= (my-peek-char t instr) #\<)
         ;; read uriref
         (let ((uriref
                (single-angle-bracket-reader instr (getnext-char instr))))
           (unless (net.uri:uri-p uriref)
             (warn (warn "Syntax error: illegal uriref in ~S" line))
             (signal (make-instance 'trepl-newline)))
           uriref))
        (t ;; otherwise qname
         (let ((qname (read-qname instr line)))
           (cond ((eq qname instr)
                  (warn "Syntax error: resource missing in ~S" line)
                  (signal (make-instance 'trepl-newline)))
                 (t qname))))))

(defun read-prefix (instr line)
  (let ((prefix (read-prefixName instr)))
    (unless prefix
      (warn "Syntax error: illegal prefix in ~S" line)
      (signal (make-instance 'trepl-newline)))
    prefix))

(defun read-nodeID (instr line)
  (let ((id (read instr nil instr)))
    (when (eq id instr)
      (warn "Syntax error: nodeID missing in ~S" line)
      (signal (make-instance 'trepl-newline)))
    id))

(defun nameStartChar-p (ch)
  (or (char<= #\A ch #\Z)
      (char= ch #\_)
      (char<= #\a ch #\z)
      (let ((code (char-code ch)))
        (or (<= #x00C0 code #x00D6)
            (<= #x00D8 code #x00F6)
            (<= #x00F8 code #x02FF)
            (<= #x0370 code #x037D)
            (<= #x037F code #x1FFF)
            (<= #x200C code #x200D)
            (<= #x2070 code #x218F)
            (<= #x2C00 code #x2FEF)
            (<= #x3001 code #xD7FF)
            (<= #xF900 code #xFDCF)
            (<= #xFDF0 code #xFFFD)
            (<= #x10000 code #xEFFFF)))))

(defun nameChar-p (ch)
  (or (nameStartChar-p ch)
      (char= ch #\-)
      (char<= #\0 ch #\9)
      (let ((code (char-code ch)))
        (= code #x00B7)
        (<= #x0300 code #x036F)
        (<= #x203F code #x2040))))

(defun read-prefixName (stream)
  (let ((ch (my-peek-char t stream nil stream)))
    (when (eq ch stream)
      (return-from read-prefixName nil))
    (cond ((char= ch #\:) "")
          ((char= ch #\_)
           (getnext-char stream nil stream)
           (setq ch (my-peek-char t stream nil stream))
           (cond ((and (characterp ch) (char= ch #\:)) "_")
                 (t (ungetnext-char #\_ stream)
                    nil)))
          ((char= ch #\@)
           (concatenate 'string
             (cons (getnext-char stream)
                   (loop as ch = (getnext-char stream nil stream)
                       while (and (not (eq ch stream))
                                  (nameChar-p ch))
                       collect ch
                       finally (unless (eq ch stream)
                                 (ungetnext-char ch stream))))))
          ((char= ch #\?)
           (concatenate 'string
             (cons (getnext-char stream)
                   (loop as ch = (getnext-char stream nil stream)
                       while (and (not (eq ch stream))
                                  (nameChar-p ch))
                       collect ch
                       finally (unless (eq ch stream)
                                 (ungetnext-char ch stream))))))
          ((and (nameStartChar-p ch) (not (char= ch #\_)))
           (concatenate 'string
             (cons (getnext-char stream)
                   (loop as ch = (getnext-char stream nil stream)
                       while (and (not (eq ch stream))
                                  (nameChar-p ch))
                       collect ch
                       finally (unless (eq ch stream)
                                 (ungetnext-char ch stream))))))
          (t nil))))

(defun read-qname (stream line)
  "reads a qname string from <stream>. This function may returns a null string."
  (skipbl stream)
  (let ((prefix (read-prefixName stream)) ; may be "", "@prefix", "?x"
        (localn nil))
    (cond ((null prefix)
           (warn "Syntax error: illegal prefix in ~S" line))
          ((string= prefix "a") 'rdf:type)
          ((string= prefix "@prefix") (intern prefix))
          ((string= prefix "@base") (intern prefix))
          ((string= prefix "read") (intern prefix))
          ((char= (char prefix 0) #\?) (intern prefix))
          ((not (eq (getnext-char stream nil stream) #\:))
           (warn "Syntax error: illegal qname in ~S" line))
          ((null (setq localn (read-name stream)))
           (warn "Syntax error: illegal qname in ~S" line))
          (t (cond ((zerop (length prefix)) ; prefix = ""
                    (multiple-value-bind (symbol status)
                        (intern localn (cond (*base-uri* (gx:uri2package *base-uri*))
                                             (t *package*)))
                      (when (or (null status) (eq status :internal))
                        (export symbol))
                      symbol))
                   ((find-package prefix)
                    (multiple-value-bind (symbol status)
                        (intern localn (find-package prefix))
                      (when (or (null status) (eq status :internal))
                        (export symbol (find-package prefix)))
                      symbol))
                   (t (warn "No prefix definition: ~A" prefix)
                      (signal (make-instance 'trepl-newline)))
                   )))))

(defun read-name (stream)
  "reads a local name string from <stream>."
  (let ((ch (my-peek-char t stream nil stream)))
    (when (eq ch stream)
      (return-from read-name nil))
    (cond ((nameStartChar-p ch)
           (concatenate 'string
             (cons (getnext-char stream)
                   (loop as ch = (getnext-char stream nil stream)
                       while (and (not (eq ch stream))
                                  (nameChar-p ch))
                       collect ch
                       finally (unless (eq ch stream)
                                 (ungetnext-char ch stream))))))
          (t nil))))

(defun read-question (stream line)
  (skipbl stream)
  (unless (char= (getnext-char stream nil nil) #\?)
    (warn "Syntax error: the period missing in ~S" line)))

(defun read-semicolon (stream line)
  (skipbl stream)
  (unless (char= (getnext-char stream nil nil) #\;)
    (warn "Syntax error: the period missing in ~S" line)))

(defun single-angle-bracket-reader (stream char)
  (let ((nc (peek-char cl:nil stream t cl:nil t)))
    (cond ((relativeURI-p nc)
           (let* ((uri-str 
                   (coerce 
                    (loop with char until (char= #\> (setq char (getnext-char stream))) collect char)
                    'cl:string))
                  (uri (gx:uri uri-str)))
             uri))
          (t (excl::read-token stream char)
             ))))

|#