;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;
;; N-triple parser module
;;
;; IT Program Project in Japan: 
;;    Building Operation-Support System for Large-scale System using IT
;;
;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;; for the realization of the MEXT IT Program in Japan,
;;
;; Copyright © 2003, by Galaxy Express Corporation
;;
;; History
;; -------
;; 2004.12.10    N-Triple parsing modified adapting <http://www.w3.org/TR/2004/REC-rdf-testcases-20040210/>
;; 2003.05.10    File created

(defpackage :gx
    (:export read-NTriple-file addTriple-from-file))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :uri)
  ) ; end of eval-when

(in-package :gx)

;;
;; Name space is mapped to a lisp package.
;;

(defun find-package-from-documentation (resource)
  (find resource (list-all-packages) :key #'documentation :test #'equalp))

(defun find-package-from-uri (resource)
  (when (stringp resource) (setq resource (net.uri:parse-uri resource)))
  (find resource (list-all-packages)
        :key #'(lambda (pkg) (net.uri:parse-uri (documentation pkg t)))
        :test #'(lambda (u1 u2) (and (string= (net.uri:uri-host u1) (net.uri:uri-host u2))
                                     (string= (net.uri:uri-path u1) (net.uri:uri-path u2))))))

;(defun find-package-from-uri-host (resource)
;  (when (stringp resource)
;    (setq resource (net.uri:parse-uri resource)))
;  (let ((host (net.uri:uri-host resource)))
;    (find resource (list-all-packages)
;          :key #'(lambda (pkg) (net.uri:uri-host (net.uri:parse-uri (documentation pkg t))))
;          :test #'equalp)))

(declaim (inline label-localpart))
(defun label-localpart (symbol)
  (symbol-name symbol))

(declaim (inline label-prefix))
(defun label-prefix (symbol)
  (package-name (symbol-package symbol)))

(declaim (inline label-uri))
(defun label-uri (symbol)
  (documentation (symbol-package symbol) t))

;;
;; NT Format File
;;

#|
matching the production Language-Tag in Section 2.1 of [RFC 3066]. 
Note: This EBNF cannot perform the counting required by the Primary-subtag and Subtag productions.  
string ::= character* with escapes as defined in section Strings  
absoluteURI ::= character+ with escapes as defined in section URI References  
|#

(defun escaped-p (str pos)
  (cond ((= pos 0) nil)
        ((and (char= (char str (1- pos)) #\\)
              (not (escaped-p str (1- pos))))
         t)))

(defun position-with-escape (char str i)
  (let ((pos (position char str :start i)))
    (cond ((and (numberp pos) (escaped-p str pos))
           (position-with-escape char str (1+ pos)))
          (t pos))))

(defun find-with-escape (char str i)
  (let ((pos (position char str :start i)))
    (cond ((and (numberp pos) (escaped-p str pos))
           (find-with-escape char str (1+ pos)))
          (t (char str pos)))))

(defun ws-p (char)
  (declare (optimize (speed 3) (safety 1)))
  "ws ::= space | tab"
  (or (char= char (code-char #x20)) (char= char (code-char #x9))))

(defun eoln-p (char)
  (declare (optimize (speed 3) (safety 1)))
  "eoln ::= cr | lf | cr lf"
  (or (char= char (code-char #xD))
      (char= char (code-char #xA))
      (char= char #\Newline)
      ))

(defun character-p (char)
  (declare (optimize (speed 3) (safety 1)))
  "character ::= [#x20-#x7E] /* US-ASCII space to decimal 126 */ "
  (let ((code (char-code char)))
    (<= #x20 code #x7E)))

(defun name-char-p (char)
  (declare (optimize (speed 3) (safety 1)))
  (or (alpha-char-p char) (digit-char-p char)))

(defun lower-name-char-p (char)
  (declare (optimize (speed 3) (safety 1)))
  (or (lower-case-p char) (digit-char-p char)))

(defun skip-ws (str i)
  (or (position-if-not #'ws-p str :start i) (length str)))

(defun name-p (str i)
  (alpha-char-p (char str i)))
(defun parse-name (str i)
  "name ::= [A-Za-z][A-Za-z0-9]* "
  (values (subseq str i (setq i (position-if-not #'name-char-p str :start i)))
          (or i (length str))))

;; ---------------------------------------
;;  Extended N-triples

(defun QName-p (str i)
  (name-p str i))
(defun parse-QName (str i)
  (let ((Prefix nil)
        (LocalPart nil))
    (multiple-value-setq (Prefix i) (parse-name str i))
    (cond ((char= #\: (char str i))
           (incf i)
           (multiple-value-setq (LocalPart i) (parse-name str i))
           (values (cl:concatenate 'string Prefix ":" LocalPart)
                   i))
          (t (values Prefix i)))))

(defun name&space (QName)
  (cond ((find #\: QName)
         (multiple-value-bind (Prefix i) (parse-name QName 0)
           (assert (char= (char QName i) #\:))
           (values (parse-name QName (1+ i)) Prefix)))
        (t (values QName nil))))

;; ---------------------------------------

(defun ID-p (str i)
  (and (char= (char str i) #\_) (char= (char str (1+ i)) #\:) (name-p str (+ i 2))))
(defun parse-nodeID (str i)
  "nodeID ::= '_:' name"
  (let ((name nil))
    (multiple-value-setq (name i) (parse-name str (+ i 2)))
    (values (concatenate 'string "_:" name) i)))

(defun language-p (str i)
  "language ::= [a-z]+ ('-' [a-z0-9]+ )* "
  (and (lower-case-p (char str i))
       (%language-p str (1+ i))))
(defun %language-p (str i)
  (let ((pos (position-if-not #'(lambda (c) (or (lower-name-char-p c) (char= c #\-)))
                              str :start i :end (length str))))
    (or (null pos)              ; eaten up
        (not (zerop pos)))))    ; 1 char at least
(defun parse-language (str i)
  (cond ((lower-case-p (char str i))
         (let ((pos (or (position-if-not #'(lambda (c) (or (lower-name-char-p c) (char= c #\-)))
                                         str :start (1+ i) :end (length str))
                        (length str))))
           (values (subseq str i pos) pos)))))

(defun uriref-p (str i)
  "uriref ::= '<' absoluteURI '>'"
  (and (char= (char str i) #\<) (find-with-escape #\> str i)))
(defun parse-iriref (str i)
  "uriref ::= '<' absoluteURI '>'"
  (values (subseq str i (setq i (1+ (position-with-escape #\> str i))))
          i))

(defun langString-p (str i)
  "langString ::= '\"' string '\"' ( '@' language )?"
  (and (char= (char str i) #\")
       (numberp (setq i (position-with-escape #\" str (1+ i))))
       (incf i) ; next of #\"
       (cond ((= i (length str)) t)
             ((and (< i (length str)) (char= (char str i) #\@) (language-p str (1+ i))))
             ((and (< i (length str)) (char= (char str i) #\^) (char= (char str (1+ i)) #\^) (uriref-p str (+ i 2)))
              nil)
             (t t))))
(defun parse-langString (str i)
  "langString ::= '\"' string '\"' ( '@' language )?"
  (let ((string (subseq str i (setq i (1+ (position-with-escape #\" str (1+ i))))))) ; " to "
    (cond ((and (< i (length str)) (char= (char str i) #\@) (language-p str (1+ i)))
           (let ((lang nil))
             (multiple-value-setq (lang i) (parse-language str (1+ i)))
             (values (concatenate 'string string "@" lang) i)))
          (t (values string i)))))

(defun datatypeString-p (str i)
  "datatypeString ::= '"' string '"' '^^' uriref "
  (and (char= (char str i) #\")
       (numberp (setq i (position-with-escape #\" str (1+ i))))
       (incf i) ; next of #\"
       (and (< (1+ i) (length str)) (char= (char str i) #\^) (char= (char str (1+ i)) #\^)
            (uriref-p str (+ i 2)))))
(defun parse-datatypeString (str i)
  "datatypeString ::= '"' string '"' '^^' uriref "
  (let ((string (subseq str i (setq i (1+ (position-with-escape #\" str (1+ i)))))))
    (multiple-value-bind (iri i) (parse-iriref str (+ i 2))
      (values (concatenate 'string string "^^" iri) i))))

(defun literal-p (str i)
  (or (langString-p str i) (datatypeString-p str i)))
(defun parse-literal (str i)
  "literal ::= langString | datatypeString"
  ;; Following order is very important. First datatypeString, second langString.
  (cond ((datatypeString-p str i) (parse-datatypeString str i))
        ((langString-p str i) (parse-langString str i))
        ((error "Illegal literal."))))

(defun object-p (str i)
  "object ::= uriref | nodeID | literal  "
  (or (uriref-p str i) (ID-p str i) (literal-p str i)
      (QName-p str i) ; Seiji 2003.2.22
      ))
(defun parse-object (str i)
  (cond ((uriref-p str i) (parse-iriref str i))
        ((ID-p str i) (parse-nodeID str i))
        ((literal-p str i)(parse-literal str i))
        ((QName-p str i)  (parse-QName str i)) ; Seiji 2003.2.22
        ((error (format nil "Illegal object:~~%~~A~~%~~~DT^" i) str))))

(defun predicate-p (str i)
  "predicate ::= uriref  "
  (or (uriref-p str i)
      (QName-p str i) ; Seiji 2003.2.22
      ))
(defun parse-predicate (str i)
  "predicate ::= uriref  "
  (cond ((uriref-p str i)(parse-iriref str i))
        ((QName-p str i)  (parse-QName str i)) ; Seiji 2003.2.22
        ))

(defun subject-p (str i)
  "subject ::= uriref | nodeID  "
  (or (uriref-p str i) (ID-p str i)
      (QName-p str i) ; Seiji 2003.2.22
      ))
(defun parse-subject (str i)
  "subject ::= uriref | nodeID  "
  (cond ((uriref-p str i) (parse-iriref str i))
        ((ID-p str i) (parse-nodeID str i))
        ((QName-p str i)  (parse-QName str i)) ; Seiji 2003.2.22
        ((error "Illegal subject:~%~A" str))))

(defun parse-triple (str i)
  "triple ::= subject ws+ predicate ws+ object ws* '.' ws*  "
  (let (subject predicate object)
    (multiple-value-setq (subject i) (parse-subject str i))
    (setq i (skip-ws str i))
    (multiple-value-setq (predicate i) (parse-predicate str i))
    (setq i (skip-ws str i))
    (multiple-value-setq (object i) (parse-object str i))
    (setq i (skip-ws str i))
    (assert (char= (char str i) #\.) nil (format nil "No period in line: ~~A~~%~~~DT^" (+ i 7))  str)
    (list subject predicate object)))

(defun nt-comment-p (str i)
  (char= (char str i) #\#))
(defun parse-nt-comment (str i)
  "comment ::= '#' ( character - ( cr | lf ) )*  "
  (incf i)
  (list
   (subseq str i (position-if #'(lambda (c) (or (char= c #\Linefeed) (char= c #\Return)))
                              str :start i))
   nil
   nil))

(defun parse-triple-line (line)
  "line ::= ws* ( comment | triple )? eoln  "
  (when (zerop (length line)) (return-from parse-triple-line (list nil nil nil)))
  (let ((ln (1- (length line))) ; decreace 1 for eoln
        (i (skip-ws line 0)))
    ;; The order is very important.
    (cond ((<= ln i) (list nil nil nil))
          ((nt-comment-p line i) (parse-nt-comment line i))
          (t (parse-triple line i)))))

(defun read-ntripleDoc (stream process-fn)
  "ntripleDoc ::= line*  "
  (loop for line = (read-line stream nil nil)
      while line do (restart-case (apply process-fn (parse-triple-line line))
                      (skip-ntriple-line () nil)))
  :done)

(defun ask-user-nt-file ()
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("NTriple format file" . "*.nt")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%NTriple format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))

(defun read-NTriple-file (process-fn &optional (file (ask-user-nt-file)) (code :default))
  (unless file (return-from read-NTriple-file nil))
  (with-open-file (stream (pathname file) :external-format (excl:find-external-format code))
    (read-ntripleDoc stream process-fn)))

(defun rdf-parser-test (subject predicate object)
  (cond ((and (null subject) (null predicate) (null object))
         (format *standard-output* "~%Null line."))
        ((and (null predicate) (null object))
         (format *standard-output* "~%Comment line:~A" subject))
        (t (format *standard-output* "~%~S ~S ~S" subject predicate object))))

#|
(read-NTriple-file #'gx::rdf-parser-test)
(read-NTriple-file #'addTriple-from-file)
> (get-form eg:Proposal)
> (<- eg:Proposal eg:author eg:name)
> (<- eg:Proposal dc:title)
> (<- eg:Proposal eg:author rdf:type)
|#

;; End of module
;; --------------------------------------------------------------------

(provide :ntparser)
