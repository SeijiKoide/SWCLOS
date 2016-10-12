;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; Rdf I/O module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code was encoded by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002, 2004 
;;;    Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2010
;;;    Seiji Koide
;;
;; History
;; -------
;; 2013.03.10    get-line is added for turtlparser.
;; 2007.08.18    Rdf I/O module is separated.
;; 2005.12.09    OntologySpace and Ontology definition is added.
;; 2004.07.07    Rdf2Sexpr is separated from here.
;; 2004.07.07    Rdf2Nt is separated from here.
;; 2004.01.09    parse-rdf for RDF parser
;; 2002.08.15    File created
;;; ==================================================================================

(in-package :gx)

(export '(*line-number* *line-pos* *pos* expose-buf skipbl read-pattern-p skip-pattern 
           match-pattern-p peeknext-char getnext-char get-line))

;;;
;;; The line number of input stream is counted and used in error messages.
;;;

(defvar *line-number* 0 "line number, starting from one, zero is just a flag that indicates no RDF/XML parser input.")
(defvar *line-pos* 0 "current line position, starting from zero")
(defvar *pos* 0 "current position, starting from zero")

(defun line-count (stream)
  (declare (ignore stream))
  *line-number*)

;;;
;;;; RDF input functions are hidden from user's view.
;;;
;;; To parsing XML documents, dedicated input functions are developed. 
;;; The motivation of this development is as follows. The transition machine is usually modeled 
;;; for parsing the syntax of languages. However, the expression of the syntax specification of a 
;;; particular language and the transition machine for the language is very different and difficult 
;;; to imagine one to another. On the other hand, Ratfor and C (maybe) language need only one 
;;; character peeking to parse them. Generally, any number of peeking allows us to parse any context 
;;; free language. For XML parsing, in the experience, nine peeking characters is enough to parse 
;;; XML for "\<![CDATA[", maybe. 
;;;
;;; These special functions allow programmers to peek any numbers of characters in the stream.
;;; These functions are very useful to parse XML documents without state transition mechanism. 
;;; We can decide what kind of data comes up next by peeking a number of characters. 
;;;
;;; If you want to show what characters remain in the buffer, use the command <expose-buf> without 
;;; parameters. The buffer has two pointers to the start and the end of sequence of characters.
;;; * ((#\null) #\null) indicates null list.
;;; * ((a b c #\null) #\null) indicates three characters 'a', 'b', and 'c' in buffer.

(let ((xlist (cons (list #\null) nil)))
  (rplacd xlist (car xlist))
  (labels ((xnull () (char= #\null (caar xlist)))
           (xpop () (cond ((char= #\null (caar xlist)) nil)
                          (t (prog1 (caar xlist)
                               (setf (car xlist) (cdar xlist))))))
           (xpush (c) (setf (car xlist) (cons c (car xlist))))
           (xput (c) (let ((cons (cons (cadr xlist) (cddr xlist))))
                       (rplaca (cdr xlist) c)
                       (rplacd (cdr xlist) cons)
                       (rplacd xlist (cddr xlist))
                       c)))
    (defun expose-buf () xlist)
    (defun flush-buf ()
      (declare (optimize (speed 3) (safety 1)))
      (setq xlist (cons (list #\null) nil))
      (rplacd xlist (car xlist)))
    (defun putback-char (char stream)
      (declare (ignore stream) (optimize (speed 3) (safety 1)))
      (xpush char)
      (decf *pos*)
      (when (char= char #xA) (decf *line-number*))
      char)
    (defun putback-pattern (pattern stream)
      (declare (ignore stream) (optimize (speed 3) (safety 1)))
      (decf *pos* (length pattern))
      (loop for char across (reverse pattern)
          do (xpush char)))
    (defun peeknext-char (stream)
      (declare (optimize (speed 3) (safety 1)))
      (cond ((xnull)
             (let ((c (read-char stream)))
               (xput c)))
            (t (caar xlist))))
    (defun getnext-char (stream)
      (declare (optimize (speed 3) (safety 1)))
      (incf *pos*)
      (or (xpop)
          (let ((c (read-char stream)))
            (cond ((char= c #\Newline)
                   (setq *line-pos* *pos*)
                   (incf *line-number*)
                   c)
                  ((eq (char-code c) #xD)             ; Allegro reading function resolves the newline implementation difference, if cr-lf directed.
                   (cond ((eq (char-code (peek-char nil stream)) #xA) ; but maybe cr-lf is not directed, and it is.
                          (read-char stream)
                          (code-char #xA))
                         (t (error "Check Newline code"))))
                  ;((eq (char-code c) #xD)
                  ; (let ((nc (read-char stream)))
                  ;   (cond ((eq (char-code nc) #xA))  ; eat up by XML 1.1 spec
                  ;         ((eq (char-code nc) #x85)) ; eat up by XML 1.1 spec
                  ;         (t (xput nc))))            ; don't eat
                  ; (code-char #xA))                   ; internal newline by XML 1.1 spec
                  (t c)))))
    (defun match-pattern-p (pattern stream)
      (declare (optimize (speed 3) (safety 1)))
      (loop for i fixnum from (length (car xlist)) to (length pattern)
          do (xput (read-char stream)))
      (loop for char1 of-type character across (the simple-string pattern)
          for char2 of-type character in (car xlist)
          always (char= char1 char2)))
    (defun skip-pattern (pattern stream)
      (declare (ignore stream) (optimize (speed 3) (safety 1)))
      (incf *pos* (length pattern))
      (loop for i fixnum from 1 to (length pattern) do (xpop)))
    (defun read-pattern-p (pattern stream)
      (declare (optimize (speed 3) (safety 1)))
      (loop for char1 of-type character across (the simple-string pattern)
          for char2 of-type character = (getnext-char stream)
          always (char= char1 char2)))
    ;;[3]    S    ::=    (#x20 | #x9 | #xD | #xA)+ 
    (defun space-p (stream)
      (declare (optimize (speed 3) (safety 1)))
      (let ((code (char-code (peeknext-char stream))))
        (or (eq code #x20)
            (eq code #x9)
            (eq code #xD)
            (eq code #xA))))
    (defun skipbl (stream)
      (declare (optimize (speed 3) (safety 1)))
      (let ((code (char-code (peeknext-char stream))))
        (cond ((eq code #x20) (xpop) (incf *pos*) (skipbl stream))
              ((eq code #x9) (xpop) (incf *pos*) (skipbl stream))
              ((eq code #xA) (xpop) (incf *pos*) (setq *line-pos* *pos*) 
               (incf *line-number*)
               (skipbl stream))
              ((eq code #xD)
               (xpop) (incf *pos*)
               (cond ((eq (char-code (peeknext-char stream)) #xA)
                      ;; then CR+LF
                      (setq *line-pos* *pos*)
                      (xpop) (incf *pos*)
                      (incf *line-number*)
                      (skipbl stream))
                     (t (error "Check Newline!")))))))
    ))

(defun assert-pattern (pattern stream)
  "asserts that input from <stream> matches to <pattern>. 
   <pattern> is a string. This function eats up all characters 
   that equal to pattern. In case of mismatch, an error occurs."
  (or (read-pattern-p pattern stream)
      (error "Assertion error for pattern: ~A" pattern)))

(defun read-quoted-string (stream)
  "reads a quoted string from <stream>."
  (let ((q (getnext-char stream)))
    (assert (or (char= q #\") (char= q #\')))
    (coerce 
     (loop for c = (getnext-char stream)
         until (char= c q)
         collect c)
     'cl:string)))

(defun parse-pattern-delimited-string (pattern stream)
  "reads input character from <stream> until the occurence of
   <pattern> and returns it as string. <pattern> is not eaten.
   one character at least should stand before <pattern>."
  (coerce
   (loop until (match-pattern-p pattern stream)
       for c = (getnext-char stream)
       do (when (char= c #\Newline) (incf *line-number*))
       collect c)
   'cl:string))

(defun get-line (stream)
  (coerce
   (loop for c = (getnext-char stream)
       until (char= c #\Newline)
       collect c
       finally (incf *line-number*))
   'cl:string))

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Sep-13-2008
;;;

(cl:provide :rdfio)
