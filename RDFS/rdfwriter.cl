;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; RDFWriter module
;;;
;;; IT Program Project in Japan: 
;;;          Building Operation-Support System for Large-scale System using IT.
;;;
;;; This code is written by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan,
;;;
;;; Copyright (c) 2004 by Galaxy Express Corporation
;;; 
;;; Copyright (c) 2008 Seiji Koide
;;
;; History
;; -------
;; 2008.12.11    resource-p is renamed to rdf-object-p.
;; 2008.01.06    Revised
;; 2004.07.23    File created
;;
;;; ==================================================================================

(provide :rdfwriter)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :gxutils)
  (require :gxforwardref)
  )

(in-package :gx)

(export '(write-resource write-xml *force-recursive-p*
          write-rdf-all-entities-in write-xml-all-entities-in))

;;;
;;;; RDF Writer
;;;

(defvar *force-recursive-p* nil)

(defun dont-expand-p (resource)
  (and (not *force-recursive-p*)
       (cl:typep resource rdfs:|Resource|)
       (or (and (slot-boundp resource 'rdf:|about|) (slot-value resource 'rdf:|about|))
           (and (name resource) (not (nodeID? (name resource)))))))

(defun collect-used-packaged-from (x)
  (remove-duplicates
   (loop for resource in x
       with name and slots and cls
       do (setq cls (class-of resource))
         (setq name (name resource))
         (setq slots (collect-instance-slots resource))
       append (append ;(when (and (name cls) (not (nodeID-p cls))) (list (symbol-package (name cls))))
               (when (and (name cls) (not (eq (name cls) '|rdfs:Resource|)))
                 (list (symbol-package (name cls))))
               (when (and name (not (nodeID? name))) (list (symbol-package name)))
               (%collect-used-packaged-from slots)))
   :from-end t))

(defun %collect-used-packaged-from (slots)
  (loop for (role . forms) in slots
      append (cons (symbol-package role)
                   (loop for filler in forms
                       append 
                         (typecase filler
                           (symbol (list (symbol-package filler)))
                           (rdfs:|Resource|
                            (cond (*force-recursive-p*
                                   (collect-used-packaged-from (list filler)))
                                  (t nil))))))))

(defun write-xml (x &optional (stream *standard-output*))
  (unless (listp x) (setq x (list x)))
  (let ((*print-level* nil)
        (*print-length* nil)
        (packages (remove-duplicates
                   (cons (find-package :rdf)
                         (collect-used-packaged-from x)))))
    (pprint-logical-block (stream x)
      (pprint-indent :block 0 stream)
      (cond (packages
             (princ "<rdf:RDF" stream)
             (write-char #\space stream)
             (pprint-logical-block (stream packages)
               (when *base-uri*
                 (princ "xml:base = " stream)
                 (write (render-uri *base-uri* nil) :stream stream)
                 (pprint-newline :mandatory stream))
               (loop (let ((package (pprint-pop)))
                       (assert (not (null (documentation package t))))
                       (princ "xmlns:" stream)
                       (princ (package-name package) stream)
                       (princ " = " stream)
                       (write (documentation package t) :stream stream)
                       (pprint-exit-if-list-exhausted)
                       (pprint-newline :mandatory stream))))
             
             (write-char #\space stream)
             (write-char #\> stream))
            (t (princ "<rdf:RDF>" stream)))
      (pprint-indent :block 2 stream)
      (pprint-newline :mandatory stream)
      (pprint-logical-block (stream x)
        (loop (write-resource (pprint-pop) stream)
              (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory stream)
              (pprint-newline :mandatory stream)))
      (pprint-indent :block 0 stream)
      (pprint-newline :mandatory stream)
      (princ "</rdf:RDF>" stream)
      (pprint-newline :mandatory stream)))
  (values))

(defun write-resource (resource s)
  "prints each element as resource"
  (let ((class (type-of resource))
        (about (or (and (slot-boundp resource 'rdf:|about|)
                        (slot-value resource 'rdf:|about|))
                   (and (name resource)
                        (symbol2uri (name resource)))))
        (slots (collect-instance-slots resource)))
    (when (iri-p about) (setq about (render-uri about nil)))
    (when (and about *base-uri*)
      (let* ((basestr (render-uri *base-uri* nil))
             (len (length basestr)))
        (when (and (string= basestr about :end1 len :end2 len)
                   (< len (length about)))
          (setq about (subseq about len)))))
    (setq slots (remove-if #'(lambda (slot) (null (cdr slot))) slots))
    (cond ((null slots) (write-about= resource s))
          (t (write-char #\< s)
             (cond ((eq class 'rdfs:|Resource|)
                    (write 'rdf:|Description| :stream s))
                   ((eq class '|rdfs:Resource|)
                    (write 'rdf:|Description| :stream s))
                   ((owl-restriction-p resource)
                    (princ "owl:Restriction" s))
                   ((atom class)
                    (write class :stream s))
                   (t (write (car class) :stream s)
                      (setq slots
                            (append (mapcar #'(lambda (cls) (list 'rdf:|type| (symbol-value cls)))
                                      (cdr class))
                                    slots))))
             (when about
               (princ " rdf:about=" s)
               (write-char #\" s)
               (princ about s)
               (write-char #\" s)
               (write-char #\space s))
             (write-char #\> s)
             (pprint-indent :block 2 s)
             (pprint-newline :mandatory s)
             (pprint-logical-block (s slots)
               (loop (write-slot (pprint-pop) s)
                     (pprint-exit-if-list-exhausted)
                     (pprint-newline :mandatory s))
               )
             (pprint-indent :block 0 s)
             (pprint-newline :mandatory s)
             (write-char #\< s)
             (write-char #\/ s)
             (cond ((eq class 'rdfs:|Resource|)
                    (write 'rdf:|Description| :stream s))
                   ((eq class '|rdfs:Resource|)
                    (write 'rdf:|Description| :stream s))
                   ((owl-restriction-p resource)
                    (princ "owl:Restriction" s))
                   ((atom class)
                    (write class :stream s))
                   (t (write (car class) :stream s)))
             (write-char #\> s)))))

(defun collection-p (resources)
  (and (consp resources) (cdr resources)
       (not (cl:typep (car resources) 'rdf:|inLang|))
       ))

(defun write-slot-subclassof (resource s)
  (cond ((rsc-object-p resource)
         (cond ((anonymous-p resource)
                (write-char #\< s)
                (write 'rdfs:|subClassOf| :stream s)
                (write-char #\> s)
                (pprint-indent :block 2 s)
                (pprint-newline :mandatory s)
                (pprint-logical-block (s (list resource))
                  (loop (write-resource (pprint-pop) s)
                        (pprint-exit-if-list-exhausted)
                        (pprint-newline :mandatory s)))
                (pprint-indent :block 0 s)
                (pprint-newline :mandatory s)
                (write-char #\< s)
                (write-char #\/ s)
                (write 'rdfs:|subClassOf| :stream s)
                (write-char #\> s))
               (t (write-resource= 'rdfs:|subClassOf| resource s))))
        ((object? resource)
         (write-slot-subclassof (symbol-value resource) s))
        ((error "Cant happen in WRITE-SLOT-SUBCLASSOF ~S" resource))))

(defun write-slot (slot s)
  (let ((role (car slot))
        (resources (cdr slot)))
    (case role
      ((rdf:|type| rdfs:|subPropertyOf|)
       (pprint-logical-block (s resources)
         (loop (write-resource= role (pprint-pop) s)
               (pprint-exit-if-list-exhausted)
               (pprint-newline :mandatory s))))
      (rdfs:|subClassOf|
       (pprint-logical-block (s resources)
         (loop (write-slot-subclassof (pprint-pop) s)
               (pprint-exit-if-list-exhausted)
               (pprint-newline :mandatory s))))
      (otherwise
       (cond ((collection-p resources)
              (write-char #\< s)
              (write role :stream s)
              (princ " rdf:parsetype='Collection'" s)
              (write-char #\> s)
              (pprint-indent :block 2 s)
              (pprint-newline :mandatory s)
              (pprint-logical-block (s resources)
                (loop (write-it (pprint-pop) s)
                      (pprint-exit-if-list-exhausted)
                      (write-char #\space s)
                      (pprint-newline :linear s)))
              (pprint-indent :block 0 s)
              (pprint-newline :fill s)
              (write-char #\< s)
              (write-char #\/ s)
              (write role :stream s)
              (write-char #\> s))
             (t (pprint-logical-block (s resources)
                  (loop (let ((resource (pprint-pop)))
                          (cond ((datatype-p (class-of resource))
                                 (write-datatype= role resource s))
                                ((cl:typep resource 'rdf:|inLang|)
                                 (write-char #\< s)
                                 (write role :stream s)
                                 (princ " xml:lang=" s)
                                 (write-char #\" s)
                                 (princ (lang resource) s)
                                 (write-char #\" s)
                                 (write-char #\> s)
                                 (typecase (content resource)
                                   (uri (princ (content resource) s))
                                   (rdfs:|Resource| (pprint-indent :block 2 s)
                                                  (pprint-newline :mandatory s)
                                                  (write-resource (content resource) s)
                                                  (pprint-indent :block 0 s)
                                                  (pprint-newline :mandatory s))
                                   (string (princ (content resource) s))
                                   (cons (pprint-indent :block 2 s)
                                         (pprint-newline :mandatory s)
                                         (pprint-logical-block (s (content resource))
                                           (loop (write-it (pprint-pop) s)
                                                 (pprint-exit-if-list-exhausted)
                                                 (write-char #\space s)
                                                 (pprint-newline :linear s)))
                                         (pprint-indent :block 0 s)
                                         (pprint-newline :mandatory s)
                                         )
                                   (otherwise (write (content resource) :stream s)))
                                 (write-char #\< s)
                                 (write-char #\/ s)
                                 (write role :stream s)
                                 (write-char #\> s))
                                ((iri-p resource) (write-resource= role resource s))
                                ((dont-expand-p resource) (write-resource= role resource s))
                                (t (write-char #\< s)
                                   (write role :stream s)
                                   (cond ((and (consp resource) (cdr resource))
                                          (princ " rdf:parsetype='Collection'" s)))
                                   (write-char #\> s)
                                   (pprint-indent :block 2 s)
                                   (pprint-newline :fill s)
                                   (pprint-logical-block (s (list resource))
                                     (write-it (pprint-pop) s))
                                   (pprint-indent :block 0 s)
                                   (pprint-newline :fill s)
                                   (write-char #\< s)
                                   (write-char #\/ s)
                                   (write role :stream s)
                                   (write-char #\> s))))
                        (pprint-exit-if-list-exhausted)
                        (pprint-newline :mandatory s)))
                ))))))

(defun write-it (resource s)
  (typecase resource
    (string (princ resource s))
    (number (prin1 resource s))
    (uri (write-char #\< s)
                 (render-uri resource s)
                 (write-char #\> s))
    (rdfs:|Resource| (cond ((dont-expand-p resource)
                          (write-about= resource s))
                         (t (write-resource resource s))))
    (otherwise (write resource :stream s))))

(defun write-about= (resource s)
  "prints <TYPE rdf:about='uri' >"
  (let* ((about (or (and (slot-boundp resource 'rdf:|about|)
                         (slot-value resource 'rdf:|about|))
                    (and (name resource)
                         (symbol2uri (name resource))))))
    (when (iri-p about) (setq about (render-uri about nil)))
    (when (and about *base-uri*)
      (let* ((basestr (render-uri *base-uri* nil))
             (len (length basestr)))
        (when (and (string= basestr about :end1 len :end2 len)
                   (< len (length about)))
          (setq about (subseq about len)))))
    (write-char #\< s)
    (write (type-of resource) :stream s)
    (princ " rdf:about=" s)
    (write-char #\" s)
    (princ about s)
    (write-char #\" s)
    (write-char #\space s)
    (write-char #\/ s)
    (write-char #\> s)))

(defun write-datatype= (role resource s)
  "prints <ROLE rdf:|datatype|='type' >value</ROLE>"
  (let ((type (class-name (class-of resource)))
        (value (value-of resource)))
    (write-char #\< s)
    (write role :stream s)
    (princ " rdf:|datatype|=" s)
    (write-char #\" s)
    (princ (symbol2uri type) s)
    (write-char #\" s)
    (write-char #\> s)
    (write value :stream s)
    (write-char #\< s)
    (write-char #\/ s)
    (write role :stream s)
    (write-char #\> s)))

(defun write-resource= (role resource s)
  "prints <ROLE rdf:resource='uri' />"
  (let* ((about (cond ((iri-p resource) resource)
                      ((slot-boundp resource 'rdf:|about|)
                       (slot-value resource 'rdf:|about|))
                      ((name resource) (symbol2uri (name resource))))))
    (setq about
          (cond ((stringp about) (slot-value about 'rdf:|about|))
                ((iri-p about) (render-uri about nil))
                ((error "Cant happen!"))))
    (when (and about *base-uri*)
      (let* ((basestr (render-uri *base-uri* nil))
             (len (length basestr)))
        (when (and (string= basestr about :end1 len :end2 len)
                   (< len (length about)))
          (setq about (subseq about len)))))
    (write-char #\< s)
    (write role :stream s)
    (princ " rdf:resource=" s)
    (write-char #\" s)
    (princ about s)
    (write-char #\" s)
    (write-char #\space s)
    (write-char #\/ s)
    (write-char #\> s)))

;;
;; LISP to RDF
;; 

(defun lisp2rdf (&optional (infile (ask-user-cl-file)))
  (with-open-file (instr infile :direction :input)
    (with-open-file (outstr "Koide.rdf" :direction :output)
      (S2RDF  instr outstr)))
  :done)

(defun ask-user-cl-file ()
  "asks an common lisp file to user."
  #+:common-graphics
  (cg:ask-user-for-existing-pathname
   "" :allowed-types '(("Lisp format file" . "*.cl")
                       ("Any file" . "*.*")))
  #-:common-graphics
  (progn
    (format t "~%Lisp format file name? ")
    (let ((filename (read-line t)))
      (if (zerop (length filename)) nil filename))))

(defun lisp2rdf* (&optional (infile  (ask-user-cl-file)))
  (with-open-file (instr infile :direction :input)
    (S2RDF  instr t)))

(defun S2RDF (instream outstream)
  (let ((entities nil))
    (setq entities 
          (loop for entity = (read-entity instream)
                until (eq entity :eof)
              collect entity))
    (setq entities (remove-if #'null entities))
    (setq entities (remove-if-not #'boundp entities))
    ;(setq entities (remove-if-not #'rsc-object-p entities))
    ;(format t "%~S" entities)
    (write-xml (mapcar #'symbol-value entities) outstream)
    ))

(defun read-entity (instream)
  (let ((form (read instream nil :eof)))
    ;(format t "~%~S" form)
    (cond ((eql form :eof) (return-from read-entity :eof))
          (t (case (car form)
               (defIndividual (second form))
               (defConcept    (second form))
               (defProperty   (second form))
               (otherwise nil))))))

(defun collect-instance-slots (instance)
  "returns instance slots with slot form (role filler1 ...)"
  (loop for slot in (mop:class-slots (class-of instance))
      with role and filler 
      when (and (cl:typep slot 'Property-effective-slot-definition)
                (slot-boundp instance (setq role (mop:slot-definition-name slot)))
                (slot-value instance role))
      collect (cond ((listp (setq filler (slot-value instance role)))
                     (cons (mop:slot-definition-name slot) filler))
                    (t (list role filler)))))

(defun write-rdf-all-entities-in (package &optional (stream-or-file *standard-output*))
  (if (streamp stream-or-file)
      (write-xml (mapcar #'symbol-value (list-all-entities-in package)) stream-or-file)
    (with-open-file (outstream stream-or-file :direction :output :if-exists :supersede)
      (write-xml (mapcar #'symbol-value (list-all-entities-in package)) outstream))))

(defun write-xml-all-entities-in (package &optional (stream-or-file *standard-output*))
  (cond ((streamp stream-or-file)
         (format stream-or-file "<?xml version=\"1.0\" ?>~%")
         (write-xml (mapcar #'symbol-value (list-all-entities-in package)) stream-or-file))
        (t (with-open-file (outstream stream-or-file :direction :output :if-exists :supersede
                                      :external-format (excl::find-external-format "UTF-8"))
             (format outstream "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>~%")
             (write-xml (mapcar #'symbol-value (list-all-entities-in package)) outstream)))))

;; End of module
;; --------------------------------------------------------------------
