;;;-*- Mode: common-lisp; syntax: common-lisp; package: gx; base: 10 -*-
;;;
;;;; NameSpace module
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This module is separated from RDFShare module for more efficient modularity.
;;;
;;; Copyright (c) 2002, 2004 by Galaxy Express Corporation
;;;
;;; Copyright (c) 2007, 2008, 2010, 2014 Seiji Koide
;;;
;; History
;; -------
;; 2014.05.21    Find-package-from-namespace is defined.
;; 2009.09.10    Some functions are rearanged for turtle.
;; 2008.12.11    Structure resource is introduced.
;; 2008.09.11    This file is created and the content is moved from RdfShare module.
;; 2008.09.10    The definition of duration is moved here from the file duration.
;; 2008.08.12    Revised based on http://www.w3.org/TR/2004/REC-rdf-syntax-grammar-20040210/
;; 2007.12.18    RdfShare is separated from Rdf module in order to share routines with RDFGate program
;;; ==================================================================================

(cl:provide :namespace)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (cl:require :swclospackages)
  (cl:require :iri)
  ) ; end of eval-when

(cl:defpackage :gx
  (:shadow parse-uri type typep value typep)
  (:import-from :net.uri render-uri uri-fragment copy-uri uri-scheme)
  (:use :common-lisp :net.uri)
  (:export iri iri-p iri-value set-uri-namedspace set-uri-namedspace-from-pkg get-uri-namedspace
           uri-namedspace uri2package uri2env uri2symbol irregular-name&pkg export-as-QName 
           *base-uri* *default-namespace* symbol2uri name-ontology nodeID? nodeID2symbol 
           *uri2symbol-name-mapping-fun* *uri2symbol-package-mapping-fun*
           find-package-from-namespace)
  )

(in-package :gx)

;;;
;;;; URI in SWCLOS and Turtle System
;;;
;;;; URI APIs fixes
;;;
;;; Note that method <gx:iri> always interns <thing>, but <gx:parse-iri> does not intern it.
;;;

(defun parse-iri (thing &rest args)
  "when uri host exists but no uri path on <thing>, this method adds '/' to uri path. 
   This is required for the namespace interning. See also, 
   \<a href='http://www.franz.com/support/documentation/8.1/doc/operators/uri/parse-uri.htm'\>parse-uri\</a\>
   in ACL document."
  (let ((parsed (net.uri:parse-uri thing)))
    #-:allegro-v9.0
    (etypecase thing
      (string (when (char= #\# (last-char thing)) (setf (net.uri:uri-fragment parsed) "")))
      (net.uri:uri nil)  ; nothing done
      )
    (cond ((and (net.uri:uri-host parsed) (null (net.uri:uri-path parsed)))
           (copy-uri parsed :path "/"))
          (t parsed))))

;;;
;;;; OntologySpace and NameSpaces
;;;
;;; XML namespaces provide a simple method for qualifying element and attribute names in XML 
;;; documents by associating them with URI references that provides namespaces for the names. 
;;; QNames are used for element names instead of URIs, and NSAttNames are used for namespace 
;;; declaration. See the details in \<http://www.w3.org/TR/xml-names/\>.
;;;
;;; A QName consists of Prefix and LocalPart. The Prefix is associated to a namespace URI, QNames 
;;; that share the same Prefix belong to the namespace, and the uniqueness of a LocalPart in the 
;;; namespace supports the global uniqueness of QName in the WWW. 
;;; This machinery is very similar to interning mechanism of lisp symbol in package, 
;;; which consists of a symbol name and package, where a symbol name is unique in the 
;;; package. Thus, the QName is implemented as exported lisp symbol, and the namespace 
;;; is implemented as lisp package. The packages for QNames are associated to the mamesapce URIs.

;;;
;;; In most cases, the mapping from a uri to a QName is algorithmically decidable. However, 
;;; there may be cases that system cannot decide how a uri should be mapped to a QName. 
;;; In such a case, a user must provide the mapping rule or the function giving each mapping 
;;; by replying the query from system one by one. Therefore, we need a maintenance device to 
;;; keep irregular mapping for such cases. All name spaces that related to XML namespace URIs 
;;; are installed into global variable <*NameSpaces*>.
;;;

(defvar *NameSpaces* (make-uri-space)
  "hasharray where a uri part associated to Prefix is interned. This space ensures the uniqueness 
of a Prefix associated uri by interning it. See <make-uri-space> in Allegro Common Lisp documentation.")

;;;
;;; All namespaces in system are listed by <list-all-uri-namedspaces>. See gxutil module.
;;;
#|
(loop for nsc being each hash-key in *NameSpaces* do (print nsc))
|#

;;;
;;; A vocabulary URI in a namespace may be converted to a corresponding QName, in which 
;;; a Prefix part is associated to the namespace URI, and a LocalPart is assocated to the 
;;; vocabulary in the namespace. The namespace on Prefix can be shared among URIs that 
;;; have the same Prefix so that LocalPart has a unique entry in the namedspace on Prefix. 
;;; This machinery is very similar to interning mechanism of lisp symbol in package, 
;;; which consists of a symbol name and package, where a symbol name is unique in the 
;;; package. Thus, the QName is implemented as exported lisp symbol, and the namespace 
;;; is implemented as lisp package. 
;;;
;;; <uri-namedspace> is a subclass of <net.uri:uri> which has two extra slots, <package> 
;;; and <env>. <package> value keeps a package associated to a URI namespace. In case 
;;; of regular mapping from URIs to QNames, no device for mapping from a local part 
;;; of URI to a symbol name is needed, because SWCLOS provides such regular mapping.
;;; However, in case of irregular mapping, the mapping from a full URI to a LocalPart 
;;; in the namespace or a full URI to a symbol name in the package is needed.
;;; This book-keeping is done for irregular mapping in the environment slot of 
;;; <uri-namedspace>.
;;; Followings provide such a mapping device and the uri namespace functionality.
;;;

(defclass uri-namedspace (net.uri:uri)
  ((package :initform nil :accessor uri-namedspace-package)
   (env :initform nil :accessor uri-namedspace-env)) ; env is a uri to symbol-name assoc list for irregular symbol.
  (:documentation "A subclass of <net.uri:uri>. This instance has extra two slots, i.e., an associated symbol 
package slot and uri to symbol name mapping environment slot.")
  )

(defun set-uri-namedspace (prefix-uri)
  "after interning <prefix-uri> to <*NameSpaces*>, change the class of <prefix-uri> from <net.uri:uri> 
   to <uri-namedspace>. After that, symbol to uri mapping can be placed in this namespace."
  (declare (optimize (speed 3) (safety 1)))
  (cl:change-class (intern-uri (parse-iri prefix-uri) *NameSpaces*) 'uri-namedspace))

(defun get-uri-namedspace (prefix-uri)
  "retrieves a uri-namedspace on <prefix-uri> from <*NameSpaces*> by interning it."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-iri prefix-uri) *NameSpaces*))
  (when (cl:typep prefix-uri 'uri-namedspace) prefix-uri))

(defun set-uri-namedspace-from-pkg (pkg)
  "supposing <pkg> has a documentation that is the same string as rendered <prefix-uri>, 
   sets the <prefix-uri> as uri-namedspace, and puts this <pkg> into uri-namedspace-package slot."
  (when (symbolp pkg) (setq pkg (find-package pkg)))
  (assert (not (null pkg)))
  (let* ((prefix-uri-str (documentation pkg t))
         (prefix-uri (parse-iri prefix-uri-str))
         (uri-namedspace (set-uri-namedspace prefix-uri)))
    (setf (uri-namedspace-package uri-namedspace) pkg)))

(defun uri2package (prefix-uri)
  "returns a package associated to <prefix-uri>."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-iri prefix-uri) *NameSpaces*))
  (when (and (cl:typep prefix-uri 'uri-namedspace) (slot-boundp prefix-uri 'package))
    (uri-namedspace-package prefix-uri)))

(defun uri2env (prefix-uri)
  "returns a LocalPart-symbol-name association list from <prefix-uri>.
   Note that the return value is null if there is no irregular mapping 
   from uri to QName and no mapping given by replying a query."
  (declare (optimize (speed 3) (safety 1)))
  (setq prefix-uri (intern-uri (parse-iri prefix-uri) *NameSpaces*))
  (when (and (slot-exists-p prefix-uri 'env) (slot-boundp prefix-uri 'env))
    (uri-namedspace-env prefix-uri)))

;;;
;;; The first thing to be done is, in spite that whether the mapping is regular or 
;;; irregular, to divide a uri into a Prefix part and a LocalPart part. In regular 
;;; mapping, a full URI with fragment is parted into a fragmentless URI and a fragment.
;;; The fragmentless URI is associated to Prefix of QName or package in lisp, and 
;;; the fragment turns out a LocalPart of QName or symbol name in lisp.
;;;
;;; For a URI without fragment, a function bound to global variable 
;;; <*uri2symbol-name-mapping-fun*> and <*uri2symbol-package-mapping-fun*> are
;;; invoked if the system requires a corresponding QName or a package for a URI.
;;; Users can bind their own functions to these variables in order to apply to 
;;; application oriented irregular mappings. However, two functions 
;;; <default-uri2symbol-name-mapping-fun> and <default-uri2symbol-package-mapping-fun>
;;; are bound as default. See <default-uri2symbol-name-mapping-fun> and <%%uri2symbol>
;;; for the details.

(defvar *uri2symbol-name-mapping-fun* 'default-uri2symbol-name-mapping-fun
  "a function to be invoked when uri to symbol name mapping is irregular.")
(defvar *uri2symbol-package-mapping-fun* 'default-uri2symbol-package-mapping-fun
  "a function to be invoked when uri to symbol package mapping is irregular.")

(defun uri2symbol (uri)
  "transforms <uri> to a QName symbol. If irregular mapping has been established, 
   the mapping is reused. When <uri> is a string, recursively calls with uri of <uri>.
   If <uri> is regular, namely a uri with fragment, <%uri2symbol> is used, else 
   <irregular-name&pkg> is used. When <uri> is nil, nil is returned."
  (etypecase uri
    (null nil)
    (string (uri2symbol (iri uri)))
    (net.uri:uri (if (uri-fragment uri) (%uri2symbol uri)
                   ;; irregular process
                   (irregular-name&pkg uri)))))

(defun %uri2symbol (uri)
  "in case of <uri> with fragment, mapping is regular. Then, <uri> without fragment is Prefix part and fragment of <uri> 
   is LocalPart part of <uri>. This function returns the QName of <uri> without consulting the LocalPart environment in .
   its namespace. If there is no package information on <uri> without fragment part, a function bound to 
   <*uri2symbol-package-mapping-fun*> is invoked. Note that uri-namedspace-env is unused in this regular case.
   Note that QName symbol is automatically exported in this function."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((name (iri-de-escape (uri-fragment uri)))
         (butnameuri (copy-uri uri :fragment ""))
         (pkg (uri2package butnameuri))
         (namedspace nil)
         (symbol nil))
    (unless pkg 
      (when (setq pkg (funcall *uri2symbol-package-mapping-fun* butnameuri))
        (unless (documentation pkg t)
          (setf (documentation pkg t) (render-uri butnameuri nil)))
        (setq namedspace (set-uri-namedspace butnameuri))
        (unless (uri-namedspace-package namedspace)
          (setf (uri-namedspace-package namedspace) pkg))))
    (when pkg
      (shadow name pkg)
      (setq symbol (intern name pkg))
      (export symbol pkg)
      symbol)))

(defun irregular-name&pkg (uri)
  "when the mapping is irregular, this function is called. 
   Firstly, a function bound to <*uri2symbol-name-mapping-fun*> is invoked with <uri> argument. 
   If it gives a symbol then the symbol is returned. 
   If it gives a string, the string is used as symbol name in a package that is obtained from <uri> through <uri2package> or 
   from a function bound to <*uri2symbol-package-mapping-fun*>. 
   When the package is newly obtained from <*uri2symbol-package-mapping-fun*>, the package is associated this <uri> itself. 
   Then, in the worst case, each irregular <uri> has its own namespace, as system cannot know general rules from one by one Q&A. 
   You had better provide a smarter application-oriented function on <*uri2symbol-name-mapping-fun*>
   that provides always an appropriate QName."
  (let ((name (funcall *uri2symbol-name-mapping-fun* uri)))
    (cond ((symbolp name) name)
          ((stringp name)
           (let ((pkg (uri2package uri))
                 (symbol nil))
             (unless pkg 
               (when (setq pkg (funcall *uri2symbol-package-mapping-fun* uri))
                 (setq uri (set-uri-namedspace uri))
                 (unless (uri-namedspace-package uri)
                   (setf (uri-namedspace-package uri) pkg))))
             (when pkg
               (unless (assoc uri (uri2env uri) :test #'net.uri:uri=)
                 (setf (uri-namedspace-env uri) (acons uri name (uri2env uri))))
               (shadow name pkg)
               (setq symbol (intern name pkg))
               (export symbol pkg)
               symbol))))))

;;;
;;;; Mapping URI to package and symbol, ID to symbol, anonymousID to symbol
;;;
;;;; Query for Users

(defun ask-user-for-string (prompt string1 option1 option2 prompt2)
  "This function is used in <ask-user-package-name> and <ask-user-symbol-name>."
  #+:common-graphics (cg:ask-user-for-string prompt string1 option1 option2 prompt2)
  #-:common-graphics
  (progn
    (format t "~%~A ~A:" prompt prompt2)
    (let ((str (read-line t)))
      (if (zerop (length str)) (values str nil "" nil)
        (values str nil "" t)))))

(let ((ask-user-pkg-name-canceled nil)
      (force-cancel nil))
  (defun ask-user-package-name (uri)
    "asks to user package name associated to <uri>."
    (unless force-cancel
      (let ((rendered (net.uri:render-uri uri nil)))
        (multiple-value-bind (pkg str2 button enter)
            (ask-user-for-string
             "QName prefix"
             (car (last (net.uri:uri-parsed-path uri))) "Enter" "Cancel"
             (format nil "as ~A" rendered))
          (declare (ignore str2 button))
          (cond (enter
                 (setq ask-user-pkg-name-canceled nil)
                 (unless (zerop (length pkg)) pkg))
                (t ;; canceled
                 (cond ((null ask-user-pkg-name-canceled)
                        (setq ask-user-pkg-name-canceled t)
                        nil)
                       ((y-or-n-p "Do you force cancelation forever?")
                        (setq force-cancel t)
                        nil)
                       (t (setq ask-user-pkg-name-canceled nil)
                          (setq force-cancel nil)))))))))
  
  (defun ask-user-symbol-name (uri)
    "asks to user a symbol name associated to <uri>."
    ;; This function is called only uri without fragment.
    (unless force-cancel
      (let ((rendered (net.uri:render-uri uri nil)))
        (multiple-value-bind (name str2 button enter)
            (ask-user-for-string
             "Symbol name"
             "" "Enter" "Cancel"
             (format nil "for ~A" rendered))
          (declare (ignore str2 button))
          (when (and enter (not (zerop (length name))))
            name)))))
  )

(defun default-uri2symbol-package-mapping-fun (uri)
  "This function is bound to <*uri2symbol-name-mapping-fun*> as default. This function just makes a query for users."
  (let ((pkg (ask-user-package-name uri)))
    ;; pkg is a string or nil
    (when pkg
      (let ((found (find-package pkg)))
        (cond (found 
               (cond ((string= (documentation found t) (net.uri:render-uri uri nil)) found)
                     ((y-or-n-p "~S is used for ~S~%Use another package name. OK?" found (documentation found t))
                      (default-uri2symbol-package-mapping-fun uri))
                     (t nil)))
              (t (setq pkg (make-package pkg :use nil))) ; by smh
              )))))

(defun default-uri2symbol-name-mapping-fun (uri)
  "This function is bound to <*uri2symbol-package-mapping-fun*> as default. If <uri> has a uri path, 
   then the returned value of <%%uri2symbol> is returned. Othewise a query is made for users."
  (cond ((and (uri-path uri) (not (string= (uri-path uri) "/")))
         (%%uri2symbol uri))                                   ; symbol
        ((cdr (assoc uri (uri2env uri) :test #'net.uri:uri=))) ; string
        (t (ask-user-symbol-name uri))))                       ; string or nil

(defparameter *file-types*
  '("rdf" "rdfs" "owl" "xml" "htm" "html" "txt"))

(defun %%uri2symbol (uri)
  "Even if <uri> has no fragment, plausible separation is done by this function. In short, 
   the file name or the most subfolder of <uri> path is taken as symbol name, and the remaining part 
   of <uri> path is taken for namespace (package) association. If you can find some application specific rules 
   for making QName, you had better program it as well as this function does."
  (let* ((uri-path (uri-path uri))
         (path (parse-namestring uri-path))
         (type (pathname-type path))
         (name (pathname-name path))
         (directory (pathname-directory path))
         (butnameuri nil)
         (pkg nil)
         (symbol nil))
    (cond ((and (not (null-string-p type))
                (string= type "nt"))
           (setq name (concatenate 'string name "_" type)))
          ((and (not (null-string-p type))
                (not (member type *file-types* :test #'string=)))
           ;; type is not a file type
           (setq name (concatenate 'string name "." type)))
          ((and (null-string-p type)
                (char= #\. (char uri-path (1- (length uri-path)))))   ; last char is a period.
           (setq name (concatenate 'string name "."))))
    ;(assert (and (eq (car directory) :absolute) (or name (cdr directory))))
    ;; ex: "http://somewhere/somedirectory/subdir/JohnSmith" -> #<uri http://somewhere/somedirectory/subdir/>
    ;; ex: "http://somewhere/JohnSmith"                      -> #<uri http://somewhere/>
    (setq directory (cdr directory)) ; delete :absolute
    (unless name
      (setq name (car (last directory)))
      (setq directory (butlast directory)))
    (setq butnameuri
          (copy-uri uri
                    :path (apply #'concatenate 'cl:string "/"
                                 (mapcan #'(lambda (d) (list d "/")) directory))))
    (setq butnameuri (set-uri-namedspace butnameuri))
    (setq pkg (uri2package butnameuri))
    (unless pkg 
      (when (setq pkg (funcall *uri2symbol-package-mapping-fun* butnameuri))
        (unless (documentation pkg t)
          (setf (documentation pkg t) (render-uri butnameuri nil)))
        (unless (uri-namedspace-package butnameuri)
          (setf (uri-namedspace-package butnameuri) pkg))))
    (when pkg
      (shadow name pkg)
      (setq symbol (intern name pkg))
      (export symbol pkg)
      (let ((env (uri-namedspace-env butnameuri)))
        (unless (assoc symbol env)
          (setf (uri-namedspace-env butnameuri)
            (acons symbol uri env))))
      symbol)))

;;;
;;; Note that PrefixedAttName declaration in XML documents set the namespace with NCname (Prefix) 
;;; and property value (associated prefix-uri). Note that DefaultAttName declaration in XML 
;;; documents set the default namespace. See Rdf module.

(defvar *default-namespace* nil
  "Default name space IRI in current time. This value is set by <read-rdf-from-http> and <read-rdf-file>.")
(defvar *base-uri* nil
  "Base URI that is indicated in XML file. This value is set by <read-rdf-from-http>, <read-rdf-file>, and <read-rdf-from-string>.")

(defun QName2PrefixedName (QName)
  "transforms <QName> to PrefixedName string. <QName> should be a lisp symbol."
  (concatenate 'cl:string (package-name (symbol-package QName)) ":" (symbol-name QName)))

(defun QName2UnPrefixedName (QName)
  "transforms <QName> to UnPrefixedName string. <QName> should be a lisp symbol."
  (symbol-name QName))

(defun export-as-QName (symbol)
  "export this <symbol> as QName. The symbol-package of <symbol> is stored 
   into the related uri namespace."
  (proclaim `(special ,symbol))
  (let* ((pkg (symbol-package symbol))
         (ns (documentation pkg t)))
    (when pkg
      ;; if symbol is a name of Ontology, no pkg
      (export symbol pkg)
      (when (and ns (setq ns (get-uri-namedspace ns)))
        (unless (uri-namedspace-package ns)
          (setf (uri-namedspace-package ns) pkg))))))

(defun QNameString2symbol (QName)
  "transforms <QName> string to a lisp symbol."
  (let (Prefix LocalPart pkg)
    (cond ((find #\: QName)
           (let (pos)
             (setq Prefix (subseq QName 0 (setq pos (position #\: QName))))
             (setq LocalPart (subseq QName (1+ pos)))))
          (t (setq Prefix nil)
             (setq LocalPart QName)))
    (when (null-string-p LocalPart)   ; needed for turtle reader
      (error "Not Yet!"))
    (cond (Prefix
           (setq pkg (find-package Prefix)) ; nicknames available
           (when (null pkg)
             (warn "There is no package for ~A." Prefix)
             (setq pkg (make-package Prefix :use nil))  ; by smh
             (warn "~W created." pkg))
           (shadow LocalPart pkg)
           (setq QName (intern LocalPart pkg))
           (export QName pkg))
          (*default-namespace*
           (setq pkg (uri-namedspace-package *default-namespace*))
           (cond (pkg (shadow LocalPart pkg)
                      (setq QName (intern LocalPart pkg))
                      (export QName pkg))
                 (t (warn "No package definition for default namespace ~S" *default-namespace*)
                    (error "What shoud we do?")
                    (setq QName LocalPart))))
          (*base-uri*
           (setq pkg (uri-namedspace-package *base-uri*))
           (cond (pkg (shadow LocalPart pkg)
                      (setq QName (intern LocalPart pkg))
                      (export QName pkg))
                 (t (warn "No package definition for base uri ~S" *base-uri*)
                    (error "What shoud we do?")
                    (setq QName LocalPart))))
          (t (setq QName LocalPart))) ; returns a string
    QName))

(defun symbol2QNameString (symbol)
  "transforms <symbol> to QName string in the current namespace."
  ;(format t "~%Default namespace package =~S" (uri-namedspace-package *default-namespace*))
  (cond ((eql (symbol-package symbol) (uri-namedspace-package *default-namespace*))
         (symbol-name symbol))
        (t (concatenate 'cl:string (package-name (symbol-package symbol)) ":" (symbol-name symbol)))))

;;;
;;;; Symbol to URI
;;;

(defun symbol2uri (symbol)
  "transforms <symbol> to its associated uri. The symbol package affects.
   The namespace uri should has been registered and documented in package."
  ;(when (multiple-value-bind (sym in/ex) (intern (string symbol) (symbol-package symbol))
  ;        (declare (ignore sym))
  ;        (not (eq in/ex :external)))
  ;  (error "Internal symbol ~S is designated in symbol2uri." symbol))
  (or (and (boundp symbol)
           (not (null (symbol-value symbol)))
           (slot-boundp (symbol-value symbol) 'rdf:about)
           (slot-value (symbol-value symbol) 'rdf:about)
           (iri (slot-value (symbol-value symbol) 'rdf:about)))
      (let* ((name (iri-escape-for-symbol-name (symbol-name symbol)))
             (pkg (symbol-package symbol))
             (uri (documentation pkg t))
             (ns (when uri
                   (or (get-uri-namedspace uri)
                       (set-uri-namedspace uri))))
             (env (when ns (uri-namedspace-env ns))))
        (or (cdr (assoc symbol env))
            (and ns
                 (cond ((and (pathname-name (merge-pathnames (net.uri:uri-path ns)))
                             (null (net.uri:uri-fragment ns)))
                        (iri 
                         (copy-uri (net.uri:parse-uri (net.uri:render-uri ns nil)) :fragment name)))
                       (t ;; no fragment but name
                        (iri
                         (copy-uri (net.uri:parse-uri (concatenate 'cl:string (net.uri:render-uri ns nil) name)))))))
            ))))

(defun name-ontology (ontouri)
  "transforms <ontouri> to special symbol of which string is equal to <ontouri>."
;;;  (when (null-iri-p ontouri)
;;;    (cond (*default-namespace* (setq ontouri (iri *default-namespace*)))
;;;          (*base-uri* (setq ontouri (iri *base-uri*)))
;;;          (t (error "Cant happen!"))))
  (format t "~%*default-namespace* = ~S" *default-namespace*)
  (format t "~%*base-uri* = ~S" *base-uri*)
  (format t "~%ontouri = ~S" ontouri)
  (when (and *base-uri* (string= (net.uri:render-uri ontouri nil) (net.uri:render-uri *base-uri* nil)))
    (let ((pkg (uri-namedspace-package
                (if (string= (net.uri:uri-path ontouri) "/2002/07/owl")
                    (set-uri-namedspace (format nil "~A#" (net.uri:render-uri ontouri nil)))
                  (set-uri-namedspace (net.uri:render-uri ontouri nil)))
                )))
      (unless pkg (setq pkg (uri2package ontouri)))
      (return-from name-ontology (intern "Ontology" pkg))))
  (when (and *default-namespace* (string= (net.uri:render-uri ontouri nil) (net.uri:render-uri *default-namespace* nil)))
    (let ((pkg (uri-namedspace-package (set-uri-namedspace *default-namespace*))))
      (unless pkg (setq pkg (uri2package ontouri)))
      (return-from name-ontology (intern "Ontology" pkg))))
  (let ((pkg (uri2package ontouri)))
    (when (null pkg)
      (setq pkg (funcall *uri2symbol-package-mapping-fun* ontouri)))
    (make-symbol (net.uri:render-uri ontouri nil))
    ))

;
;; Advice Package-name-to package function for Allegro Reader
;
#+never
(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf (symbol-function '%.reader-error)
    (symbol-function 'excl::.reader-error))
  )
#+never
(define-condition package-not-found-in-reader-error (reader-error) ())
#+never
(define-condition symbol-not-found-in-reader-error (reader-error) ())
#+never
(define-condition symbol-not-external-in-reader-error (reader-error) ())

#+never
(eval-when (:execute :load-toplevel :compile-toplevel)
  (excl:without-package-locks
      (defun excl::.reader-error (stream format &rest args)
        (cond ((string= (car args) "Package ~S not found.")
               (cerror "Create it?"
                       'package-not-found-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               (make-package (car (second args)))
               )
              ((string= (car args) "Symbol ~S not found in the ~A package.")
               (error 'symbol-not-found-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               )
              ((string= (car args) "~
The symbol ~s is not external in the ~a package.")
               (cerror "Export it?"
                       'symbol-not-external-in-reader-error
                       :stream stream
                       :format-control format :format-arguments args)
               (let ((pkg (find-package (second (second args)))))
                 (export (find-symbol (car (second args)) pkg) pkg)
                 (find-symbol (car (second args)) pkg))
               )
              (t (error 'reader-error :stream stream
                   :format-control format :format-arguments args)))
        
        )
    )
  )

(defun find-package-from-namespace (namespace)
  (find namespace (list-all-packages) :test #'string= :key #'(lambda (pkg) (documentation pkg t))))
  
;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide May-21-2014
;;;
