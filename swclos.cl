;;;  -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;
;;; This system is borrowed from AIMA by Norvig, and modified by Seiji.
;;; Copyright (c) 2008 Seiji Koide

;;;; A minimal facility for defining systems of files

(defparameter *doc-system-names* nil
  "A list of names of the systems that have been defined.")

(defstruct doc-system
  name (requires nil) (doc "") (parts nil) (examples nil) (loaded? nil))

(defun target-system-requires (system)
  (loop for mod in (ds:modules system)
      when (get-target-system mod)
      collect it))

(defun target-system-parts (system)
  (loop for mod in (ds:modules system)
      when (typep mod 'ds:default-module)
      collect mod))

;;;; The Top-Level Functions:

(defmacro def-doc-system (name requires doc &body parts)
  "Define a system as a list of parts.  A part can be a string, which denotes
  a file name; or a symbol, which denotes a (sub)system name; or a list of the
  form (subdirectory / part...), which means the parts are in a subdirectory.
  The <requires> argument is a list of systems that must be loaded before this 
  one.  Note that a documentation string is mandatory."
  `(add-doc-system :name ',name
                   :requires ',requires :doc ',doc :parts ',parts))

;;;; Support Functions

(defun add-doc-system (&key name requires doc parts examples)
  (pushnew name *doc-system-names*)
  (setf (get 'doc-system name)
    (make-doc-system :name name :examples examples
                     :requires requires :doc doc :parts parts)))

(defun get-target-system (name)
  "Return the system with this name.  (If argument is a system, return it.)"
  (cond ((typep name 'ds:default-system) name)
        ((symbolp name) (find-system name))
        (t nil)))

(defun operate-on-target-system (target part operation &key (path nil) (load t)
                                        (directory-operation #'identity))
  "Perform the operation on the part (or system) and its subparts (if any).
  Reasonable operations are load, load-binary, compile-load, and echo.
  If LOAD is true, then load any required systems that are unloaded."
  (let (system)
    (cond
     ((typep part 'ds:default-module) (funcall operation (ds:source-pathname part)))
     ((stringp part) (funcall operation (target-file part :swclos :path path)))
     ((and (consp part) (eq (second part) '/))
      (let* ((subdirectory (mklist (first part)))
             (new-path (append path subdirectory)))
        (funcall directory-operation new-path)
        (dolist (subpart (nthcdr 2 part))
          (operate-on-target-system target subpart operation :load load 
                                    :path new-path
                                    :directory-operation directory-operation))))
     ((consp part)
      (dolist (subpart part)
        (operate-on-target-system target subpart operation :load load :path path
                                  :directory-operation directory-operation)))
     ((setf system (get-target-system part))
      ;; Load the required systems, then operate on the parts
      (load-system system)
      (operate-on-target-system target (target-system-parts system) operation
                                :load load :path path
                                :directory-operation directory-operation))
     (t (warn "Unrecognized part: ~S in path ~A" part path)))))

(defun target-file (name system &key (type nil) (path nil))
  "Given a file name and maybe a file type and a relative path from the 
  target system root directory, return the right complete pathname."
  (make-pathname :name name :type type :defaults #+:asdf (asdf:component-pathname (asdf:find-system system))
                                                 #-:asdf (ds:default-pathname (find-system system))
                 :directory (append (pathname-directory #+:asdf (asdf:component-pathname (asdf:find-system system))
                                                        #-:asdf (ds:default-pathname (find-system system)))
                                    (mklist path))))

(defun mklist (x)
  "If x is a list, return it; otherwise return a singleton list, (x)."
  (if (listp x) x (list x)))

;;; ----------------------------------------------------------------------
;;;; Definitions of Systems
;;; ----------------------------------------------------------------------

(def-doc-system :RDF ()
  "RDF module."
  ("RDF" / "Utils" "RdfIO" "IRI" "packages" "Xml" "rdferror" "NameSpace" 
   "RDFShare" "Rdf" "RdfReader" "node"))

(def-doc-system :RDFS ()
  "RDFS module."
  ("RDFS" / "SlotDef" "RDFboot" "GxType" "DomainRange" "RdfsObjects" "RdfsKernel" 
   "GxForwardRef" "RdfsCore" "gxutils" "rdfwriter"))

(def-doc-system :OWL (:RDFS)
  "OWL module."
  ("OWL" / "owlerror" "owlkernel" "owlsamedifferent" "owlequivalentdisjoint" "NNF" "tunify" "subsume" "OWL"))

(def-doc-system :NTriple (:RDFS :OWL)
  "NTriple module."
  ("NTriple" / "ntriple" "ntparser" "ntwriter"))

(def-doc-system all ()
  "There is no difference between all and everything"
  RDF RDFS OWL NTriple)

(def-doc-system everything ()
  "There is no difference between all and everything"
  all)

(setf *doc-system-names* (nreverse *doc-system-names*))
