;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-

(eval-when (:load-toplevel :execute)
  (defparameter *rdf-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "RDF")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *rdf-directory*)
         :device (pathname-device *rdf-directory*)
         :directory (pathname-directory *rdf-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; end of eval-when

(excl:defsystem :rdf (:pretty-name "RDF subsystem of SWCLOS"
                       :default-pathname #,*rdf-directory*)
  (:module :utils        "Utils")
  (:module :rdfio        "RdfIO")
  (:module :iri          "IRI")
  (:module :rdfspackages "packages")
  (:module :xml          "Xml"          (:load-before-compile :rdfspackages))
  (:module :rdferror     "rdferror"     (:load-before-compile :utils :rdfspackages))
  (:module :namespace    "NameSpace"    (:load-before-compile :rdfspackages :iri))
  (:module :rdfliteral   "Literal"      (:load-before-compile :xml))
  (:module :rdfshare     "RDFShare"     (:load-before-compile :rdfspackages :rdfio :namespace))
  (:module :rdfparser    "RdfParser"    (:load-before-compile :rdfspackages :namespace :rdfshare))
  (:module :rdfform      "RdfReader"    (:load-before-compile :rdfspackages :rdfparser))
  (:module :rdfnode      "node"         (:load-before-compile :rdfspackages :iri))
  )

(format t "~%;;To recompile, execute these forms:~%~s~%"
  '(excl:compile-system :rdf :recompile t))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(excl:load-system    :rdf))
