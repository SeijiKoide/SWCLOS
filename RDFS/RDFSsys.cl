;;; -*- Mode: common-lisp; Syntax: Common-Lisp; -*-

(eval-when (:load-toplevel :execute)
  (defparameter *rdfs-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (pathname-directory *load-truename*)))
  (setf (logical-pathname-translations "RDFS")
    `(("*.*"
       ,(make-pathname
         :host (pathname-host *rdfs-directory*)
         :device (pathname-device *rdfs-directory*)
         :directory (pathname-directory *rdfs-directory*)
         :name :wild
         :type :wild
         ))))
  ) ; end of eval-when

(excl:defsystem :rdf (:pretty-name "RDF subsystem of SWCLOS"
                       :default-pathname #,*rdfs-directory*)
  (:module :utils        "../RDF/Utils")
  (:module :rdfio        "../RDF/RdfIO")
  (:module :iri          "../RDF/IRI")
  (:module :swclospackages "../RDF/packages")
  (:module :xml          "../RDF/Xml"          (:load-before-compile :swclospackages))
  (:module :rdferror     "../RDF/rdferror"     (:load-before-compile :utils :swclospackages))
  (:module :namespace    "../RDF/NameSpace"    (:load-before-compile :swclospackages :iri))
  (:module :litreal      "../RDF/Literal"      (:load-before-compile :utils :swclospackages :xml))
  (:module :rdfshare     "../RDF/RDFShare"     (:load-before-compile :swclospackages :rdfio :namespace))
  (:module :rdfparser    "../RDF/RdfParser"    (:load-before-compile :swclospackages :namespace :rdfshare))
  (:module :rdfform      "../RDF/RdfReader"    (:load-before-compile :swclospackages :rdfparser))
  (:module :rdfnode      "../RDF/node"         )
  )

(excl:defsystem :rdfs (:pretty-name "RDFS subsystem of SWCLOS"
                       :default-pathname #,*rdfs-directory*)
  (:module :rdf :rdf)
  (:module :slotdef      "SlotDef"      (:load-before-compile :rdf))
  (:module :rdfboot      "RDFboot"      (:load-before-compile :rdf :slotdef))
  (:module :domainrange  "DomainRange"  (:load-before-compile :rdf :rdfboot))
  (:module :rdfskernel   "RdfsKernel"   (:load-before-compile :rdf :slotdef :rdfboot))
  (:module :gxtype       "GxType"       (:load-before-compile :rdf :slotdef :rdfboot))
  (:module :rdfsobjects  "RdfsObjects"  (:load-before-compile :rdf :rdfboot :gxtype))
  (:module :gxforwardref "GxForwardRef" (:load-before-compile :rdf :gxtype :rdfsobjects :domainrange :rdfskernel))
  (:module :rdfscore     "RdfsCore"     (:load-before-compile :rdf :domainrange :rdfsobjects :rdfskernel))
  (:module :gxutils      "gxutils"      (:load-before-compile :rdf :rdfscore))
  (:module :rdfwriter    "rdfwriter"    (:load-before-compile :rdf :gxutils :gxforwardref))
  )

(format t "~%;;To recompile, execute these forms:~%~s~%"
  '(excl:compile-system :rdfs :recompile t))

(format t "~%;;To load, execute these forms:~%~s~%"
  '(excl:load-system    :rdfs))
