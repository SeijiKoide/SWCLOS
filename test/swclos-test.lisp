(require :tester)

;;; cd SWCLOS
;;; (load "SWCLOSsys.cl")
;;; (compile-system :swclos :recompile t)

(in-package :gx-user)

(use-package :util.test)


(defmacro test-uo (expected-value test-form)
  `(test ,(princ-to-string expected-value)
         (format nil "~S" ,test-form)
         :test #'string=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; 03Resource.htm
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;gx-user(3): 
(test <http://www.w3.org/2000/01/rdf-schema#Resource>
      <http://www.w3.org/2000/01/rdf-schema#Resource>)

;; gx-user(4): 
(test-uo "#<rdfs:Class rdfs:Resource>"
         (iri-value <http://www.w3.org/2000/01/rdf-schema#Resource>))

;; gx-user(5): 
(test 'rdfs:Resource
      (uri2symbol <http://www.w3.org/2000/01/rdf-schema#Resource>))


;; gx-user(6): 
(test <http://www.w3.org/2000/01/rdf-schema#Resource>
      (symbol2uri 'rdfs:Resource))


;;; <John_Doe> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <Man> .

(test-uo "#<rdfs:Class rdfs:Resource>" rdfs:Resource)


;; gx-user(8): 
(test-uo "#<rdfs:Class rdfs:Resource>"
         (iri-value <http://www.w3.org/2000/01/rdf-schema#Resource>))

;; gx-user(9): 
(test-uo "#<rdfs:Class rdfs:Resource>"
         <<http://www.w3.org/2000/01/rdf-schema#Resource>>)

#|||
;; gx-user(17): 
 (uri2symbol <http://somewhere/maindir/subdir/JohnSmith>)
;>>  QName prefix as http://somewhere/maindir/subdir/: --> somewhere
;=>  oh:JohnSmith
swsd:JohnSmith


;; gx-user(18): 
 (uri2symbol <http://somewhere/JohnSmith>)
;=>  |8|:JohnSmith


sw:JohnSmith
;; gx-user(19): 

Symbol name for http://JohnSmith/:js

QName prefix as http://JohnSmith/:js

 (uri2symbol <http://JohnSmith/>)
;=>  js:js
;>>  
;>>  Symbol name for http://JohnSmith/:
;>>  QName prefix as http://JohnSmith/:
;=>  nil

no:J.S.
|||#

;; gx-user(2): 
(defpackage :ex 
  (:documentation "http://somewhere/main/sub/file"))
;=>  #<The ex package>

(test t (and (find-package :ex) t))

;; gx-user(3): 
(test (find-package :ex)
      (set-uri-namedspace-from-pkg (find-package :ex)))

;; gx-user(4):
(test "http://somewhere/main/sub/file"
  (net.uri::uri-string (get-uri-namedspace <http://somewhere/main/sub/file>))
  :test #'string=)

#|| ???
;; gx-user(5):
 (test (find-package :ex)
  (uri2package "http://somewhere/main/sub/file"))

;; gx-user(6): 
 (uri2symbol "http://somewhere/main/sub/file#JohnSmith")
;=>  fx:JohnSmith

;; QName prefix as http://somewhere/main/sub/file#
;; fx
 (uri2symbol "http://somewhere/main/sub/file#JohnSmith")
;=>  fx:JohnSmith

ex:JohnSmith
|||#
;; gx-user(7): 

(test '(<http://www.w3.org/2000/01/rdf-schema#Literal>
        <http://www.w3.org/2000/01/rdf-schema#comment>
        <http://www.w3.org/2000/01/rdf-schema#subClassOf>
        <http://www.w3.org/2000/01/rdf-schema#Class>
        <http://www.w3.org/2000/01/rdf-schema#Datatype>
        <http://www.w3.org/2000/01/rdf-schema#domain>
        <http://www.w3.org/2000/01/rdf-schema#ContainerMembershipProperty>
        <http://www.w3.org/2000/01/rdf-schema#member>
        <http://www.w3.org/2000/01/rdf-schema#Container>
        <http://www.w3.org/2000/01/rdf-schema#isDefinedBy>
        <http://www.w3.org/2000/01/rdf-schema#range>
        <http://www.w3.org/2000/01/rdf-schema#seeAlso>
        <http://www.w3.org/2000/01/rdf-schema#label>
        <http://www.w3.org/2000/01/rdf-schema#Resource>
        <http://www.w3.org/2000/01/rdf-schema#subPropertyOf>)
  (loop for x being each external-symbol in (find-package :rdfs)
        collect (symbol2uri x))
  :test #'equal)


;; gx-user(4):

(test '(rdfs:Literal rdfs:comment rdfs:subClassOf rdfs:Class rdfs:Datatype
        rdfs:domain rdfs:ContainerMembershipProperty rdfs:member
        rdfs:Container rdfs:isDefinedBy rdfs:range rdfs:seeAlso rdfs:label
        rdfs:Resource rdfs:subPropertyOf)
  (list-all-entities-in :rdfs)
  :test #'equal)


;; gx-user(2):

(test t (cl:typep rdfs:Resource rdfs:Resource))

;; gx-user(3):
(test t (cl:typep rdfs:Class rdfs:Resource))


;; gx-user(4):
(test t (cl:typep rdf:Property rdfs:Resource))

;; gx-user(5): 
(test t (object? 'rdfs:Resource))

;; gx-user(6): 
(test t (object? 'rdfs:Class))

;; gx-user(7):
(test t (object? 'rdf:Property))


;; gx(8): 
(test '(rdfs:Resource rdfs:Container rdf:Alt rdf:Seq rdf:Bag
        ill-structured-XMLLiteral rdf:Statement rdf:List rdf:nil rdfs:Literal
        rdf:XMLLiteral rdf:PlainLiteral rdf:Property rdfs:member rdf:value
        rdf:rest rdf:first rdf:object rdf:subject rdf:predicate rdf:type
        rdfs:seeAlso rdfs:subPropertyOf rdfs:subClassOf rdfs:range rdfs:domain
        rdfs:isDefinedBy rdfs:comment rdfs:label
        rdfs:ContainerMembershipProperty |rdfs:Resource| rdfs:Class
        owl:cardinalityRestriction owl:hasValueRestriction
        owl:someValuesFromRestriction owl:allValuesFromRestriction
        owl:Restriction owl:ObjectProperty owl:Class rdf:Alt rdf:Seq rdf:Bag
        rdfs:ContainerMembershipProperty rdfs:Container
        ill-structured-XMLLiteral rdf:Statement shadowed-class
        ill-structured-XMLLiteral shadowed-class rdf:Property rdfs:Resource
        rdfs:Class shadowed-class rdfs:Datatype xsd:duration xsd:boolean
        xsd:anyURI xsd:double xsd:float xsd:string xsd:unsignedByte
        xsd:unsignedShort xsd:unsignedInt xsd:unsignedLong xsd:positiveInteger
        xsd:nonNegativeInteger xsd:byte xsd:short xsd:int xsd:long
        xsd:negativeInteger xsd:nonPositiveInteger xsd:integer xsd:decimal
        xsd:anySimpleType rdf:XMLLiteral rdf:PlainLiteral)
  (mapcar #'name (list-all-resources t))
  :test #'equal)


;; gx-user(2):
(test 'rdfs:Resource
      (name rdfs:Resource))

;; gx-user(3): 
(test-uo "#<rdfs:Resource :anonymous>"
         (addObject rdfs:Resource '((rdf:about "NothingElseURI"))))

;; gx-user(4): 
(test-uo "#<rdfs:Resource :anonymous>"
         <<NothingElseURI>>)

;; gx-user(5):
(test "NothingElseURI"
      (slot-value <<NothingElseURI>> 'rdf:about)
      :test #'string=)

;; gx-user(6):
(test t (anonymous-p <<NothingElseURI>>))

;; gx-user(2): 
(test-uo "#<|rdfs:Resource| :anonymous>" _:a01)

;; gx-user(3):
(test '_:a01 (quote _:a01))

;; gx-user(4):
(test-uo "(#<|rdfs:Resource| :anonymous> #<|rdfs:Resource| :anonymous>)"
         (list _:a01 _:a02))


;; gx-user(5):
(test t (eq _:a01 _:a01))

;; gx-user(6): 
(test nil (eq _:a01 _:a02))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; 04RDFObject.htm
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|(rdf:Description
  (rdf:about "http://www.w3.org/TR/rdf-syntax-grammar")
  (ex:editor
    (rdf:Description
      (ex:homePage
        (rdf:Description (rdf:about "http://purl.org/net/dajobe/")))
      (ex:fullName "Dave Beckett")))
  (dc:title "RDF/XML Syntax Specification (Revised)"))|#

;; gx-user(3): 
(defpackage ex)

(test t (and (find-package :ex) t))

;; gx-user(4):
(defpackage dc)

(test t (and (find-package :dc) t))

;;gx-user(5):
(addForm '(rdf:Description
           (rdf:about "http://www.w3.org/TR/rdf-syntax-grammar")
           (ex::editor
            (rdf:Description
             (ex::homePage
              (rdf:Description (rdf:about "http://purl.org/net/dajobe/")))
             (ex::fullName "Dave Beckett")))
           (dc::title "RDF/XML Syntax Specification (Revised)")))
;>>> QName prefix as http://purl.org/net/:purl
;>>> QName prefix as http://www.w3.org/TR/:w3
#|||

Warning: Entail by rdf1: ex::editor rdf:type rdf:Property.
Warning: Entail by rdf1: dc::title rdf:type rdf:Property.
Warning: Entail by rdf1: ex::homePage rdf:type rdf:Property.
Warning: Entail by rdf1: ex::fullName rdf:type rdf:Property.
#<|rdfs:Resource| common-lisp:nil>
|||#

;; gx-user(6):
(test '(rdf:Description w3:rdf-syntax-grammar
        (rdf:about <http://www.w3.org/TR/rdf-syntax-grammar>)
        (ex:editor
         (rdf:Description (ex:homePage purl:dajobe)
          (ex:fullName "Dave Beckett")))
        (dc:title "RDF/XML Syntax Specification (Revised)"))
  (get-form <<http://www.w3.org/TR/rdf-syntax-grammar>>)
  :test #'equal)


#|(rdf:Description (rdf:about "http://www.w3.org/TR/rdf-syntax-grammar")
  (ex:editor
    (rdf:Description
      (ex:homePage (rdf:Description (rdf:about "http://purl.org/net/dajobe/")))
      (ex:fullName "Dave Beckett")))
  (dc:title "RDF/XML Syntax Specification (Revised)"))|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; 05RDFSchema.htm
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; gx-user(33):
(test t (rsc-object-p rdfs:Class))

;; gx-user(34):
(test t (rsc-object-p rdfs:Resource))

;; gx-user(35):
(test t (rsc-object-p rdf:Property))

;; gx-user(36):
(test t (rsc-object-p rdfs:comment))

;; gx-user(37):

(test nil (rsc-object-p "This is a comment."))

;; gx-user(38):
(test nil (rsc-object-p "This is also a comment."@en))

;; gx-user(39):
(test t (rsc-object-p "1"^^xsd:nonNegativeInteger))

;; gx-user(40):
(test nil (rsc-object-p 1))

;; gx-user(41):
(test nil (rsc-object-p <Foo>))

;; gx-user(42):
;;; QName prefix as /
(test t (rsc-object-p <<Foo>>))

;; gx-user(43):
(test t (rdf-class-p rdfs:Class))

;; gx-user(44):
(test t (rdf-class-p rdfs:Resource))

;; gx-user(45):
(test t (rdf-class-p rdf:Property))

;; gx-user(46):
(test nil (rdf-class-p rdfs:comment))

;; gx-user(47):
(test t (rdf-metaclass-p rdfs:Class))

;; gx-user(48):
(test nil (rdf-metaclass-p rdfs:Resource))

;; gx-user(48):
(test nil (rdf-instance-p rdf:Property))

;; gx-user(49):
(test t (rdf-instance-p rdfs:comment))

;; gx-user(50):
(test t (rdf-class-p rdf:List))

;; gx-user(51):
(test t (rdf-instance-p rdf:nil))

;; gx-user(10):
(test 'xsd:byte (type-of 1))

;; gx-user(11):
(test 'xsd:integer (type-of "1"^^xsd:integer))

;; gx-user(14):
(test 'rdfs:Class (type-of rdf:Property))

;; gx-user(15):
(test-uo "#<rdfsClass rdfs:Class>" (class-of rdf:Property))

;; gx-user(16):
(test 'rdf:Property (type-of rdfs:comment))

;; gx-user(17):
(test-uo "#<rdfs:Class rdf:Property>"
         (class-of rdfs:comment))

;; gx-user(18):
(test 'rdfs:Class (type-of rdfs:Datatype))

;; gx-user(19):
(test-uo "#<rdfsClass rdfs:Class>"
         (class-of rdfs:Datatype))

;; gx-user(20):
(test 'rdfs:Class (type-of rdfs:Class))

;; gx-user(21):
;; ???
#|(test-uo "#<gx::meta-node rdfsClass>"
         (class-of rdfs:Class))|#

(test-uo "#<gx::rdf-node rdfsClass>"
         (class-of rdfs:Class))


;; gx-user(31):
(test-uo "(#<rdf:List rdf:nil>)"
         (collect-direct-instances-of rdf:List))

;; gx-user(32):
(test '(owl:cardinalityRestriction owl:hasValueRestriction
        owl:someValuesFromRestriction owl:allValuesFromRestriction
        owl:Restriction owl:ObjectProperty owl:Class rdf:Alt rdf:Seq rdf:Bag
        rdfs:ContainerMembershipProperty rdfs:Container
        ill-structured-XMLLiteral rdf:Statement shadowed-class
        ill-structured-XMLLiteral shadowed-class rdf:Property rdfs:Resource
        rdfs:Class xsd:duration xsd:boolean xsd:anyURI xsd:double xsd:float
        xsd:string xsd:unsignedByte xsd:unsignedShort xsd:unsignedInt
        xsd:unsignedLong xsd:positiveInteger xsd:nonNegativeInteger xsd:byte
        xsd:short xsd:int xsd:long xsd:negativeInteger xsd:nonPositiveInteger
        xsd:integer xsd:decimal xsd:anySimpleType rdf:XMLLiteral
        rdf:PlainLiteral)
  (mapcar #'name (collect-all-instances-of rdfs:Class))
  :test #'equal)

;; gx-user(7):
(test '(t t)
  (multiple-value-list
   (typep <http://somewhere/> rdfs:Resource))
  :test #'equal)

;; gx-user(8):
(test '(t t)
  (multiple-value-list 
   (typep "This is literal." rdfs:Resource))
  :test #'equal)

;; gx-user(9):
(test '(t t)
  (multiple-value-list 
   (typep 1 rdfs:Resource))
  :test #'equal)

;; gx-user(10):
(test '(t t)
  (multiple-value-list 
   (typep "This is literal." rdfs:Literal))
  :test #'equal)

;; gx-user(11):
(test '(t t)
  (multiple-value-list 
   (typep 1 rdfs:Literal))
  :test #'equal)


;; gx-user(35):
(test '(t t)
  (multiple-value-list 
   (subtypep rdf:Alt rdfs:Container))
  :test #'equal)

;; gx-user(36):
(test '(t t)
  (multiple-value-list 
   (subtypep rdf:Alt rdfs:Resource))
  :test #'equal)

;; gx-user(37):
(test '(t t)
  (multiple-value-list 
   (subtypep <http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt> rdfs:Resource))
  :test #'equal)

;; gx-user(38):
;;; ???
(test '(t t)
  (multiple-value-list 
   (subtypep xsd:integer rdfs:Literal))
  :test #'equal)

;; gx-user(39):
;;; ???
(test '(t t)
  (multiple-value-list 
   (subtypep xsd:integer rdfs:Resource))
  :test #'equal)


;; gx-user(40):
(test-uo "#<rdfs:Class rdfs:Container>"
         (slot-value rdf:Alt 'rdfs:subClassOf))

;; gx-user(3):
(test t (subproperty-p rdfs:isDefinedBy rdfs:seeAlso))

;; gx-user(4):
;;; subproperty-of -> undef
#|(test-uo "(#<rdf:Property rdfs:isDefinedBy>)"
         (subproperty-of rdfs:seeAlso))|#

;; (rdfs:subPropertyOf rdfs:isDefinedBy)
;=>  (#<rdf:Property rdfs:seeAlso>)

;; gx-user(5):
(test-uo "(#<rdf:Property rdfs:seeAlso>)"
         (superproperty-of rdfs:isDefinedBy))


;; gx-user(2):
(test-uo "#<rdf:Property rdfs:comment>"
         rdfs:comment)

;; gx-user(3):
(test "The class resource, everything." (slot-value rdfs:Resource 'rdfs:comment)
      :test #'string=)

;; gx-user(4):
(test "A description of the subject resource."
      (slot-value rdfs:comment 'rdfs:comment)
      :test #'string=)

;; gx-user(2):
(test-uo "#<rdfs:Class rdf:Property>"
         (slot-value rdfs:comment 'rdf:type))

;; gx-user(3):
(test-uo "#<rdfs:Class rdf:Property>"
         (class-of rdfs:comment))

;; gx-user(4):
(test 'rdf:Property (type-of rdfs:comment))

;; gx-user(5):
(test 'rdf:Property (-> rdfs:comment rdf:type))

;; gx-user(6):
(test 'rdfs:Class (-> rdfs:comment rdf:type rdf:type))

;; gx-user(7):
;;; ??? subproperty-of -> undef
#|(test "The class of classes." 
      (-> rdfs:comment rdf:type rdf:type rdfs:comment)
      :test #'string=)|#

;; gx-user(8):
;;; ??? subproperty-of -> undef
#|(test 'xsd:string
      (-> rdfs:comment rdf:type rdf:type rdfs:comment rdf:type))|#


;; gx-user(2):
(defpackage vin)

(test t (and (find-package :vin) t))

;; gx-user(3):
;;; ??? attempt to call `rdfs:range' which is an undefined function.
(defIndividual vin::ElyseZinfandel
  (rdf:type vin::Zinfandel)
  (vin::hasMaker vin::Elyse))

#||
Warning:
Entail by rdf1:
vin::hasMaker rdf:type 
rdf:Property.
Warning:
Range entail by rdf:type:
vin::Zinfandel rdf:type 
rdfs:Class.
#<vin:Zinfandel vin:ElyseZinfandel>
||#
;; gx-user(4):

(test-uo "#<|rdfs:Resource| vin:Elyse>"
         vin:Elyse)

;; gx-user(5):
(defIndividual vin:Elyse (rdf:type vin::Winery))
;; Warning: Range entail by rdf:type:

;; gx-user(6):
(defConcept vin:Zinfandel (rdfs:subClassOf vin::Wine))
;; Warning: Range entailX1 by rdfs:subClassOf: vin::Wine rdf:type rdfs:Class.
;; #<rdfs:Class vin:Zinfandel>

;; gx-user(7):
(test-uo "#<rdfs:Class vin:Wine>"
         (slot-value vin:Zinfandel 'rdfs:subClassOf))


;; gx-user(8):
(test '(t t)
  (multiple-value-list
   (subtypep vin:Zinfandel rdfs:Resource))
  :test #'equal)

;; gx-user(9):
(defIndividual vin:ElyseZinfandel (vin::hasColor vin::Red))
;; Warning: Entail by rdf1: vin::hasColor rdf:type rdf:Property.
;; #<vin:Zinfandel vin:ElyseZinfandel>

;; gx-user(10):
(test '(|rdfs:Resource| vin:ElyseZinfandel (rdf:type vin:Zinfandel)
        (vin:hasMaker vin:Elyse) (vin:hasColor vin:Red))
  (get-form vin:ElyseZinfandel)
  :test #'equal)

;; gx-user(17):
(makunbound 'MyResource)
(defIndividual MyResource (myProp "original one"))
;; Warning: Entail by rdf1: myProp rdf:type rdf:Property.
;; #<|rdfs:Resource| MyResource>

;; gx-user(18):
(test "original one" (slot-value MyResource 'myProp)
      :test #'string=)

;; gx-user(19):
(test '("added 1st" "original one")
  (put-value MyResource myProp "added 1st")
  :test #'equal)

;; gx-user(20):
(test '("added 2nd" "added 1st" "original one")
  (put-value MyResource myProp "added 2nd")
  :test #'equal)

;; gx-user(21):
(test '("added 2nd" "added 1st" "original one")
  (slot-value MyResource 'myProp)
  :test #'equal)

;; gx-user(22):
(test '("added 2nd" "added 1st" "original one")
  (put-value MyResource myProp "added 1st")
  :test #'equal)

;; gx-user(23):
(test '("added 2nd" "added 1st" "original one")
  (slot-value MyResource 'myProp)
  :test #'equal)


;; gx-user(2):
(defpackage vin)

;;; ??? attempt to call `owl-class-p' which is an undefined function.
;; gx-user(3):
#|(defIndividual vin::SaucelitoCanyonZinfandel1998
  (rdf:type vin::Zinfandel)
  (rdf:type vin::Vintage))|#

;; gx-user(4):
#|(test '(t t)
  (multiple-value-list
   (typep vin:SaucelitoCanyonZinfandel1998 vin:Zinfandel))
  :test #'equal)|#

;; gx-user(5):
#|(test '(t t)
  (multiple-value-list
   (typep vin:SaucelitoCanyonZinfandel1998 vin:Vintage))
  :test #'equal)|#

;; gx-user(6):
#|(test '(vin:Vintage vin:Zinfandel)
  (type-of vin:SaucelitoCanyonZinfandel1998)
  :test #'equal)|#

;; gx-user(53):
(test-uo "#<rdf:Property rdfs:comment>"
         rdfs:comment)

;; gx-user(54):
;;; ???
;; "#<gx::Property-effective-slot-definition rdfs:comment @ #x20a9520a>"
(test 'gx::Property-effective-slot-definition
  (type-of (find 'rdfs:comment (mop:class-slots rdfs:Class) :key #'gx:name)))

;; gx-user(55):
;;; ???
(test t (cl:typep (find 'rdfs:comment (mop:class-slots rdfs:Class)
                        :key #'name)
                  'mop:slot-definition))

;; gx-user(56):
(test '((rdf:PlainLiteral
            "The class of plain (i.e. untyped) literal values.")
           (rdf:XMLLiteral "The class of XML literal values.")
           (rdfs:Class "The class of classes.")
           (rdfs:Resource "The class resource, everything.")
           (rdf:Property "The class of RDF properties.")
           (rdf:Statement "The class of RDF statements.")
           (rdfs:Container "The class of RDF containers.")
           (rdfs:ContainerMembershipProperty
            "The class of container membership properties, rdf:_1, rdf:_2, ...,    all of which are sub-properties of 'member'.")
           (rdf:Bag "The class of unordered containers.")
           (rdf:Seq "The class of ordered containers.")
           (rdf:Alt "The class of containers of alternatives.")
           (rdfs:label "A human-readable name for the subject.")
           (rdfs:comment "A description of the subject resource.")
           (rdfs:isDefinedBy "The definition of the subject resource.")
           (rdfs:domain "A domain of the subject property.")
           (rdfs:range "A range of the subject property.")
           (rdfs:subClassOf "The subject is a subclass of a class.")
           (rdfs:subPropertyOf "The subject is a subproperty of a property.")
           (rdfs:seeAlso
            "Further information about the subject resource.")
           (rdf:type "The subject is an instance of a class.")
           (rdf:predicate
            "The predicate of the subject RDF statement.")
           (rdf:subject "The subject of the subject RDF statement.")
           (rdf:object "The object of the subject RDF statement.")
           (rdf:first "The first item in the subject RDF list.")
           (rdf:rest
            "The rest of the subject RDF list after the first item.")
           (rdf:value "Idiomatic property used for structured values.")
           (rdfs:member "A member of the subject container.")
           (rdf:nil
            "The empty list, with no items in it. If the rest of a list is nil then the list has no  more items in it."))
  (mapcar (lambda (x)
            (destructuring-bind (class desc) x
              `(,(name class) 
                 ,(substitute #\Space #\Newline
                              (substitute #\Space #\Return desc)))))
          (collect-all-extensions-of rdfs:comment))
  :test #'equal)

;;; *EOF*

