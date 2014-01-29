;;;-*- Mode: common-lisp; syntax: common-lisp; package: xsd; base: 10 -*-
;;;
;;;; XML 1.1
;;;
;;; IT Program Project in Japan: 
;;;    Building Operation-Support System for Large-scale System using IT
;;;
;;; This code was encoded by Seiji Koide at Galaxy Express Corporation, Japan,
;;; for the realization of the MEXT IT Program in Japan.
;;;
;;; Copyright (c) 2002, 2003, 2004
;;;    Galaxy Express Corporation
;;; 
;;; Copyright (c) 2007, 2008, 2010
;;;    Seiji Koide
;;;
;;; This file provides xml and xsd name space, and xsd datatypes in lisp. The data types as 
;;; resource objects that wrap lisp data are defined in RdfsKernel and RdfsCore file.
;;; 
;;;
;; History
;; -------
;; 2008.12.25    The contents are blushed up with respect to denotational semantics. 
;; 2005.12.07    This file is shared by DIG module.
;; 2004.11.13    Modified for Allegro 7.0
;; 2004.02.23    Usage of Franz LXML format and the parser is undeployed.
;; 2004.01.09    Usage of Franz LXML format and the parser is decided.
;; 2002.09.04    File created
;;; ==================================================================================

(cl:provide :xsd)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :swclospackages)
  ) ; end of eval-when

(in-package :xsd)

;;;
;;;; XML Built-in Datatypes
;;; 
;;; Followings shows xsd type subtype relation, cf. http://www.w3.org/TR/2001/REC-xmlschema-2-20010502.
;;; ----------------------------------------------------------------------------------
;;; xsd:anySimpleType
;;;       |
;;;       +- xsd:boolean
;;;       +-xsd:anyURI
;;;       +-xsd:string
;;;       +-xsd:float
;;;       +-xsd:double
;;;       +-xsd:decimal -+- xsd:integer +- xsd:long -- xsd:int -- xsd:short -- xsd:byte
;;;                                     +-xsd:nonPositiveInteger -- xsd:negativeInteger
;;;                                     +-xsd:nonNegativeInteger --+
;;;                                                                |
;;;       +--------------------------------------------------------+
;;;       +-- xsd:positiveInteger
;;;       +-- xsd:unsignedLong - xsd:unsignedInt - xsd:unsignedShort - xsd:unsignedByte
;;; ----------------------------------------------------------------------------------
;;; Note that type hierarchy of number in Lisp is as follows.
;;; ----------------------------------------------------------------------------------
;;; cl:number -+- cl:real -+- cl:rational -+- cl:integer -+- cl:bignum
;;;            |           |               |              +- cl:fixnum -- cl:bit
;;;            |           |               +- cl:ratio
;;;            |           |
;;;            |           +- cl:float -+- cl:short-float
;;;            |                        +- cl:single-float
;;;            |                        +- cl:double-float
;;;            |                        +- cl:long-float
;;;            +- cl:complex
;;; ----------------------------------------------------------------------------------
;;; In Lisp, an input token is internalized to a lisp object in read process of REPL. 
;;; For instance, a token that eveloped by double quotations is converted to a lisp 
;;; string object typed to cl:string. A token '1' is converted to an object of lisp 
;;; typed to cl:fixnum. A greater number expression than <most-positive-fixnum> in lisp 
;;; is internalized to an object typed to cl:bignum. In RDF semantics, a string and 
;;; integer denotes itself in the RDF universe like lisp. Note that RDF semantics does 
;;; not have the notion of internalization and externalization. We mapped the denotation 
;;; of plane literal of rdf:Literal to internalized data object in lisp, and the 
;;; denotation of xsd typed data to an instance object of xsd datatype. Note also that 
;;; Java provides automatic data conversion between raw data and wrapped object data 
;;; (boxing and unboxing). In SWCLOS, the function <value-of> is used to get 
;;; internalized lisp data from wrapping xsd object.
;;;
;;;; Mapping from Common Lisp Datatypes to RDF Datatypes
;;;
;;; There is no complex number in xsd. Therefore, we ignore cl:complex.
;;; * cl:single-float is mapped to xsd:float.
;;; * cl:double-float is mapped to xsd:double.
;;; * cl:integer is mapped to xsd:integer. 
;;; * The range of cl:bignum and cl:fixnum is mapped to the range of xsd:long, xsd:int, 
;;;   xsd:short, and xsd:byte according to the definition of range of each datatype.
;;; * cl:rational is mapped to xsd:decimal.
;;; The mapping of xsd:decimal is not straightforward. It may be lexically expressed by 
;;; an integer, or any number of decimal digits with a point and succeeding any number 
;;; of fractional digits. If the number of digits is finite (it is so in practice), the 
;;; xsd:decimal value range is included in cl:rational? in Common Lisp. If the number of 
;;; digits could be infinite, the xsd:decimal value range is equivalent to cl:rational. 
;;; Therefore, xsd:decimal is mapped to cl:rational. However, lisp internalizes simple 
;;; input number expressions of digits with a point to an appropriate cannonical value, 
;;; e.g., bignum or float. Then, you need to explicitly designate a rational value like 
;;; ``(rational nnn.mmm)'' if you want to set it as type xsd:decimal instead of xsd:float. 
;;; Note also a float number has a limit for the precision. See the followings.
;;; ----------------------------------------------------------------------------------
;;; cg-user(22): 1000000.1
;;; 1000000.1
;;; cg-user(23): 100000000.1
;;; 1.0e+8
;;; cg-user(24): (rational 1000000.1)
;;; 8000001/8
;;; cg-user(25): (rational 100000000.1)
;;; 100000000
;;; ----------------------------------------------------------------------------------
;;;
;;; The followings are examples of xsd data types. All of following forms return true.
;;; ----------------------------------------------------------------------------------
;;; (cl:typep 1 'xsd:positiveInteger)
;;; (cl:typep -1 'xsd:negativeInteger)
;;; (cl:typep 0 'xsd:nonNegativeInteger)
;;; (cl:typep 0 'xsd:nonPositiveInteger)
;;; (cl:typep 32767 'xsd:short)
;;; (cl:typep 32768 'xsd:int)
;;; (cl:typep 2147483647 'xsd:int)
;;; (cl:typep 2147483648 'xsd:long)
;;; (cl:typep 9223372036854775807 'xsd:long)
;;; (cl:typep 9223372036854775808 'xsd:integer)
;;; (cl:typep 1 'xsd:decimal)
;;; (cl:typep 1.0e0 'xsd:float)
;;; (cl:typep 1.0d0 'xsd:double)
;;; (cl:typep (rational 1) 'xsd:decimal)
;;; (cl:typep (rational 0.000001) 'xsd:decimal)
;;; (cl:typep 0.000001 'xsd:float)
;;; (cl:typep "string?" 'xsd:string)
;;; (cl:typep (iri "http://somewhere") 'xsd:anyURI)
;;; (cl:typep 'xsd:false 'xsd:boolean)
;;; ----------------------------------------------------------------------------------
;;; See also function <type-of> in GxType module.
;;; See also GxType module with respect to the lexical space representation.
;;; See also <disjoint-p> on the discussion on disjointness of xsd datatypes.

(cl:deftype unsignedByte () "0 &lt;= x &lt;= 255" '(cl:unsigned-byte 8))
(cl:deftype unsignedShort () "0 &lt;= x &lt;= 65535" '(cl:unsigned-byte 16))
(cl:deftype unsignedInt () "0 &lt;= x &lt;= 4294967295" '(cl:unsigned-byte 32))
(cl:deftype unsignedLong () "0 &lt;= x &lt;= 18446744073709551615" '(cl:unsigned-byte 64))
;;cf. http://www.franz.com/support/documentation/8.0/ansicl/dictentr/unsigned.htm

(cl:deftype byte () "-128 &lt;= x &lt;= 127" '(cl:signed-byte 8))
(cl:deftype short () "-32768 &lt;= x &lt;= 32767" '(cl:signed-byte 16))
(cl:deftype int () "-2147483648 &lt;= x &lt;= 2147483647" '(cl:signed-byte 32))
(cl:deftype long () "-9223372036854775808 &lt;= x &lt;= 9223372036854775807" '(cl:signed-byte 64))
;;cf. http://www.franz.com/support/documentation/8.0/ansicl/dictentr/signed-b.htm
(cl:deftype integer () "any number of digits without point but with or without + or -" 'cl:integer)

(cl:deftype positiveInteger () "integer greater than 0" '(cl:integer 1 cl:*))
(cl:deftype nonPositiveInteger () "integer smaller than 1" '(cl:integer cl:* 0))
(cl:deftype negativeInteger () "integer smaller than 0" '(cl:integer cl:* -1))
(cl:deftype nonNegativeInteger () "integer greater than -1" '(cl:integer 0 cl:*))

(cl:deftype float () "floating point number in lisp" 'cl:single-float)
(cl:deftype double () "double floating point number in lisp" 'cl:double-float)

(cl:deftype decimal () "rational number in lisp, which should be revised in future." 'cl:rational)

(cl:deftype string () "string in lisp" 'cl:string)

(cl:deftype boolean () "xsd:true or xsd:false" '(cl:member xsd:true xsd:false))

(cl:deftype anyURI () "net.uri:rui in ACL" 'net.uri:uri)

(cl:deftype anySimpleType () "xsd:boolean, xsd:anyURI, xsd:string, xsd:float, xsd:double, or xsd:decimal"
  '(cl:or boolean anyURI string float double decimal))

;(cl:declaim (cl:special nonPositiveInteger nonNegativeInteger anySimpleType))

#|
;-------------------------------------------
(cl:subtypep 'xsd:unsignedByte 'xsd:unsignedShort)
(cl:subtypep 'xsd:unsignedByte 'xsd:unsignedInt)
(cl:subtypep 'xsd:unsignedByte 'xsd:unsignedLong)
(cl:subtypep 'xsd:unsignedByte 'xsd:nonNegativeInteger)
(cl:subtypep 'xsd:unsignedByte 'xsd:integer)
(cl:subtypep 'xsd:unsignedByte 'xsd:decimal)
(cl:subtypep 'xsd:unsignedByte 'xsd:anySimpleType)

(cl:subtypep 'xsd:unsignedShort 'xsd:unsignedInt)
(cl:subtypep 'xsd:unsignedShort 'xsd:unsignedLong)
(cl:subtypep 'xsd:unsignedShort 'xsd:nonNegativeInteger)
(cl:subtypep 'xsd:unsignedShort 'xsd:integer)
(cl:subtypep 'xsd:unsignedShort 'xsd:decimal)
(cl:subtypep 'xsd:unsignedShort 'xsd:anySimpleType)

(cl:subtypep 'xsd:unsignedInt 'xsd:unsignedLong)
(cl:subtypep 'xsd:unsignedInt 'xsd:nonNegativeInteger)
(cl:subtypep 'xsd:unsignedInt 'xsd:integer)
(cl:subtypep 'xsd:unsignedInt 'xsd:decimal)
(cl:subtypep 'xsd:unsignedInt 'xsd:anySimpleType)

(cl:subtypep 'xsd:unsignedLong 'xsd:nonNegativeInteger)
(cl:subtypep 'xsd:unsignedLong 'xsd:integer)
(cl:subtypep 'xsd:unsignedLong 'xsd:decimal)
(cl:subtypep 'xsd:unsignedLong 'xsd:anySimpleType)

(cl:subtypep 'xsd:nonNegativeInteger 'xsd:integer)
(cl:subtypep 'xsd:nonNegativeInteger 'xsd:decimal)
(cl:subtypep 'xsd:nonNegativeInteger 'xsd:anySimpleType)

(cl:subtypep 'xsd:integer 'xsd:decimal)
(cl:subtypep 'xsd:integer 'xsd:anySimpleType)

(cl:subtypep 'xsd:decimal 'xsd:anySimpleType)
;-------------------------------------------
(cl:subtypep 'xsd:byte 'xsd:short)
(cl:subtypep 'xsd:byte 'xsd:int)
(cl:subtypep 'xsd:byte 'xsd:long)
(cl:subtypep 'xsd:byte 'xsd:integer)
(cl:subtypep 'xsd:byte 'xsd:decimal)
(cl:subtypep 'xsd:byte 'xsd:anySimpleType)

(cl:subtypep 'xsd:short 'xsd:int)
(cl:subtypep 'xsd:short 'xsd:long)
(cl:subtypep 'xsd:short 'xsd:integer)
(cl:subtypep 'xsd:short 'xsd:decimal)
(cl:subtypep 'xsd:short 'xsd:anySimpleType)

(cl:subtypep 'xsd:int 'xsd:long)
(cl:subtypep 'xsd:int 'xsd:integer)
(cl:subtypep 'xsd:int 'xsd:decimal)
(cl:subtypep 'xsd:int 'xsd:anySimpleType)

(cl:subtypep 'xsd:long 'xsd:integer)
(cl:subtypep 'xsd:long 'xsd:decimal)
(cl:subtypep 'xsd:long 'xsd:anySimpleType)
;-------------------------------------------
(cl:subtypep 'xsd:positiveInteger 'xsd:nonNegativeInteger)
(cl:subtypep 'xsd:positiveInteger 'xsd:integer)
(cl:subtypep 'xsd:positiveInteger 'xsd:decimal)
(cl:subtypep 'xsd:positiveInteger 'xsd:anySimpleType)

(cl:subtypep 'xsd:negativeInteger 'xsd:nonPositiveInteger)
(cl:subtypep 'xsd:negativeInteger 'xsd:integer)
(cl:subtypep 'xsd:negativeInteger 'xsd:decimal)
(cl:subtypep 'xsd:negativeInteger 'xsd:anySimpleType)

(cl:subtypep 'xsd:nonPositiveInteger 'xsd:integer)
(cl:subtypep 'xsd:nonPositiveInteger 'xsd:decimal)
(cl:subtypep 'xsd:nonPositiveInteger 'xsd:anySimpleType)
;-------------------------------------------
(cl:subtypep 'xsd:boolean 'xsd:anySimpleType)
(cl:subtypep 'xsd:anyURI 'xsd:anySimpleType)
(cl:subtypep 'xsd:string 'xsd:anySimpleType)
(cl:subtypep 'xsd:float 'xsd:anySimpleType)
(cl:subtypep 'xsd:double 'xsd:anySimpleType)
|#

;; End of module
;; --------------------------------------------------------------------
;;;
;;; Seiji Koide Sep-11-2008
;;;
