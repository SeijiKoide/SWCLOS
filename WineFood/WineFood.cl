;;;
;;; Wine ontology in RDFS
;;;

(in-package gx-user)

;; This code directly read from food and wine repository via the Internet.
#|
(eval-when (:execute :load-toplevel :compile-toplevel)
  (require :aserve)
  )
(multiple-value-bind (response code headers uri)
    ;; note that url address and ontology address in contents mismatch
    (net.aserve.client:do-http-request "http://www.w3.org/TR/2004/REC-owl-guide-20040210/food.rdf"
      :proxy "150.73.3.41")
  (cond ((eq code 200)
         (gx::read-rdf-from-string #'addRdfXml response)
         (values))
        (t cl:nil)))
(multiple-value-bind (response code headers uri)
    ;; note that url address and ontology address in contents mismatch
    (net.aserve.client:do-http-request "http://www.w3.org/TR/2004/REC-owl-guide-20040210/wine.rdf"
      :proxy "150.73.3.41")
  (cond ((eq code 200)
         (gx::read-rdf-from-string #'addRdfXml response)
         (values))
        (t cl:nil)))
|#
#|
:cd ../../allegro-projects/SWCLOS/winefood
(read-rdf-file #'addRdfXml "wine.rdf")
|#
#|
;; This code reads from local files.
(let ((*default-pathname-defaults* *load-pathname*))
  (read-rdf-file #'addRdfXml "wine.rdf")
  (read-rdf-file #'addRdfXml "food.rdf")
  :done)
|#
#|
(defIndividual Unknown (rdf:type vin:Wine)
  (vin:madeFromGrape vin:ZinfandelGrape))
|#
(let ((*default-pathname-defaults* *load-pathname*))
(time
 (let ((*error-output* cl:nil))
   (setq *autoepistemic-local-closed-world* cl:nil)
   (read-rdf-file #'addRdfXml "wine.rdf")
   (read-rdf-file #'addRdfXml "food.rdf")
   (setq *autoepistemic-local-closed-world* t)
   ))
)
