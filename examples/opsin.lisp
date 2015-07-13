
(cl:defpackage :abcl-cdk-opsin-examples
  (:use :cl :abcl-cdk :abcl-cdk-opsin))

(cl:in-package :abcl-cdk-opsin-examples)

(jimport |org.openscience.cdk.isomorphism| |UniversalIsomorphismTester|)

(mol-to-svg
 (read-smiles-string
  (parse-iupac-name-to-smiles
   "(3,3-dimethyl-7-oxo-6-[(2-phenylacetyl)amino]-4-thia-1-azabicyclo[3.2.0]heptane-2- carboxylic acid)"))
 "penicillin-g.svg")

(defparameter *caffeine-from-smiles*
  (read-smiles-string "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))

(mol-to-svg *caffeine-from-smiles* "caffeine-from-smiles.svg")

(defparameter *caffeine-from-iupac*
  (read-smiles-string
   (parse-iupac-name-to-smiles
    "1,3,7-trimethylpurine-2,6-dione")))

(mol-to-svg *caffeine-from-smiles* "caffeine-from-iupac.svg")

(defparameter *universal-isomorphism-tester* (java:jnew |UniversalIsomorphismTester|))
(java:jcall "isIsomorph" *universal-isomorphism-tester* *caffeine-from-smiles* *caffeine-from-iupac*)


(defparameter *hairy-macrocycle*
  (read-smiles-string
   (parse-iupac-name-to-smiles
    "3,6-diamino-N-[[15-amino-11-(2-amino-3,4,5,6-tetrahydropyrimidin-4-yl)-8- [(carbamoylamino)methylidene]-2-(hydroxymethyl)-3,6,9,12,16-pentaoxo- 1,4,7,10,13-pentazacyclohexadec-5-yl]methyl]hexanamide")))
(mol-to-svg *hairy-macrocycle* "hairy-macrocycle.svg")

(defparameter *oxooxa*
  (read-smiles-string
   (parse-iupac-name-to-smiles
    "N-(5-chloro-2-methyl-phenyl)-2-methoxy-N-(2-oxooxazolidin-3-yl)acetamide")))
(mol-to-svg *oxooxa* "oxooxa.svg")

(mol-to-svg
 (read-smiles-string
  (parse-iupac-name-to-smiles
   "benzo[1'',2'':3,4;4'',5'':3',4']dicyclobuta[1,2-b:1',2'-c']difuran"))
 "difuran.svg")

(mol-to-svg
 (read-smiles-string
  (parse-iupac-name-to-smiles
   "naphtho[1,2-a]azulene"))
 "azulene.svg")

;;
;; "6,7-(prop[1]en[1]yl[3]ylidene)benzo[a]cyclohepta[e][8]annulene"
;; this is an IUPAC name that doesn't seem to be supported by OPSIN
(mol-to-svg
 (read-smiles-string
  (parse-iupac-name-to-smiles
   "benzo[a]cyclohepta[e][8]annulene"))
 "annulene.svg")


