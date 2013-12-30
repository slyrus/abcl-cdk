
(cl:defpackage :abcl-cdk-examples
  (:use :common-lisp :abcl-cdk))

(cl:in-package :abcl-cdk-examples)

(jimport |org.openscience.cdk.geometry.cip| |CIPTool|)

#+nil (named-readtables:in-readtable abcl-cdk::abcl-cdk-readtable)

(defparameter *valine* (parse-smiles-string "CC(C)[C](C(=O)O)N"))

(defparameter *l-valine* (parse-smiles-string "CC(C)[C@@H](C(=O)O)N"))
(abcl-cdk::generate-smiles-string *l-valine*)
(abcl-cdk::generate-chiral-smiles-string *l-valine*)

(defparameter *d-valine* (parse-smiles-string "CC(C)[C@H](C(=O)O)N"))
(abcl-cdk::generate-smiles-string *d-valine*)
(abcl-cdk::generate-chiral-smiles-string *d-valine*)

(defun example-file (name)
  (merge-pathnames name abcl-cdk-examples-config:*base-directory*))

(mol-to-svg *l-valine* (example-file "l-valine.svg"))
(mol-to-pdf *l-valine* (example-file "l-valine.pdf"))

(mol-to-pdf (parse-smiles-string "CC(C)[C@@H](C(=O)O)N") (example-file "l-valine.pdf"))

(defparameter *caffeine* (parse-smiles-string "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
(mol-to-svg *caffeine* (example-file "caffeine.svg"))
(mol-to-pdf *caffeine* (example-file "caffeine.pdf"))

(defparameter *ticagrelor* (parse-smiles-string "CCCSC1=NC2=C(C(=N1)N[C@@H]3C[C@H]3C4=CC(=C(C=C4)F)F)N=NN2[C@@H]5C[C@@H]([C@H]([C@H]5O)O)OCCO"))
(mol-to-svg *ticagrelor* (example-file "ticagrelor.svg"))
(mol-to-pdf *ticagrelor* (example-file "ticagrelor.pdf"))

(defparameter *z-tamoxifen* (parse-smiles-string "CC/C(=C(\\C1=CC=CC=C1)/C2=CC=C(C=C2)OCCN(C)C)/C3=CC=CC=C3"))
(mol-to-svg *z-tamoxifen* (example-file "z-tamoxifen.svg"))
(mol-to-pdf *z-tamoxifen* (example-file "z-tamoxifen.pdf"))

(defparameter *tamoxifen* (parse-smiles-string "CCC(=C(C1=CC=CC=C1)C2=CC=C(C=C2)OCCN(C)C)C3=CC=CC=C3"))
(mol-to-svg *tamoxifen* (example-file "tamoxifen.svg"))
(mol-to-pdf *tamoxifen* (example-file "tamoxifen.pdf"))

(defparameter *alanine* (parse-smiles-string "C[C@@H](C(=O)O)N"))
(mol-to-pdf *alanine* (example-file "alanine.pdf"))

(defparameter *t2* (parse-smiles-string "CCCSC1=NC2=C(C(=N1)NC3CC3C4=CC(=C(C=C4)F)F)N=NN2C5CC(C(C5O)O)OCCO"))
(mol-to-pdf *t2* (example-file "t2.pdf"))

(get-inchi *ticagrelor*)
(get-inchi *caffeine*)
(get-inchi *t2*)
(get-inchi *z-tamoxifen*)
(get-inchi-key *z-tamoxifen*)
(get-inchi-key *tamoxifen*)

(get-inchi (parse-smiles-string "c1ccccc1"))

(get-inchi (parse-smiles-string "CC(C)C"))

(defparameter *benzene* (parse-smiles-string "c1ccccc1"))
(defparameter *benzene-2* (parse-smiles-string "C1=CC=CC=C1"))
(defparameter *pyridine* (parse-smiles-string "n1ccccc1"))

(get-inchi *pyridine*)
(get-inchi *benzene*)
(get-inchi *benzene-2*)

(get-inchi (parse-smiles-string "N1=CC=CC=C1"))

(map nil #'describe (items (#"atoms" *benzene-2*)))
(map nil #'describe (items (#"bonds" *benzene-2*)))

(map nil
     (lambda (bond)
       (print (cons (#"getAtom" bond 0)
                    (#"getAtom" bond 1))))
     (items (#"bonds" *l-valine*)))

(let* ((mol *l-valine*)
       (tc (first (items (#"stereoElements" mol)))))
  (describe
   (java:jstatic "getCIPChirality" |CIPTool| mol tc)))

(mol-to-pdf *l-valine* "l-valine.pdf")
(mol-to-pdf *d-valine* "d-valine.pdf")

(defparameter *e-but-2-ene* (parse-smiles-string "[H]\\C(C)=C(\\[H])C"))
(mol-to-svg *e-but-2-ene* "blog/e-but-2-ene.svg" :width 128 :height 128)
(mol-to-pdf *e-but-2-ene* "blog/e-but-2-ene.pdf" :width 128 :height 128)

(defparameter *z-but-2-ene* (parse-smiles-string "[H]/C(C)=C(\\[H])C"))
(mol-to-svg *z-but-2-ene* "blog/z-but-2-ene.svg" :width 128 :height 128)
(mol-to-pdf *z-but-2-ene* "blog/z-but-2-ene.pdf" :width 128 :height 128)
