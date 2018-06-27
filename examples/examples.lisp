
(cl:defpackage :abcl-cdk-examples
  (:use :common-lisp :abcl-cdk))

(cl:in-package :abcl-cdk-examples)

(jimport |java.awt| |Color|)
(jimport |java.io| |FileWriter|)

(jimport |org.openscience.cdk.geometry.cip| |CIPTool|)
(jimport |org.openscience.cdk.config| |Isotopes|)
(jimport |org.openscience.cdk.tools.manipulator| |MolecularFormulaManipulator|)
(jimport |org.openscience.cdk| |DefaultChemObjectBuilder|)
(jimport |org.openscience.cdk.modeling.builder3d| |ModelBuilder3D|)
(jimport |org.openscience.cdk.io| |CMLWriter|)


(defun example-file (name)
  (merge-pathnames name abcl-cdk-examples-config:*base-directory*))

(defparameter *valine* (read-smiles-string "CC(C)[C](C(=O)O)N"))
(mol-to-svg *valine* (example-file "valine.svg"))

(defparameter *l-valine* (read-smiles-string "CC(C)[C@@H](C(=O)O)N"))
(write-smiles-string *l-valine*)
(write-chiral-smiles-string *l-valine*)

(defparameter *d-valine* (read-smiles-string "CC(C)[C@H](C(=O)O)N"))
(write-smiles-string *d-valine*)
(write-chiral-smiles-string *d-valine*)

(mol-to-svg *l-valine* (example-file "l-valine.svg"))
(mol-to-pdf *l-valine* (example-file "l-valine.pdf"))

(mol-to-pdf (read-smiles-string "CC(C)[C@@H](C(=O)O)N") (example-file "l-valine.pdf"))

(defparameter *caffeine* (read-smiles-string "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
(mol-to-svg *caffeine* (example-file "caffeine.svg"))
(mol-to-pdf *caffeine* (example-file "caffeine.pdf"))

(defparameter *ticagrelor* (read-smiles-string "CCCSC1=NC2=C(C(=N1)N[C@@H]3C[C@H]3C4=CC(=C(C=C4)F)F)N=NN2[C@@H]5C[C@@H]([C@H]([C@H]5O)O)OCCO"))
(mol-to-svg *ticagrelor* (example-file "ticagrelor.svg"))
(mol-to-pdf *ticagrelor* (example-file "ticagrelor.pdf"))

(defparameter *z-tamoxifen* (read-smiles-string "CC/C(=C(\\C1=CC=CC=C1)/C2=CC=C(C=C2)OCCN(C)C)/C3=CC=CC=C3"))
(mol-to-svg *z-tamoxifen* (example-file "z-tamoxifen.svg"))
(mol-to-pdf *z-tamoxifen* (example-file "z-tamoxifen.pdf"))

(defparameter *tamoxifen* (read-smiles-string "CCC(=C(C1=CC=CC=C1)C2=CC=C(C=C2)OCCN(C)C)C3=CC=CC=C3"))
(mol-to-svg *tamoxifen* (example-file "tamoxifen.svg"))
(mol-to-pdf *tamoxifen* (example-file "tamoxifen.pdf"))

(defparameter *alanine* (read-smiles-string "C[C@@H](C(=O)O)N"))
(mol-to-pdf *alanine* (example-file "alanine.pdf"))

(defparameter *t2* (read-smiles-string "CCCSC1=NC2=C(C(=N1)NC3CC3C4=CC(=C(C=C4)F)F)N=NN2C5CC(C(C5O)O)OCCO"))
(mol-to-pdf *t2* (example-file "t2.pdf"))

(get-inchi *ticagrelor*)
(get-inchi *caffeine*)
(get-inchi *t2*)
(get-inchi *z-tamoxifen*)
(get-inchi-key *z-tamoxifen*)
(get-inchi-key *tamoxifen*)

(get-inchi (read-smiles-string "c1ccccc1"))

(get-inchi (read-smiles-string "CC(C)C"))

(defparameter *benzene* (read-smiles-string "c1ccccc1"))
(defparameter *benzene-2* (read-smiles-string "C1=CC=CC=C1"))
(defparameter *pyridine* (read-smiles-string "n1ccccc1"))

(get-inchi *pyridine*)
(get-inchi *benzene*)
(get-inchi *benzene-2*)

(get-inchi (read-smiles-string "N1=CC=CC=C1"))

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

(mol-to-svg *d-valine* (example-file "d-valine.svg"))
(mol-to-pdf *d-valine* (example-file "d-valine.pdf"))

(defparameter *but-2-ene* (read-smiles-string "CC=CC"))

(defparameter *e-but-2-ene* (read-smiles-string "[H]\\C(C)=C(\\[H])C"))
(mol-to-svg *e-but-2-ene* (example-file "e-but-2-ene.svg") :width 128 :height 128)
(mol-to-pdf *e-but-2-ene* (example-file "e-but-2-ene.pdf") :width 128 :height 128)

(defparameter *z-but-2-ene* (read-smiles-string "[H]/C(C)=C(\\[H])C"))
(mol-to-svg *z-but-2-ene* (example-file "z-but-2-ene.svg") :width 128 :height 128)
(mol-to-pdf *z-but-2-ene* (example-file "z-but-2-ene.pdf") :width 128 :height 128)

(let ((abcl-cdk::*background-color* (java:jfield |Color| "black"))
      (abcl-cdk::*default-bond-color* (java:jfield |Color| "white")))
  (mol-to-svg *ticagrelor* (example-file "ticagrelor-inverted.svg")))

(defparameter *but-2-ene* (read-smiles-string "C[C:1]=[C:2]C"))
(mol-to-svg *but-2-ene* (example-file "but-2-ene.svg") :width 128 :height 128)

(mapcar #'get-atom-atom-mapping (items (#"atoms" *but-2-ene*)))

(defparameter *iso-fac* (java:jstatic "getInstance" |Isotopes|))
(defparameter *butane* (read-smiles-string "CCCC"))
(defparameter *butane-mf* (java:jstatic "getMolecularFormula" |MolecularFormulaManipulator| *butane*))

;; (#"getIsotopeCount" *butane-mf*)
;; (#"getIsotope" *iso-fac* "C" 12)
;; (#"getIsotopeCount" *butane-mf* (#"getIsotope" *iso-fac* "C" 12))


(let ((mb3d (java:jstatic "getInstance" |ModelBuilder3D| (java:jstatic "getInstance" |DefaultChemObjectBuilder|)))
      (ac (abcl-cdk:copy-atom-container *butane*)))
  (let ((ac3d (#"generate3DCoordinates" mb3d ac java:+false+)))
    (let* ((f (java:jnew |FileWriter| "molecule.cml"))
           (cmlw (java:jnew |CMLWriter| f)))
      (#"write" cmlw ac3d)
      (#"close" cmlw))
 ;; * <pre>
 ;; *   FileWriter output = new FileWriter("molecule.cml");
 ;; *   CMLWriter cmlwriter = new CMLWriter(output);
 ;; *   cmlwriter.write(molecule);
 ;; *   cmlwriter.close();
 ;; * </pre>


    ac3d))

(jimport |org.openscience.cdk.depict| |DepictionGenerator|)

(let ((mol (read-smiles-string "CC(C)[C](C(=O)O)N")))
  (let ((dg (let ((dg (java:jnew |DepictionGenerator|)))
              (java:jcall "withAtomColors"
                          (java:jcall "withSize" dg 128 128)))))
    (let ((depiction (java:jcall "depict" dg mol)))
      (java:jcall "writeTo" depiction (namestring (example-file "foo.svg")))
      (java:jcall "writeTo" depiction (namestring (example-file "foo.pdf"))))))

