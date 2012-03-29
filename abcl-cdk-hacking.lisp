
(cl:require 'abcl-contrib)
(cl:require 'abcl-asdf)
(cl:require :extensible-sequences)
(cl:require :java-collections)

(cl:defpackage :abcl-cdk-hacking
  (:use :common-lisp))

(cl:in-package :abcl-cdk-hacking)

(defmacro jimport (package class)
  `(defparameter ,(intern class) (concatenate 'string ,package "." ,class)))

(jimport "org.openscience.cdk.smiles" "SmilesParser")
(jimport "org.openscience.cdk" "DefaultChemObjectBuilder")

(defparameter *smiles-parser*
  (java:jnew "org.openscience.cdk.smiles.SmilesParser"
             (java:jcall
              (java:jmethod (java:jclass "org.openscience.cdk.DefaultChemObjectBuilder")
                            "getInstance")
              nil)))

(defun getmol (smiles-string)
  (java:jcall "parseSmiles" *smiles-parser* smiles-string))

(defun jlist (&rest initial-contents)
  (sequence:make-sequence-like
   (java:jnew "java.util.Vector") (length initial-contents)
   :initial-contents initial-contents))

(defun mol-to-svg (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let*
        ((r (java:jnew
             "org.openscience.cdk.renderer.AtomContainerRenderer"
             (jlist
              (java:jnew "org.openscience.cdk.renderer.generators.BasicAtomGenerator")
              (java:jnew "org.openscience.cdk.renderer.generators.BasicBondGenerator")
              (java:jnew "org.openscience.cdk.renderer.generators.BasicSceneGenerator"))             (java:jnew "org.openscience.cdk.renderer.font.AWTFontManager")))
         (vg (java:jnew "org.freehep.graphicsio.svg.SVGGraphics2D"
                        (java:jcall "getWrappedOutputStream" out-stream)
                        (java:jnew "java.awt.Dimension" 320 320)))
         (adv (java:jnew "org.openscience.cdk.renderer.visitor.AWTDrawVisitor" vg)))
      (java:jcall "startExport" vg)
      (java:jcall "generateCoordinates"
                  (java:jnew "org.openscience.cdk.layout.StructureDiagramGenerator" mol))
      (java:jcall "setup" r mol (java:jnew "java.awt.Rectangle" 0 0 100 100))
      (java:jcall "paint" r mol adv
                  (java:jnew (java:jconstructor "java.awt.geom.Rectangle2D$Double" 4)
                             10 10 300 300)
                  java:+true+)
      (java:jcall "endExport" vg))))

(defun mol-to-pdf (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let*
        ((r (java:jnew
             "org.openscience.cdk.renderer.AtomContainerRenderer"
             (jlist
              (java:jnew "org.openscience.cdk.renderer.generators.BasicAtomGenerator")
              (java:jnew "org.openscience.cdk.renderer.generators.BasicBondGenerator")
              (java:jnew "org.openscience.cdk.renderer.generators.BasicSceneGenerator"))
             (java:jnew "org.openscience.cdk.renderer.font.AWTFontManager")))
         (vg (java:jnew "org.freehep.graphicsio.pdf.PDFGraphics2D"
                        (java:jcall "getWrappedOutputStream" out-stream)
                        (java:jnew "java.awt.Dimension" 320 320)))
         (adv (java:jnew "org.openscience.cdk.renderer.visitor.AWTDrawVisitor" vg)))
      (java:jcall "startExport" vg)
      (java:jcall "generateCoordinates"
                  (java:jnew "org.openscience.cdk.layout.StructureDiagramGenerator" mol))
      (java:jcall "setup" r mol (java:jnew "java.awt.Rectangle" 0 0 100 100))
      (java:jcall "paint" r mol adv
                  (java:jnew (java:jconstructor "java.awt.geom.Rectangle2D$Double" 4)
                             10 10 300 300)
                  java:+true+)
      (java:jcall "endExport" vg))))


(defparameter *tam* (getmol "CCC(=C(C1=CC=CC=C1)C2=CC=C(C=C2)OCCN(C)C)C3=CC=CC=C3"))

(mol-to-svg *tam* "/tmp/tam.svg")

(mol-to-pdf *tam* "/tmp/tam.pdf")
