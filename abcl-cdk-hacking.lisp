;;; file: abcl-cdk-hacking.lisp
;;;
;;; Copyright (c) 2012 Cyrus Harmon (ch-lisp@bobobeach.com)
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require :extensible-sequences)
  (cl:require :java-collections))

(cl:defpackage :abcl-cdk-hacking
  (:use :common-lisp))

(cl:in-package :abcl-cdk-hacking)

(defmacro jimport (java-package class &optional package)
  `(defparameter ,(apply #'intern class
                         (when package (list package)))
     (concatenate 'string (symbol-name (quote ,java-package))
                  "."
                  (symbol-name (quote ,class)))))

(jimport |org.openscience.cdk.smiles| |SmilesParser|)
(jimport |org.openscience.cdk| |DefaultChemObjectBuilder|)

(jimport |org.openscience.cdk.renderer| |AtomContainerRenderer|)
(jimport |org.openscience.cdk.renderer.generators| |BasicAtomGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicBondGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator|)

(jimport |org.openscience.cdk.renderer.font| |AWTFontManager|)
(jimport |org.freehep.graphicsio.svg| |SVGGraphics2D|)
(jimport |org.freehep.graphicsio.pdf| |PDFGraphics2D|)
(jimport |org.openscience.cdk.renderer.visitor| |AWTDrawVisitor|)
(jimport |org.openscience.cdk.layout| |StructureDiagramGenerator|)

(jimport |java.util| |Vector|)
(jimport |java.awt| |Rectangle|)
(jimport |java.awt| |Dimension|)
(jimport |java.awt.geom| |Rectangle2D$Double|)

(defparameter *smiles-parser*
  (java:jnew |SmilesParser|
             (java:jcall
              (java:jmethod (java:jclass |DefaultChemObjectBuilder|)
                            "getInstance")
              nil)))

(defun getmol (smiles-string)
  (java:jcall "parseSmiles" *smiles-parser* smiles-string))

(defun jlist (&rest initial-contents)
  (sequence:make-sequence-like
   (java:jnew |Vector|) (length initial-contents)
   :initial-contents initial-contents))

(defun mol-to-svg (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let*
        ((r (java:jnew |AtomContainerRenderer|
                       (jlist
                        (java:jnew |BasicAtomGenerator|)
                        (java:jnew |BasicBondGenerator|)
                        (java:jnew |BasicSceneGenerator|))
                       (java:jnew |AWTFontManager|)))
         (vg (java:jnew |SVGGraphics2D|
                        (java:jcall "getWrappedOutputStream" out-stream)
                        (java:jnew |Dimension| 320 320)))
         (adv (java:jnew |AWTDrawVisitor| vg)))
      (java:jcall "startExport" vg)
      (java:jcall "generateCoordinates"
                  (java:jnew |StructureDiagramGenerator| mol))
      (java:jcall "setup" r mol (java:jnew |Rectangle| 0 0 100 100))
      (java:jcall "paint" r mol adv
                  (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                             10 10 300 300)
                  java:+true+)
      (java:jcall "endExport" vg))))

(defun mol-to-pdf (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let*
        ((r (java:jnew |AtomContainerRenderer|
                       (jlist
                        (java:jnew |BasicAtomGenerator|)
                        (java:jnew |BasicBondGenerator|)
                        (java:jnew |BasicSceneGenerator|))
                       (java:jnew |AWTFontManager|)))
         (vg (java:jnew |PDFGraphics2D|
                        (java:jcall "getWrappedOutputStream" out-stream)
                        (java:jnew |Dimension| 320 320)))
         (adv (java:jnew |AWTDrawVisitor| vg)))
      (java:jcall "startExport" vg)
      (java:jcall "generateCoordinates"
                  (java:jnew |StructureDiagramGenerator| mol))
      (java:jcall "setup" r mol (java:jnew |Rectangle| 0 0 100 100))
      (java:jcall "paint" r mol adv
                  (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                             10 10 300 300)
                  java:+true+)
      (java:jcall "endExport" vg))))

(defparameter *tam* (getmol "CCC(=C(C1=CC=CC=C1)C2=CC=C(C=C2)OCCN(C)C)C3=CC=CC=C3"))

(mol-to-svg *tam* "/tmp/tam.svg")

(mol-to-pdf *tam* "/tmp/tam.pdf")
