;;; file: abcl-cdk.lisp
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

(cl:in-package :abcl-cdk)

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

(jimport |java.awt| |Rectangle|)
(jimport |java.awt| |Dimension|)
(jimport |java.awt.geom| |Rectangle2D$Double|)

(defparameter *smiles-parser*
  (java:jnew |SmilesParser|
             (java:jcall
              (java:jmethod |DefaultChemObjectBuilder| "getInstance")
              nil)))

(defparameter *renderer-generators*
  (jlist (java:jnew |BasicAtomGenerator|)
         (java:jnew |BasicBondGenerator|)
         (java:jnew |BasicSceneGenerator|)))

(defun parse-smiles-string (smiles-string)
  (#"parseSmiles" *smiles-parser* smiles-string))

(defun mol-to-svg (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let* ((renderer (java:jnew |AtomContainerRenderer|
                                *renderer-generators*
                                (java:jnew |AWTFontManager|)))
           (graphics (java:jnew |SVGGraphics2D|
                                (#"getWrappedOutputStream" out-stream)
                                (java:jnew |Dimension| 512 512)))
           (draw-visitor (java:jnew |AWTDrawVisitor| graphics)))
      (#"startExport" graphics)
      (#"generateCoordinates" (java:jnew |StructureDiagramGenerator| mol))
      (#"setup" renderer mol (java:jnew |Rectangle| 0 0 511 511))
      (#"paint" renderer mol draw-visitor
                (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                           10 10 491 491)
                java:+true+)
      (#"endExport" graphics))))

(defun mol-to-pdf (mol pathname)
  (with-open-file (out-stream pathname :direction :output
                                       :if-exists :supersede
                                       :element-type :default)
    (let* ((renderer (java:jnew |AtomContainerRenderer|
                                *renderer-generators*
                                (java:jnew |AWTFontManager|)))
           (graphics (java:jnew |PDFGraphics2D|
                                (#"getWrappedOutputStream" out-stream)
                                (java:jnew |Dimension| 512 512)))
           (draw-visitor (java:jnew |AWTDrawVisitor| graphics)))
      (#"startExport" graphics)
      (#"generateCoordinates" (java:jnew |StructureDiagramGenerator| mol))
      (#"setup" renderer mol (java:jnew |Rectangle| 0 0 511 511))
      (#"paint" renderer mol draw-visitor
                (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                           10 10 491 491)
                java:+true+)
      (#"endExport" graphics))))

