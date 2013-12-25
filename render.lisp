;;; file: render.lisp
;;;
;;; Copyright (c) 2012-2013 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (jimport |javax.vecmath| |Vector2d|)

  (jimport |org.openscience.cdk| |Atom|)

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
  (jimport |java.awt.geom| |Rectangle2D$Double|))

;;
;; Atom Container Rendering Support

(defparameter *renderer-generators*
  (jlist (java:jnew |BasicSceneGenerator|)
         (java:jnew |BasicBondGenerator|)
         (java:jnew |BasicAtomGenerator|)))

(defparameter *atom-container-renderer* (java:jnew |AtomContainerRenderer|
                                                   *renderer-generators*
                                                   (java:jnew |AWTFontManager|)))

(defun prepare-atom-container-for-rendering (ac &key (angle 0d0) flip)
  (#"generateCoordinates" (java:jnew |StructureDiagramGenerator| ac)
                          (java:jnew (java:jconstructor |Vector2d| 2)
                                     (cos angle) (sin angle)))
  (case flip
    (:vertical (flip-atom-container-vertical ac))
    (:horizontal (flip-atom-container-horizontal ac))
    (:both (flip-atom-container-vertical ac)
           (flip-atom-container-horizontal ac))))

(defun mol-to-graphics (mol renderer graphics width height x-margin y-margin)
  (let ((draw-visitor (java:jnew |AWTDrawVisitor| graphics)))
    (#"startExport" graphics)
    (#"setup" renderer mol (java:jnew |Rectangle| 0 0 (1- width) (1- height)))
    (#"paint" renderer mol draw-visitor
              (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                         x-margin y-margin (- width (* x-margin 2)) (- height (* y-margin 2)))
              java:+true+)
    (#"endExport" graphics)))

(defun draw-atom-container-to-svg (mol pathname width height x-margin y-margin)
  (with-open-file (out-stream pathname :direction :output
                              :if-exists :supersede
                              :element-type :default)
    (let ((graphics (java:jnew |SVGGraphics2D|
                               (#"getWrappedOutputStream" out-stream)
                               (java:jnew |Dimension| width height))))
      (mol-to-graphics mol *atom-container-renderer* graphics width height x-margin y-margin))))

(defun draw-atom-container-to-pdf (mol pathname width height x-margin y-margin)
  (with-open-file (out-stream pathname :direction :output
                              :if-exists :supersede
                              :element-type :default)
    (let ((graphics (java:jnew |PDFGraphics2D|
                               (#"getWrappedOutputStream" out-stream)
                               (java:jnew |Dimension| width height))))
      (mol-to-graphics mol *atom-container-renderer* graphics width height x-margin y-margin))))

(defun mol-to-svg (mol pathname &key (width 512) (height 512) (margin 10)
                                     (x-margin margin) (y-margin margin) (angle 0d0) flip)
  (let ((mol (#"clone" mol)))
    (prepare-atom-container-for-rendering mol :angle angle :flip flip)
    (draw-atom-container-to-svg mol pathname width height x-margin y-margin)))

(defun mol-to-pdf (mol pathname &key (width 512) (height 512) (margin 10)
                                     (x-margin margin) (y-margin margin) (angle 0d0) flip)
  (let ((mol (#"clone" mol)))
    (prepare-atom-container-for-rendering mol :angle angle :flip flip)
    (draw-atom-container-to-pdf mol pathname width height x-margin y-margin)))

