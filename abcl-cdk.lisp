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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (jimport |javax.vecmath| |Point2d|)
  (jimport |javax.vecmath| |Vector2d|)

  (jimport |org.openscience.cdk| |Atom|)

  (jimport |org.openscience.cdk.ringsearch| |AllRingsFinder|)

  (jimport |org.openscience.cdk.interfaces| |IBond|)
  (jimport |org.openscience.cdk.interfaces| |IBond$Stereo|)
  (jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality|)
  (jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality$Stereo|)

  (jimport |org.openscience.cdk.smiles| |SmilesParser|)
  (jimport |org.openscience.cdk.smiles| |SmilesGenerator|)
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
  (jimport |java.awt.geom| |Rectangle2D$Double|))


;;
;; Some lisp-side constants
(defconstant +clockwise+ (java:jfield |ITetrahedralChirality$Stereo| "CLOCKWISE"))
(defconstant +anti-clockwise+ (java:jfield |ITetrahedralChirality$Stereo| "ANTI_CLOCKWISE"))
(defconstant +up+ (java:jfield |IBond$Stereo| "UP"))
(defconstant +down+ (java:jfield |IBond$Stereo| "DOWN"))

;;
;; General Lisp-side Java Utility Routines
(defun items (iterable)
  (let ((iterator (#"iterator" iterable)))
    (loop while (#"hasNext" iterator)
          collect (#"next" iterator))))


;;
;; Atom Container Utility Functions
(defun get-atom (atom-container atom-number)
  (#"getAtom" atom-container atom-number))

(defun get-atoms-of-symbol (ac symbol)
  (loop for atom in (items (#"atoms" ac))
     for s = (#"getSymbol" atom)
     when (equal s symbol)
     collect atom))

;; 
;; Use #"getConnectedBondsList" instead!!!
#+nil
(defun get-bonds-containing-atom (ac atom)
  (loop for bond in (items (#"bonds" ac))
     when (progn
            (member atom (items (#"atoms" bond)) :test 'equalp))
     collect bond))

;;
;; Use #"getConnectedAtomsList" instead!!!
(defun get-neighbors (ac atom)
  (let ((bonds (#"getConnectedBondsList" ac atom)))
    (loop for bond in bonds
       collect (remove atom (items (#"atoms" bond)) :test 'equalp))))

(defun get-largest-ring (ac)
  (let* ((arf (java:jnew |AllRingsFinder|))
         (rs (#"findAllRings" arf ac)))
  (loop for ring in (items (#"atomContainers" rs))
     maximizing (#"getRingSize" ring)
     finally (return ring))))

;;
;; SMILES Parsing Routines
(defparameter *smiles-parser*
  (java:jnew |SmilesParser|
             (java:jcall
              (java:jmethod |DefaultChemObjectBuilder| "getInstance")
              nil)))

(defparameter *smiles-generator*
  (java:jnew |SmilesGenerator|))

(defparameter *isomeric-smiles-generator*
  ;; for John May's master+ branch, we need to use isomeric, not isomericGenerator. argh...
  (java:jstatic "isomeric" |SmilesGenerator|)
  #+nil
  (java:jstatic "isomericGenerator" |SmilesGenerator|))

(defun parse-smiles-string (smiles-string)
  (#"parseSmiles" *smiles-parser* smiles-string))

(defun generate-smiles-string (atom-container)
  (#"createSMILES" *smiles-generator* atom-container))

(defun generate-chiral-smiles-string (atom-container)
  (#"createSMILES" *isomeric-smiles-generator* atom-container))


;;
;; Atom Container Rendering Support

(defun get-point2d (atom)
  (#"getPoint2d" atom))

(defparameter *renderer-generators*
  (jlist (java:jnew |BasicSceneGenerator|)
         (java:jnew |BasicBondGenerator|)
         (java:jnew |BasicAtomGenerator|)))

(defparameter *atom-container-renderer* (java:jnew |AtomContainerRenderer|
                                                   *renderer-generators*
                                                   (java:jnew |AWTFontManager|)))

(defun atom-container-atom-positions (ac)
  (loop for atom in (items (#"atoms" ac))
     collect (let ((oldx  (java:jfield "x" (#"getPoint2d" atom)))
                   (oldy (java:jfield "y" (#"getPoint2d" atom))))
               (cons oldx oldy))))

(defun atom-container-atom-extrema (ac)
  (loop for (x . y) in
       (loop for atom in (items (#"atoms" ac))
          collect (cons (java:jfield "x" (#"getPoint2d" atom))
                        (java:jfield "y" (#"getPoint2d" atom))))
     minimizing x into xmin
     maximizing x into xmax
     minimizing y into ymin
     maximizing y into ymax
     finally (return (list (cons xmin xmax) (cons ymin ymax)))))

(defun flip-atom-container-bonds (ac)
  (loop for bond in (items (#"bonds" ac))
     do (let ((stereo (#"getStereo" bond)))
          (cond ((equalp stereo +up+)
                 (#"setStereo" bond +down+))
                ((equalp stereo +down+)
                 (#"setStereo" bond +up+)))))
  ac)

(defun flip-atom-container-horizontal (ac)
  (destructuring-bind ((xmin . xmax) (ymin . ymax))
      (atom-container-atom-extrema ac)
    (declare (ignore ymin ymax))
    (loop for atom in (items (#"atoms" ac))
       do (let ((oldx  (java:jfield "x" (#"getPoint2d" atom))))
            (setf (java:jfield "x" (#"getPoint2d" atom))
                  (+ xmin (- xmax oldx)))))
    (flip-atom-container-bonds ac)
    ac))

(defun flip-atom-container-vertical (ac)
  (destructuring-bind ((xmin . xmax) (ymin . ymax))
      (atom-container-atom-extrema ac)
    (declare (ignore xmin xmax))
    (loop for atom in (items (#"atoms" ac))
       do (let ((oldy (java:jfield "y" (#"getPoint2d" atom))))
            (setf (java:jfield "y" (#"getPoint2d" atom))
                  (+ ymin (- ymax oldy)))))
    (flip-atom-container-bonds ac)
    ac))

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

