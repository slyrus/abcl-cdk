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

(jimport |java.awt| |Color|)
(jimport |java.lang| |String|)
(jimport |javax.vecmath| |Vector2d|)
(jimport |java.io| |ByteArrayOutputStream|)

(jimport |org.openscience.cdk.interfaces| |IAtom|)

(jimport |org.openscience.cdk| |Atom|)
(jimport |org.openscience.cdk.graph| |ConnectivityChecker|)

(jimport |org.openscience.cdk.renderer| |AtomContainerRenderer|)
(jimport |org.openscience.cdk.renderer.generators| |BasicAtomGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicBondGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicBondGenerator$DefaultBondColor|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$BackgroundColor|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$FitToScreen|)
(jimport |org.openscience.cdk.renderer.generators| |BasicSceneGenerator$BondLength|)

(jimport |org.openscience.cdk.renderer.font| |AWTFontManager|)
(jimport |org.freehep.graphicsio| |PageConstants|)
(jimport |org.freehep.graphicsio.svg| |SVGGraphics2D|)
(jimport |org.freehep.graphicsio.pdf| |PDFGraphics2D|)
(jimport |org.openscience.cdk.renderer.visitor| |AWTDrawVisitor|)
(jimport |org.openscience.cdk.layout| |StructureDiagramGenerator|)

(jimport |java.awt| |Dimension|)
(jimport |java.awt| |Rectangle|)
(jimport |java.awt.geom| |Rectangle2D$Double|)

;;
;; Atom Container Rendering Support

(defparameter *renderer-generators*
  (jlist (java:jnew |BasicSceneGenerator|)
         (java:jnew |BasicBondGenerator|)
         (java:jnew |BasicAtomGenerator|)))


(defparameter *font-manager* (java:jnew |AWTFontManager|))
(defparameter *atom-container-renderer* (java:jnew |AtomContainerRenderer|
                                                   *renderer-generators*
                                                   *font-manager*))

(defun prepare-atom-container-for-rendering (ac &key (angle 0d0) flip)
  (let ((sdg (java:jnew |StructureDiagramGenerator| ac)))
    (#"generateCoordinates" sdg
                            (java:jnew (java:jconstructor |Vector2d| 2)
                                       (cos angle) (sin angle))))
  (when angle
    (rotate-atom-container ac angle))
  (case flip
    (:vertical (flip-atom-container-vertical ac))
    (:horizontal (flip-atom-container-horizontal ac))
    (:both (flip-atom-container-vertical ac)
           (flip-atom-container-horizontal ac)))
  ac)

(defparameter *background-color* (java:jfield |Color| "white"))
(defparameter *default-bond-color* (java:jfield |Color| "black"))
(defparameter *scene-background-color* nil)
(defparameter *graphics-background-color* nil)
(defparameter *scene-bond-length* 20)

(defmacro with-graphics ((graphics) &body body)
  `(progn
    (#"startExport" ,graphics)
    ,@body
    (#"endExport" ,graphics)))

(defun mol-to-graphics (mol renderer graphics x1 y1 x2 y2 x-margin y-margin)
  (let ((*scene-background-color* (or *scene-background-color* *background-color*))
        (*graphics-background-color* (or *graphics-background-color* *background-color*))
        (model (#"getRenderer2DModel" renderer)))
    (when *scene-bond-length*
      (#"set" model
              (java:jclass |BasicSceneGenerator$BondLength|)
              (java:jnew "java.lang.Double" *scene-bond-length*)))
    (#"set" model
            (java:jclass |BasicSceneGenerator$BackgroundColor|)
            *scene-background-color*)
    (#"set" model
            (java:jclass |BasicBondGenerator$DefaultBondColor|)
            *default-bond-color*)
    (let ((draw-visitor (java:jnew |AWTDrawVisitor| graphics)))
      (let ((bounds (java:jnew |Rectangle| x1 y1 x2 y2)))
        (#"setup" renderer mol bounds)
        (when *graphics-background-color*
          (#"setBackground" graphics *graphics-background-color*)
          (#"clearRect" graphics x1 y1 x2 y2)))
      (#"paint" renderer mol draw-visitor
                (let ((width (1+ (- x2 x1)))
                      (height (1+ (- y2 y1))))
                  (java:jnew (java:jconstructor |Rectangle2D$Double| 4)
                             (+ x1 x-margin) (+ y1 y-margin)
                             (- width (* x-margin 2)) (- height (* y-margin 2))))
                java:+true+)
      )))

(defun draw-atom-container-to-svg-stream (mol stream width height x-margin y-margin)
  (let ((graphics (java:jnew |SVGGraphics2D| stream
                             (java:jnew |Dimension| width height))))
    (with-graphics (graphics)
      (mol-to-graphics mol *atom-container-renderer* graphics 0 0 (1- width) (1- height) x-margin y-margin))))

(defun draw-atom-container-to-svg-string (mol width height x-margin y-margin &key (trim-header t))
  (let ((str (java:jnew |ByteArrayOutputStream|)))
    (draw-atom-container-to-svg-stream mol
                                       str
                                       width height x-margin y-margin)
    (let ((jstr (java:jnew |String| (#"toByteArray" str) "UTF-8")))
      (if trim-header
          (java:jobject-lisp-value (#"substring" jstr 39))
          (java:jobject-lisp-value jstr)))))

(defun draw-atom-container-to-svg (mol pathname width height x-margin y-margin)
  (with-open-file (out-stream pathname :direction :output
                              :if-exists :supersede
                              :element-type :default)
    (draw-atom-container-to-svg-stream mol
                                       (#"getWrappedOutputStream" out-stream)
                                       width height x-margin y-margin)))

(defun draw-atom-container-to-pdf (mol pathname width height x-margin y-margin)
  (with-open-file (out-stream pathname :direction :output
                              :if-exists :supersede
                              :element-type :default)
    (let ((prop (java:jstatic "getDefaultProperties" |PDFGraphics2D|)))
        (#"setProperty" prop
                        (java:jfield |PDFGraphics2D| "PAGE_SIZE")
                        (java:jfield |PageConstants| "A6"))
        (#"setProperty" prop
                        (java:jfield |PDFGraphics2D| "ORIENTATION")
                        (java:jfield |PageConstants| "LANDSCAPE")))
    (let ((graphics (java:jnew |PDFGraphics2D|
                               (#"getWrappedOutputStream" out-stream)
                               (java:jnew |Dimension| width height))))
      (with-graphics (graphics)
        (mol-to-graphics mol *atom-container-renderer* graphics 0 0 (1- width) (1- height) x-margin y-margin)))))

(defparameter *default-molecule-width* 512)
(defparameter *default-molecule-height* 512)
(defparameter *default-molecule-margin* 0)
(defparameter *default-molecule-x-margin* nil)
(defparameter *default-molecule-y-margin* nil)
(defparameter *default-molecule-angle* 0d0)
(defparameter *default-molecule-flip* nil)

(defun mol-to-svg (mol pathname &key (width *default-molecule-width*)
                                     (height *default-molecule-height*)
                                     (margin *default-molecule-margin*)
                                     (x-margin (or *default-molecule-x-margin* margin))
                                     (y-margin (or *default-molecule-y-margin* margin))
                                     (angle *default-molecule-angle*)
                                     (flip *default-molecule-flip*))
  (let ((mol (java:jcall "clone" mol)))
    (prepare-atom-container-for-rendering mol :angle angle :flip flip)
    (draw-atom-container-to-svg mol pathname width height x-margin y-margin)
    pathname))

(defun mol-to-svg-string (mol &key (width *default-molecule-width*)
                                   (height *default-molecule-height*)
                                   (margin *default-molecule-margin*)
                                   (x-margin (or *default-molecule-x-margin* margin))
                                   (y-margin (or *default-molecule-y-margin* margin))
                                   (angle *default-molecule-angle*)
                                   (flip *default-molecule-flip*))
  (let ((mol (java:jcall "clone" mol)))
    (prepare-atom-container-for-rendering mol :angle angle :flip flip)
    (draw-atom-container-to-svg-string mol width height x-margin y-margin)))

(defun mol-to-pdf (mol pathname &key (width *default-molecule-width*)
                                     (height *default-molecule-height*)
                                     (margin *default-molecule-margin*)
                                     (x-margin (or *default-molecule-x-margin* margin))
                                     (y-margin (or *default-molecule-y-margin* margin))
                                     (angle *default-molecule-angle*)
                                     (flip *default-molecule-flip*))
  (let ((mol (java:jcall "clone" mol)))
    (prepare-atom-container-for-rendering mol :angle angle :flip flip)
    (draw-atom-container-to-pdf mol pathname width height x-margin y-margin)
    pathname))

(defun atom-container-to-svg-stream (ac out-stream &key (width *default-molecule-width*)
                                                        (height *default-molecule-height*)
                                                        (margin *default-molecule-margin*)
                                                        (x-margin (or *default-molecule-x-margin* margin))
                                                        (y-margin (or *default-molecule-y-margin* margin))
                                                        (angle *default-molecule-angle*)
                                                        (flip *default-molecule-flip*))
  (let ((mols (atom-container-set-atom-containers
               (java:jstatic "partitionIntoMolecules" |ConnectivityChecker| ac))))
    (let ((height (* height (length mols))))
      (let ((graphics (java:jnew |SVGGraphics2D|
                                 (java:jcall "getWrappedOutputStream" out-stream)
                                 (java:jnew |Dimension| width height))))
        (with-graphics (graphics)
          (loop for mol in mols
             for y-offset from 0 by height
             do
               (prepare-atom-container-for-rendering mol :angle angle :flip flip)
               (mol-to-graphics mol *atom-container-renderer* graphics 0 y-offset
                                (1- width) (1- (+ y-offset height)) x-margin y-margin)))))))

(defun atom-container-to-svg (ac pathname &rest args &key (width *default-molecule-width*)
                                                          (height *default-molecule-height*)
                                                          (margin *default-molecule-margin*)
                                                          (x-margin (or *default-molecule-x-margin* margin))
                                                          (y-margin (or *default-molecule-y-margin* margin))
                                                          (angle *default-molecule-angle*)
                                                          (flip *default-molecule-flip*))
  (with-open-file (out-stream pathname :direction :output
                              :if-exists :supersede
                              :element-type :default)
    (apply #'atom-container-to-svg-stream ac out-stream args))
  pathname)


#|

final long[] labels = Canon.label(m, GraphUtil.toAdjList(m));
 
IAtom[] atoms = AtomContainerManipulator.getAtomArray(m);
IBond[] bonds = AtomContainerManipulator.getBondArray(m);
 
// atoms don't know their index
for (int i = 0; i < labels.length; i++)
  atoms[i].setProperty("rank", labels);
  
// sort atoms and bonds (ensure neighbours are provided in the same order)
Arrays.sort(atoms, new Comparator<IAtom>() {
    @Override public int compare(IAtom a, IAtom b) {
        return a.getProperty("rank", Long.class)
                .compareTo(a.getProperty("rank", Long.class));
    }
});
Arrays.sort(bonds, new Comparator<IBond>() {
    @Override public int compare(IBond a, IBond b) {
        long a1 = a.getAtom(0).getProperty("rank");
        long a2 = a.getAtom(1).getProperty("rank");
        long b1 = b.getAtom(0).getProperty("rank");
        long b2 = b.getAtom(1).getProperty("rank");
        int cmp = Longs.compare(Math.min(a1, a2), Math.min(b1, b2));
        return cmp != 0 ? cmp : Longs.compare(Math.max(a1, a2), Math.max(b1, b2)); 
    }
});
 
// set the new orderings
m.setAtoms(atoms);
m.setBonds(bonds);
 
// clean up
for (IAtom a : m.atoms()) {
    a.removeProperty("rank");
    if (a.getProperties().isEmpty())
        a.setProperties(null);
        }
|#

(jimport |org.openscience.cdk.graph.invariant| |Canon|)
(jimport |org.openscience.cdk.graph| |GraphUtil|)
(jimport |org.openscience.cdk.tools.manipulator| |AtomContainerManipulator|)


(defun get-bond-ranks (b)
  (mapcar (lambda (x) (java:jcall "getProperty" x "rank"))
          (atoms b)))

(defun reorder-atom-container-atoms-and-bonds (ac)
  (let ((labels (jarray->list
                 (java:jstatic "label" |Canon|
                               ac
                               (java:jstatic-raw "toAdjList" |GraphUtil| ac)))))
    (let ((atoms (java:jstatic-raw "getAtomArray" |AtomContainerManipulator| ac))
          (bonds (java:jstatic-raw "getBondArray" |AtomContainerManipulator| ac)))
      (loop for i below (length labels)
         for atom = (java:jarray-ref atoms i)
         do (java:jcall "setProperty" atom "rank" i))
      
      (let ((new-atom-list
             (java:jnew-array-from-list |IAtom| (sort (jarray->list atoms) #'<
                                                      :key (lambda (x) (java:jcall "getProperty" x "rank"))))))
        (java:jcall "setAtoms" ac new-atom-list))
      
      (let ((new-bond-list
             (java:jnew-array-from-list |IBond| (sort (jarray->list bonds) #'<
                                                      :key (lambda (x) (apply #'max (get-bond-ranks x)))))))
        (java:jcall "setBonds" ac new-bond-list))
      
      ;; clean up
      (loop for i below (length labels)
         for atom = (java:jarray-ref atoms i)
         do (java:jcall "removeProperty" atom "rank")
           
         ;; FIXME this breaks things!!
           #+nil
           (when (java:jcall "isEmpty" (java:jcall "getProperties" atom))
             (java:jcall "setProperties" atom java:+null+))))
    ac))

