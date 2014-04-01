;;; file: utilities.lisp
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

(defmacro jimport (java-package class &optional package)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter ,(apply #'intern class
                          (when package (list package)))
      (concatenate 'string (symbol-name (quote ,java-package))
                   "."
                   (symbol-name (quote ,class))))))

(jimport |java.util| |Vector|)
(jimport |java.util| |Arrays|)
(jimport |java.lang| |Integer|)
(jimport |org.openscience.cdk| |CDKConstants|)
(jimport |org.openscience.cdk.interfaces| |IAtom|)
(jimport |org.openscience.cdk.interfaces| |IBond|)
(jimport |org.openscience.cdk.config| |Isotopes|)
(jimport |org.openscience.cdk.interfaces| |IPseudoAtom|)
(jimport |org.openscience.cdk| |AtomContainer|)
(jimport |org.openscience.cdk.graph| |ShortestPaths|)
(jimport |org.openscience.cdk.ringsearch| |AllRingsFinder|)
(jimport |org.openscience.cdk.tools.manipulator| |MolecularFormulaManipulator|)

(jimport |org.openscience.cdk.tools.manipulator| |MolecularFormulaManipulator|)
(jimport |org.openscience.cdk.tools.manipulator| |AtomContainerManipulator|)

(jimport |org.openscience.cdk.graph.invariant| |Canon|)
(jimport |org.openscience.cdk.graph| |GraphUtil|)

(defun jlist (&rest initial-contents)
  (sequence:make-sequence-like
   (java:jnew |Vector|) (length initial-contents)
   :initial-contents initial-contents))

(defun jarray->list (jarr)
  (loop for i below (java:jarray-length jarr)
     collect (java:jarray-ref jarr i)))

;;
;; Java Array Routines
;;
(defun java-2d-array->array (jarr)
  (let* ((outer-dim (java:jarray-length jarr))
         (inner-dim
          (loop for i below outer-dim
             maximize (java:jarray-length (java:jarray-ref jarr i)))))
    (let ((arr (make-array (list outer-dim inner-dim))))
      (loop for i below outer-dim
         do (let ((inner-dim (java:jarray-length (java:jarray-ref jarr i))))
              (loop for j below inner-dim
                 do (setf (aref arr i j)
                          (java:jarray-ref jarr i j)))))
      arr)))

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

(defun atoms (ac)
  (items (#"atoms" ac)))

(defun bonds (ac)
  (items (#"bonds" ac)))


(defun get-atoms-of-symbol (ac symbol)
  (loop for atom in (items (#"atoms" ac))
     for s = (#"getSymbol" atom)
     when (equal s symbol)
     collect atom))

(defun get-pseudo-atoms (ac)
  (loop for atom in (atoms ac)
     when (java:jinstance-of-p atom |IPseudoAtom|)
     collect atom))

(defun get-bonds-containing-atom (ac atom)
  (items (#"getConnectedBondsList" ac atom)))

(defun get-neighbors (ac atom)
  (items (#"getConnectedAtomsList" ac atom)))

(defun get-largest-ring (ac)
  (let* ((arf (java:jnew |AllRingsFinder|))
         (rs (#"findAllRings" arf ac)))
    (loop for ring in (items (#"atomContainers" rs))
       maximizing (#"getRingSize" ring)
       finally (return ring))))

(defun get-reachable-atoms (ac start)
  (let ((n-shortest-paths (java:jnew |ShortestPaths| ac start)))
    (loop for atom in (items (#"atoms" ac))
       for d = (#"distanceTo" n-shortest-paths atom)
       when (< d (java:jfield |Integer| "MAX_VALUE"))
       collect atom)))

(defun get-atom-atom-mapping (atom)
  (#"getProperty" atom (java:jfield |CDKConstants| "ATOM_ATOM_MAPPING")))

(defun get-reachable-bonds (ac start)
  (remove-duplicates
   (apply #'append
          (let ((n-shortest-paths (java:jnew |ShortestPaths| ac start)))
            (loop for atom in (items (#"atoms" ac))
               for d = (#"distanceTo" n-shortest-paths atom)
               when (< d (java:jfield |Integer| "MAX_VALUE"))
               collect (get-bonds-containing-atom ac atom))))
   :test 'equalp))

(defparameter *isotopes* (java:jstatic "getInstance" |Isotopes|))
(defparameter *h-1* (#"getMajorIsotope" *isotopes* 1))
(defparameter *h-1-exact-mass* (#"getExactMass" *h-1*))
(defparameter *h-1-natural-mass* (#"getNaturalMass" *isotopes* *h-1*))

(defun configure-atom-container (ac)
  (loop for a in (atoms ac)
     do (#"configure" *isotopes* a))
  ac)

(defun get-atom-container-exact-mass (ac)
  (configure-atom-container ac)
  (loop for a in (atoms ac)
     sum (let ((h-count (#"getImplicitHydrogenCount" a)))
               (let ((h-mass (if h-count
                                 (* h-count *h-1-exact-mass*)
                                 0)))
                 (+ h-mass (#"getExactMass" a))))))

(defun get-atom-container-natural-mass (ac)
  (configure-atom-container ac)
  (loop for a in (atoms ac)
     sum (let ((h-count (#"getImplicitHydrogenCount" a)))
               (let ((h-mass (if h-count
                                 (* h-count *h-1-natural-mass*)
                                 0)))
                 (+ h-mass (#"getNaturalMass" *isotopes* a))))))

(defun get-molecular-formula (ac)
  (java:jstatic "getString" |MolecularFormulaManipulator|
   (java:jstatic "getMolecularFormula" |MolecularFormulaManipulator| ac)))

(defun atom-container-set-atom-containers (acs)
  (items (#"atomContainers" acs)))

(defun remove-atoms (ac atoms-to-remove)
  "Remove all atoms other specified in the list atoms-to-remove (and
their associated bonds) from ac."
  (mapcar (lambda (x) (#"removeAtomAndConnectedElectronContainers" ac x))
          (remove-if-not (lambda (x) (member x atoms-to-remove :test 'equal))
                         (atoms ac)))
  ac)

(defun keep-atoms (ac atoms-to-keep)
  "Remove all atoms other than those specified in the list
atoms-to-keep (and their associated bonds) from ac."
  (mapcar (lambda (x) (#"removeAtomAndConnectedElectronContainers" ac x))
          (remove-if (lambda (x) (member x atoms-to-keep :test 'equal))
                     (atoms ac)))
  ac)

(defun copy-atom-container (ac)
  "Create a (shallow) copy of an atom container."
  (java:jnew |AtomContainer| ac))


(defun reorder-atoms (ac)
  (let ((labels (#"label" '#.(make-symbol |Canon|) ac
                          (#0"toAdjList" '#.(make-symbol |GraphUtil|) ac))))
    (let ((atoms (#"getAtomArray" '#.(make-symbol |AtomContainerManipulator|) ac))
          (bonds (#"getBondArray" '#.(make-symbol |AtomContainerManipulator|) ac)))
      ;; set atom ranks
      (loop for atom across atoms
         for i from 0
         do (#"setProperty" atom "rank" (java:jarray-ref labels i)))

      (sort atoms (lambda (a b)
                    (let ((a-rank (#"getProperty" a "rank" (java:jclass "java.lang.Integer")))
                          (b-rank (#"getProperty" b "rank" (java:jclass "java.lang.Integer"))))
                      (> a-rank b-rank))))

      (sort bonds (lambda (a b)
                    (let ((a0 (#"getProperty" (#"getAtom" a 0) "rank" (java:jclass "java.lang.Integer")))
                          (a1 (#"getProperty" (#"getAtom" a 1) "rank" (java:jclass "java.lang.Integer")))
                          (b0 (#"getProperty" (#"getAtom" b 0) "rank" (java:jclass "java.lang.Integer")))
                          (b1 (#"getProperty" (#"getAtom" b 1) "rank" (java:jclass "java.lang.Integer"))))
                      (if (= (min a0 a1) (min b0 b1))
                          (> (max a0 a1) (max b0 b1))
                          (> (min a0 a1) (min b0 b1))))))
      
      (#"setAtoms" ac (java:jnew-array-from-array |IAtom| atoms))
      (#"setBonds" ac (java:jnew-array-from-array |IBond| bonds))
      
      ;; clean up
      (loop for atom across atoms
         for i from 0
         do
           (#"removeProperty" atom "rank")
           (when (null (#"getProperties" atom))
             (#"setProperties" atom java:+null+)))))
  ac)

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
