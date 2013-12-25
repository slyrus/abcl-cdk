;;; file: geometry.lisp
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
  (jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality|)
  (jimport |org.openscience.cdk.interfaces| |ITetrahedralChirality$Stereo|)
  (jimport |org.openscience.cdk.interfaces| |IBond|)
  (jimport |org.openscience.cdk.interfaces| |IBond$Stereo|))

;;;; Some lisp-side constants
(defconstant +clockwise+ (java:jfield |ITetrahedralChirality$Stereo| "CLOCKWISE"))
(defconstant +anti-clockwise+ (java:jfield |ITetrahedralChirality$Stereo| "ANTI_CLOCKWISE"))
(defconstant +up+ (java:jfield |IBond$Stereo| "UP"))
(defconstant +down+ (java:jfield |IBond$Stereo| "DOWN"))

(defun atom-container-atom-positions (ac)
  (loop for atom in (items (#"atoms" ac))
     collect (let ((oldx  (java:jfield "x" (#"getPoint2d" atom)))
                   (oldy (java:jfield "y" (#"getPoint2d" atom))))
               (cons oldx oldy))))

(defun atom-list-atom-extrema (atom-list)
  (loop for (x . y) in
       (loop for atom in atom-list
          collect (cons (java:jfield "x" (#"getPoint2d" atom))
                        (java:jfield "y" (#"getPoint2d" atom))))
     minimizing x into xmin
     maximizing x into xmax
     minimizing y into ymin
     maximizing y into ymax
     finally (return (list (cons xmin xmax) (cons ymin ymax)))))

(defun atom-container-atom-extrema (ac)
  (atom-list-atom-extrema (items (#"atoms" ac))))

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

(defun flip-atoms-vertical (ac atom-list)
  (destructuring-bind ((xmin . xmax) (ymin . ymax))
      (atom-list-atom-extrema atom-list)
    (declare (ignore xmin xmax))
    (loop for atom in atom-list
       do (let ((oldy (java:jfield "y" (#"getPoint2d" atom))))
            (setf (java:jfield "y" (#"getPoint2d" atom))
                  (+ ymin (- ymax oldy)))))
    #+nil (flip-atom-container-bonds ac) ;; FIXME!!!
    ac))

