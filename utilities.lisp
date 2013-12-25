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
  `(defparameter ,(apply #'intern class
                         (when package (list package)))
     (concatenate 'string (symbol-name (quote ,java-package))
                  "."
                  (symbol-name (quote ,class)))))

(jimport |java.util| |Vector|)
(jimport |org.openscience.cdk.ringsearch| |AllRingsFinder|)

(defun jlist (&rest initial-contents)
  (sequence:make-sequence-like
   (java:jnew |Vector|) (length initial-contents)
   :initial-contents initial-contents))

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

(defun get-bonds-containing-atom (ac atom)
  (items (#"getConnectedBondsList" ac atom)))

;;
;; Use #"getConnectedAtomsList" instead!!!
(defun get-neighbors (ac atom)
  (let ((bonds (get-bonds-containing-atom ac atom)))
    (loop for bond in bonds
       collect (remove atom (items (#"atoms" bond)) :test 'equalp))))

(defun get-largest-ring (ac)
  (let* ((arf (java:jnew |AllRingsFinder|))
         (rs (#"findAllRings" arf ac)))
  (loop for ring in (items (#"atomContainers" rs))
     maximizing (#"getRingSize" ring)
     finally (return ring))))

