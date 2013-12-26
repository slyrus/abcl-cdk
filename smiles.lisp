;;; file: smiles.lisp
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
  (jimport |org.openscience.cdk| |Atom|)

  (jimport |org.openscience.cdk.smiles| |SmilesParser|)
  (jimport |org.openscience.cdk.smiles| |SmilesGenerator|)
  (jimport |org.openscience.cdk| |DefaultChemObjectBuilder|))

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
