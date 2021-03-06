;;; file: package.lisp
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

(cl:defpackage :abcl-cdk
  (:use :common-lisp)
  (:export #:jimport
           #:jlist

           #:items

           #:atoms
           #:bonds
           #:get-atoms-of-symbol
           #:get-bonds-containing-atom
           #:get-neighbors
           #:get-pseudo-atoms
           
           #:get-largest-ring
           
           #:+up+
           #:+down+
           #:+clockwise+
           #:+anti-clockwise+
           
           #:read-smiles-string
           #:write-smiles-string
           #:write-chiral-smiles-string
           #:mol-to-svg
           #:atom-container-to-svg
           #:mol-to-pdf
           #:*background-color*
           #:*default-bond-color*

           #:*default-molecule-width*
           #:*default-molecule-height*
           #:*default-molecule-margin*
           #:*default-molecule-x-margin*
           #:*default-molecule-y-margin*
           #:*default-molecule-angle*
           #:*default-molecule-flip*
           
           #:prepare-atom-container-for-rendering
           #:flip-atoms-around-bond
           #:draw-atom-container-to-svg
           #:draw-atom-container-to-pdf

           #:get-inchi
           #:get-inchi-key
           
           #:get-reachable-atoms
           #:get-reachable-bonds

           #:get-atom-atom-mapping

           #:get-atom-container-exact-mass
           #:get-atom-container-natural-mass

           #:get-molecular-formula

           #:keep-atoms
           #:remove-atoms
           #:copy-atom-container

           ;; REACTIONs
           #:make-reaction
           #:reactants
           #:agents
           #:products

           ;; REACTION-SETs
           #:make-reaction-set
           #:reactions

           ;; new(-ish) CDK Depcit routines
           #:depict-to-svg
           #:*inverse-colorer-class*))

(cl:defpackage :abcl-cdk-opsin
  (:use :common-lisp :abcl-cdk)
  (:export #:parse-iupac-name-to-smiles))

