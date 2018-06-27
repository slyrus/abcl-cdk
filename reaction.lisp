;;; file: reaction.lisp
;;;
;;; Copyright (c) 2012-2014 Cyrus Harmon (ch-lisp@bobobeach.com)
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

(defun make-reaction (id reactants products agents)
  (let ((rxn (java:jnew |Reaction|)))
    (java:jcall "setID" rxn id)
    (loop for r in reactants
       do (java:jcall "addReactant" rxn r))
    (loop for p in products
       do (java:jcall "addProduct" rxn p))
    (loop for a in agents
       do (java:jcall "addAgent" rxn a))
    rxn))

(defun reactants (reaction)
  (atom-container-set-atom-containers (#"getReactants" reaction)))

(defun products (reaction)
  (atom-container-set-atom-containers (#"getProducts" reaction)))

(defun agents (reaction)
  (atom-container-set-atom-containers (#"getAgents" reaction)))

(defun make-reaction-set (id reaction-list)
  (let ((rxn-set (java:jnew |ReactionSet|)))
    (java:jcall "setID" rxn-set id)
    (loop for r in reaction-list
       do (java:jcall "addReaction" rxn-set r))
    rxn-set))

(defun reactions (reaction-set)
  (items (#"reactions" reaction-set)))
