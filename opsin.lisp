

(cl:in-package :abcl-cdk-opsin)

(jimport |uk.ac.cam.ch.wwmm.opsin| |NameToStructure|)

(defparameter *nts* (java:jstatic "getInstance" |NameToStructure|))

(defun parse-iupac-name-to-smiles (iupac)
  (java:jcall "parseToSmiles" *nts* iupac))

