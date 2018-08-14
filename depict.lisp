
(cl:in-package :abcl-cdk)

(defparameter +automatic+ (java:jfield |DepictionGenerator| "AUTOMATIC"))

(defparameter *default-depiction-generator*
  (java:jcall "withAtomColors" (java:jnew |DepictionGenerator|)))

(defun default-depiction-generator ()
  *default-depiction-generator*)

(defun depict-to-svg (mol pathname &key (size +automatic+)
                                        (width size)
                                        (height size)
                                        (depiction-generator (default-depiction-generator)))
  (let ((dg (java:jcall "withSize"
                        depiction-generator
                        width height)))
    (let ((depiction (java:jcall "depict" dg mol)))
      (java:jcall "writeTo" depiction (namestring pathname))))
  pathname)

(defun depict-to-svg-string (mol &key (size +automatic+)
                                      (width size)
                                      (height size)
                                      (depiction-generator (default-depiction-generator)))
  (let ((dg (java:jcall "withSize"
                        depiction-generator
                        width height)))
    (let ((depiction (java:jcall "depict" dg mol)))
      (java:jcall "toSvgStr" depiction))))

(jimport |org.openscience.cdk.interfaces| |IAtom|)

(jimport |org.openscience.cdk.renderer.color| |IAtomColorer|)
(jimport |org.openscience.cdk.renderer.color| |CDK2DAtomColors|)

(defparameter *inverse-colorer-class*
  (let ((colors (java:jnew |CDK2DAtomColors|)))
    (flet ((get-atom-color (this atom &optional color)
             (declare (ignore this color))
             (let ((res (java:jcall "getAtomColor" colors atom)))
               (if (java:jequal res (java:jfield |Color| "black"))
                   (java:jfield |Color| "white")
                   res))))
      (java:jnew-runtime-class
       "InverseColorer"
       :interfaces (list |IAtomColorer|)
       :methods `(("getAtomColor" ,|Color| (,|IAtom|)
                   ,#'get-atom-color
                   :modifiers (:public))
                  ("getAtomColor" ,|Color| (,|IAtom| ,|Color|)
                   ,#'get-atom-color
                   :modifiers (:public)))
       :access-flags '(:public :static :final)))))

(defparameter *inverse-colorer-2-class*
  (flet ((get-atom-color (this atom &optional color)
           (declare (ignore color))
           (let ((res (java:jcall "CDK2DAtomColors.getAtomColor" this atom)))
             (if (java:jequal res (java:jfield |Color| "black"))
                 (java:jfield |Color| "white")
                 res))))
    (java:jnew-runtime-class
     "InverseColorer2"
     :superclass |CDK2DAtomColors|
     :interfaces (list |IAtomColorer|)
     :methods `(("getAtomColor" ,|Color| (,|IAtom|)
                 ,#'get-atom-color
                 :modifiers (:public))
                ("getAtomColor" ,|Color| (,|IAtom| ,|Color|)
                 ,#'get-atom-color
                 :modifiers (:public)))
     :access-flags '(:public :static :final))))

