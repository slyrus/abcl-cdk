
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require 'abcl-contrib)
  (cl:require 'abcl-asdf)
  (cl:require 'extensible-sequences)
  (cl:require 'java-collections))

(asdf:defsystem :abcl-cdk
  :name "abcl-cdk"
  :author "Cyrus Harmon"
  :serial t
  :default-component-class asdf:cl-source-file
  :components
  ((:mvn "org.freehep/freehep-graphics2d" :version "2.2.1")
   (:mvn "org.freehep/freehep-graphicsio-pdf" :version "2.2.1")
   (:mvn "org.freehep/freehep-graphicsio-svg" :version "2.2.1")
   (:mvn "org.openscience.cdk/cdk-git" :version "1.5.5")
   (:file "package")
   (:file "utilities")
   (:file "smiles")
   (:file "geometry")
   (:file "render")
   (:file "inchi")
   (:file "reaction")))

(cl:defpackage #:abcl-cdk-config (:export #:*base-directory*))

(cl:defparameter abcl-cdk-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

