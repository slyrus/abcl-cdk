
(asdf:defsystem :abcl-cdk-hacking
  :name "abcl-cdk-hacking"
  :author "Cyrus Harmon"
  :serial t
  :default-component-class asdf:cl-source-file
  :components
  ((:mvn "junit/junit" :version "3.8.2")
   (:mvn "org.freehep/freehep-graphics2d" :version "2.1.1")
   (:mvn "org.freehep/freehep-graphicsio-pdf" :version "2.1.1")
   (:mvn "org.freehep/freehep-graphicsio-svg" :version "2.1.1")
   (:mvn "org.openscience/cdk" :version "1.4.7")
   (:file "abcl-cdk-hacking")))

(cl:defpackage #:abcl-cdk-hacking-config (:export #:*base-directory*))

(cl:defparameter abcl-cdk-hacking-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

