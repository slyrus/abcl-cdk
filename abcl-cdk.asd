
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require 'abcl-contrib)
  (cl:require 'abcl-asdf)
  (cl:require 'extensible-sequences)
  (cl:require 'java-collections))

(asdf:defsystem :abcl-cdk
  :name "abcl-cdk"
  :author "Cyrus Harmon"
  :serial t
  :depends-on (alexandria)
  :components
  ((:mvn "org.freehep/freehep-graphics2d" :version "2.2.1")
   (:mvn "org.freehep/freehep-graphicsio-pdf" :version "2.2.1")
   (:mvn "org.freehep/freehep-graphicsio-svg" :version "2.2.1")
   (:mvn "org.openscience.cdk/cdk-bundle/1.5.6-SNAPSHOT" :repository nil)
   (:mvn "uk.ac.cam.ch.opsin/opsin-snapshot" :version "2.0")
   (:mvn "dk.brics.automaton/automaton")
   (:mvn "xom/xom" :version "1.2.5")
   (:mvn "commons-io/commons-io")
   (:mvn "commons-cli/commons-cli")
   (:mvn "log4j/log4j")
   (:file "package")
   (:file "utilities")
   (:file "smiles")
   (:file "geometry")
   (:file "render")
   (:file "inchi")
   (:file "reaction")
   (:file "opsin")))

(cl:defpackage #:abcl-cdk-config (:export #:*base-directory*))

(cl:defparameter abcl-cdk-config:*base-directory* 
  (make-pathname :name nil :type nil :defaults *load-truename*))

