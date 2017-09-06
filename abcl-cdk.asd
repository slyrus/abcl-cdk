
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require 'abcl-contrib)
  (cl:require 'abcl-asdf)
  (cl:require 'jss)
  (cl:require 'extensible-sequences)
  (cl:require 'java-collections))

(asdf:defsystem :abcl-cdk
  :name "abcl-cdk"
  :author "Cyrus Harmon"
  :serial t
  :depends-on (alexandria)
  :defsystem-depends-on (asdf-mvn-module)
  :components
  ((:mvn-module cdk
                :dependencies
                ("org.freehep/freehep-graphics2d/2.4"
                 "org.freehep/freehep-graphicsio-pdf/2.4"
                 "org.freehep/freehep-graphicsio-svg/2.4"
                 "org.openscience.cdk/cdk-bundle/2.1-SNAPSHOT"
                 "uk.ac.cam.ch.opsin/opsin-core/2.3.1"
                 "dk.brics.automaton/automaton/1.11-8"
                 "xom/xom/1.2.5"
                 "commons-io/commons-io/2.5"
                 "commons-cli/commons-cli/1.4"
                 "log4j/log4j/1.2.17"))
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

