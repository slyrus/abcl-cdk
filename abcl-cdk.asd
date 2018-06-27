
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

                 "org.openscience.cdk/cdk-annotation/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-atomtype/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-builder3d/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-builder3dtools/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-bundle/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-charges/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-cip/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-control/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-core/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-ctab/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-data/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-datadebug/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-depict/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-dict/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-diff/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-extra/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-fingerprint/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-forcefield/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-formula/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-fragment/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-group/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-hash/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-inchi/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-interfaces/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-io/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-ioformats/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-ionpot/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-iordf/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-isomorphism/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-legacy/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-libiocml/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-libiomd/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-log4j/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-model/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-pcore/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-pdb/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-pdbcml/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qm/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsar/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsaratomic/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarbond/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarcml/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarionpot/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarmolecular/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarprotein/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-qsarsubstance/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-reaction/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-render/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-renderawt/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-renderbasic/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-renderextra/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-sdg/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-signature/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-smarts/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-smiles/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-smsd/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-standard/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-structgen/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-tautomer/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-testdata/2.2-SNAPSHOT"
                 "org.openscience.cdk/cdk-valencycheck/2.2-SNAPSHOT"

                 "uk.ac.cam.ch.opsin/opsin-core/2.3.1"
                 "dk.brics.automaton/automaton/1.11-8"
                 "com.io7m.xom/xom/1.2.10"

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

