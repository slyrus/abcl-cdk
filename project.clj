

(defproject cdk-hacking "0.0.0"
  :description "A toy project for playing around with CDK"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.freehep/freehep-graphics2d "2.1.1"]
                 [org.freehep/freehep-graphicsio-pdf "2.1.1"]
                 [org.freehep/freehep-graphicsio-svg "2.1.1"]
                 [org.openscience/cdk "1.4.7"]
                 [junit "3.8.2"]
                 [org.apache.maven/maven-aether-provider "3.0.4"]
                 [org.apache.maven.wagon/wagon-http "2.2"]
 ]

  :dev-dependencies [[swank-clojure "1.5.0-SNAPSHOT"]]
  :repositories {"freehep" "http://java.freehep.org/maven2"})
