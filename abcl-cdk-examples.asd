
(asdf:defsystem :abcl-cdk-examples
  :name "abcl-cdk-example"
  :author "Cyrus Harmon"
  :serial t
  :default-component-class asdf:cl-source-file
  :depends-on (abcl-cdk)
  :components
  ((:module :examples
            :components
            ((:file "examples")))))

(cl:defparameter abcl-cdk-examples-config:*base-directory* 
  (make-pathname :name "examples/" :type nil :defaults *load-truename*))

