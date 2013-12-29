
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

(cl:defpackage #:abcl-cdk-examples-config (:export #:*base-directory*))

(cl:defparameter abcl-cdk-examples-config:*base-directory* 
  (merge-pathnames #P"examples/"
                   (make-pathname :name nil :type nil :defaults *load-truename*)))

