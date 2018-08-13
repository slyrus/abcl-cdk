
(cl:in-package #:abcl-cdk)

(defparameter *svg-dtd-hash*
  (let ((ht (make-hash-table :test 'equalp))
        (path (asdf:component-pathname (asdf:find-system "abcl-cdk"))))
    (map nil
         (lambda (x) (setf (gethash (format nil "http://www.w3.org/Graphics/SVG/1.1/DTD/svg~A" x)
                                    ht)
                           (merge-pathnames (format nil "svg/1.1/DTD/svg~A" x)
                                            path)))
         '("11.dtd"
           "11-model.mod"
           "11-attribs.mod"
           "-framework.mod"
           "-datatypes.mod"
           "-qname.mod"
           "-core-attrib.mod"
           "-container-attrib.mod"
           "-viewport-attrib.mod"
           "-paint-attrib.mod"
           "-opacity-attrib.mod"
           "-graphics-attrib.mod"
           "-docevents-attrib.mod"
           "-graphevents-attrib.mod"
           "-animevents-attrib.mod"
           "-xlink-attrib.mod"
           "-extresources-attrib.mod"
           "-core-attrib.mod"
           "-structure.mod"
           "-conditional.mod"
           "-image.mod"
           "-style.mod"
           "-shape.mod"
           "-text.mod"
           "-marker.mod"
           "-profile.mod"
           "-gradient.mod"
           "-pattern.mod"
           "-clip.mod"
           "-mask.mod"
           "-filter.mod"
           "-cursor.mod"
           "-hyperlink.mod"
           "-view.mod"
           "-script.mod"
           "-animation.mod"
           "-font.mod"
           "-extensibility.mod"))
    ht))

(defun parse-svg-string (svg-string)
  (flet ((resolver (pubid sysid)
	   (declare (ignore pubid))
           (let ((pathname (gethash (puri:render-uri sysid nil) *svg-dtd-hash*)))
	     (when pathname
	       (open pathname :element-type '(unsigned-byte 8))))))
    (cxml:parse svg-string (stp:make-builder) :entity-resolver #'resolver)))

(defun svg-body (svg-stp)
  (let ((sink (cxml:make-string-sink)))
    (stp:serialize (stp:nth-child 1 svg-stp) sink)
    (sax:end-document sink)))
