
(in-package :cl-user)

(defclass cl-quakeinfo-source-file (asdf:cl-source-file) ())

(defmethod asdf:source-file-type ((f cl-quakeinfo-source-file) (m asdf:module))
  (declare (ignorable f m))
  "cl")

(asdf:disable-output-translations)

(cond
 (asdf:*central-registry*
  (push #p"../cl-geocode/" asdf:*central-registry*))
 (t
  (setf asdf:*central-registry*
    '(*default-pathname-defaults*
      #p"../cl-geocode/"))))

(asdf:defsystem cl-quakeinfo
    :default-component-class cl-quakeinfo-source-file
    :components ((:file "cl-quakeinfo"))
    :depends-on ("cl-geocode"))
