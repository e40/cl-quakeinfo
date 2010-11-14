
(in-package :cl-user)

(defclass my-cl-source-file (asdf:cl-source-file) ())

(defmethod asdf:source-file-type ((f my-cl-source-file) (m asdf:module))
  (declare (ignorable f m))
  "cl")

(asdf:disable-output-translations)

(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #p"../cl-geocode/"))

(asdf:defsystem cl-quakeinfo
    :default-component-class cl-user::my-cl-source-file
    :components ((:file "cl-quakeinfo"))
    :depends-on ("cl-geocode"))
