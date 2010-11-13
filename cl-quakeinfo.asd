
(defsystem cl-quakeinfo
    :default-component-class cl-user::my-cl-source-file
    :components ((:file "cl-quakeinfo"))
    :depends-on ("cl-geocode"))
