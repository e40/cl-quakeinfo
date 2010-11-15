
(in-package :cl-user)

(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #p"~layer/src/cl-geocode/"))
  
(asdf:load-system 'cl-geocode)
(asdf:load-system 'cl-quakeinfo)

(load "..//google-maps-key.cl")

(get-quake-info (place-to-location "Oakland, CA")
		:period :week
		:larger-than nil
		:within 1.0)

(exit 0)
