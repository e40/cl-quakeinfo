
(in-package :cl-user)

(eval-when (compile eval load) (require :asdf))

(setf asdf:*central-registry*
  '(*default-pathname-defaults*
    #p"~layer/src/cl-geocode/"))
  
(asdf:load-system 'cl-geocode)
(asdf:load-system 'cl-quakeinfo)

(load "..//google-maps-key.cl")

#+allegro (setq tpl:*print-length* nil)

(pprint
 (get-quake-info (place-to-location "Oakland, CA")
		 :period :week
		 :larger-than nil
		 :within 1.0))
(pprint
 (get-quake-info (place-to-location "Oakland, CA")
		 :period :week
		 :convert-date t
		 :larger-than nil
		 :within 1.0))

(exit 0)
