(defsystem "cl-quakeinfo"
  :depends-on
  ("cl-geocode"
   "uiop"
   (:feature (:not :allegro) "acl-compat")
   (:feature (:not :allegro) "cl-ppcre")
   (:feature (:not :allegro) "cl-date-time-parser")
   (:feature :allegro (:require "regexp2"))
   (:feature :allegro (:require "datetime"))
   (:feature :allegro (:require "aserve")))
  :components ((:file "cl-quakeinfo" :type "cl")))
