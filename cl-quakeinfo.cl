;; This software is Copyright (c) Kevin Layer, 2006-2010.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(eval-when (compile eval load)
  (require :regexp2)
  (require :aserve) ;; for http-copy-file
  (use-package :cl-geocode))

(in-package :cl-user)

(defvar *usgs-gov-url-prefix* 
    "http://earthquake.usgs.gov/eqcenter/recenteqsww/catalogs/")

(defun get-quake-info (reference-location
		       &key (period :week)
			    (within
			     ;; distance in decimal degrees
			     3.0)
			    (larger-than 1.0)
			    (temp-file "/tmp/quakeinfo.txt")
		       &aux url)
  (setq url
    (format nil "~a~a"
	    *usgs-gov-url-prefix*
	    (case period
	      (:week "eqs7day-M1.txt")
	      (:day  "eqs1day-M1.txt")
	      (:hour "eqs1hour-M1.txt")
	      (t (error "bad period: ~s." period)))))
  
  (and (probe-file temp-file) (ignore-errors (delete-file temp-file)))
  (format t ";; Downloading quake data...")
  (force-output)
  (net.aserve.client:http-copy-file url temp-file)
  (format t "done.~%")
  (force-output)
  (let (header-line line lines location)
    (unwind-protect
	(with-open-file (s temp-file)
	  ;; This trick only works on UNIX:
	  #-mswindows (delete-file temp-file)
	  (setq header-line (read-line s nil s))
	  (when (eq header-line s) (error "no header?"))
	  (loop
	    (setq line (read-line s nil s))
	    (when (eq line s)
	      (return (nreverse lines)))
	    (multiple-value-bind (found whole date latitude longitude
				  magnitude)
		(match-re
		 #.(util.string:string+
		    "^"
		    "[^,]+,"
		    "[^,]+,"
		    "[^,]+,"
		    "\"([^\"]+)\","	;date
		    "([^,]+),"		;latitude
		    "([^,]+),"		;longitude
		    "([^,]+),"		;magnitude
		    )
		 line)
	      (declare (ignore whole))
	      (if* found
		 then (setq latitude
			(or (ignore-errors (read-from-string latitude))
			    (error "bad latitude: ~s" latitude)))
		      (setq longitude
			(or (ignore-errors (read-from-string longitude))
			    (error "bad longitude: ~s" longitude)))
		      (setq magnitude
			(or (ignore-errors (read-from-string magnitude))
			    (error "bad magnitude: ~s" magnitude)))
		      (setq location (make-location :latitude latitude
						    :longitude longitude))
		      (when (and (location-near-p location
						  reference-location
						  within)
				 (or (null larger-than)
				     (> magnitude larger-than)))
			(push (list date (location-to-place location)
				    location magnitude)
			      lines))
		 else (warn  "couldn't parse line: ~a~%" line)))))
      #+mswindows (delete-file temp-file))))

(setq *global-gc-behavior* :auto) ;; get rid of GC related messages

(format t ";; Try this:~%")
(pprint
 '(get-quake-info
   (place-to-location "Oakland, CA")
   :period :week
   :larger-than nil :within 1.0))
