;; This software is Copyright (c) Kevin Layer, 2006-2010.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(eval-when (compile eval load)
  #+allegro (require :regexp2)
  #+allegro (require :aserve) ;; for http-copy-file
  (use-package :cl-geocode))

(in-package :cl-user)

(defvar *usgs-gov-url-prefix* 
    "http://earthquake.usgs.gov/eqcenter/recenteqsww/catalogs/")

(defvar *quake-info-re*
    (let ((re (concatenate 'simple-string
		"^"
		"[^,]+,"
		"[^,]+,"
		"[^,]+,"
		"\"([^\"]+)\","		;date
		"([^,]+),"		;latitude
		"([^,]+),"		;longitude
		"([^,]+),"		;magnitude
		)))
      #+allegro (compile-re re)
      #+sbcl (cl-ppcre:create-scanner re)))

(defun get-quake-info (reference-location
		       &key (period :week)
			    (within
			     ;; distance in decimal degrees
			     3.0)
			    (larger-than 1.0)
			    (temp-file "/tmp/quakeinfo.txt")
		       &aux url
			    (re *quake-info-re*))
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
  #+allegro (net.aserve.client:http-copy-file url temp-file)
  #+sbcl
  (let ((p (run-program "/usr/bin/curl" (list "-L" url "-o" temp-file)
			:wait t)))
    (when (not (zerop (process-exit-code p)))
      (error "Failed to retrieve data via curl.  Exit code ~d."
	     (process-exit-code p))))
  (format t "done.~%")
  (force-output)
  
  (let (header-line line lines location
	date latitude longitude magnitude)
    (unwind-protect
	(with-open-file (s temp-file)
	  ;; This trick only works on UNIX:
	  #-mswindows (delete-file temp-file)
	  (setq header-line (read-line s nil s))
	  (when (eq header-line s) (error "no header?"))
	  (tagbody
	   top
	    (setq line (read-line s nil s))
	    (when (eq line s)
	      (return-from get-quake-info
		(nreverse lines)))
	    #+sbcl
	    (multiple-value-bind (found res-vec)
		(cl-ppcre:scan-to-strings re line)
	      (when (not found)
		(warn "couldn't parse line: ~a~%" line)
		(go top))
	      (setq date (aref res-vec 0)
		    latitude (aref res-vec 1)
		    longitude (aref res-vec 2)
		    magnitude (aref res-vec 3)))
	    #+allegro
	    (multiple-value-bind (found whole xdate xlatitude xlongitude
				  xmagnitude)
		(match-re re line)
	      (declare (ignore whole))
	      (when (not found)
		(warn "couldn't parse line: ~a~%" line)
		(go top))
	      (setq date xdate
		    latitude xlatitude
		    longitude xlongitude
		    magnitude xmagnitude))
	    
	    (setq latitude
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
	    (go top)))
      #+mswindows (delete-file temp-file))))

#+allegro
(setq *global-gc-behavior* :auto) ;; get rid of GC related messages

(format t ";; Try this:~%")
(pprint
 '(get-quake-info
   (place-to-location "Oakland, CA")
   :period :week
   :larger-than nil :within 1.0))
