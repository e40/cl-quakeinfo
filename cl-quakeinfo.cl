;; This software is Copyright (c) Kevin Layer, 2006-2010.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage :cl-quakeinfo
  (:use :common-lisp :cl-geocode #+allegro :excl #-allegro acl-compat.excl))

(in-package :cl-quakeinfo)

;; See
;;  http://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php
;; for more information.
(defparameter *usgs-gov-url-prefix*
  "http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/")

(defparameter *quake-info-re*
  (let ((re (concatenate 'simple-string
	      "^"
	      "([^,]+),"		;ISO 8601 date/time
	      "([^,]+),"		;latitude
	      "([^,]+),"		;longitude
	      "[^,]+,"
	      "([^,]+),"		;magnitude
	      )))
    #+allegro (compile-re re)
    #+sbcl (cl-ppcre:create-scanner re)))

(defun get-quake-info (reference-location
		       &key (verbose t)
			    (period :week)
			    (within
			     ;; distance in decimal degrees
			     3.0f0)
			    (larger-than 1.0f0)
			    (temp-file "/tmp/quakeinfo.txt")
			    filter
			    convert-date
		       &aux url)
  (setq url
    (format nil "~a~a"
	    *usgs-gov-url-prefix*
	    (case period
	      (:week "all_week.csv")
	      (:day  "all_day.csv")
	      (:hour "all_hour.csv")
	      (t (error "bad period: ~s." period)))))

  (and (probe-file temp-file) (ignore-errors (delete-file temp-file)))
  (when verbose
    (format t ";; Downloading quake data...")
    (force-output))
  #+allegro (net.aserve.client:http-copy-file url temp-file)
  #-allegro
  (with-open-file (s temp-file :direction :output :element-type 'character)
    (write-sequence (drakma:http-request url :method :get) s))
  (format t "done.~%")
  (force-output)

  (let (header-line line lines location
	date latitude longitude magnitude
        (*read-default-float-format* 'single-float))
    (unwind-protect
	(with-open-file (s temp-file)
	  ;; This trick only works on UNIX:
	  #-os-windows (delete-file temp-file)
	  (setq header-line (read-line s nil s))
	  (when (eq header-line s) (error "no header?"))
	  (tagbody
	   top
	    (setq line (read-line s nil s))
	    (when (eq line s)
	      (return-from get-quake-info
		(nreverse lines)))
	    #-allegro
	    (multiple-value-bind (found res-vec)
		(cl-ppcre:scan-to-strings *quake-info-re* line)
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
		(match-re *quake-info-re* line)
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
	      (let ((place (location-to-place location)))
		(when (or
		       (null filter)
		       (not (find (cons place magnitude)
				  filter
				  :test
				  (lambda (item ref)
				    (and (string= (car item)
						  (car ref))
					 (< (cdr item)
					    (cdr ref)))))))
		  (push (list (if convert-date (quake-date-to-ut date) date)
			      place
			      location
			      magnitude)
			lines))))
	    (go top)))
      #+os-windows (delete-file temp-file))))

(defun quake-date-to-ut (date)
  #+allegro
  (truncate
   ;; quake data, apparently, is to fractional seconds
   (util.date-time:date-time-to-ut (util.date-time:date-time date)))
  #-allegro
  (cl-date-time-parser:parse-date-time date))

;; TODO: move this somewhere else
#+allegro (setq excl:*global-gc-behavior* :auto) ;; get rid of GC related messages

(let ((*package* (find-package :common-lisp))
      (*print-pretty* t))
  (format t ";; Try this:~%~W~%"
          '(get-quake-info
            (place-to-location "Oakland, CA")
            :period :week :larger-than nil :within 1.0f0))
  (finish-output))
