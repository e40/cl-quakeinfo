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
			    filter
			    convert-date
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
      #+mswindows (delete-file temp-file))))

;; Crikey, why use this date format????
(defun quake-date-to-ut (date)
  ;; Turn "December 25, 2006 13:43:07 GMT" into a ut.
  (flet ((cvt (str start-end)
	   (let ((res 0))
	     (do ((i (car start-end) (1+ i))
		  (end (cdr start-end)))
		 ((>= i end) res)
	       (setq res 
		 (+ (* 10 res)
		    (- (char-code (schar str i)) #.(char-code #\0))))))))
    #-allegro (error "quake-date-to-ut not converted yet")
    #+allegro
    (multiple-value-bind (found whole
			  day-name month day year
			  hour minute second)
	(match-re
	 "([A-Z][a-z]+),[ ]+([A-Z][a-z]+)[ ]+(\\d+),[ ]+(\\d+)[ ]+(\\d+):(\\d+):(\\d+) (GMT|UTC)"
	 date
	 :return :index)
      (declare (ignore whole day-name))
      (when found
	(return-from quake-date-to-ut
	  (encode-universal-time
	   (cvt date second)
	   (cvt date minute)
	   (cvt date hour)
	   (cvt date day)
	   (compute-month date (car month))
	   (cvt date year)
	   0))))
    
    (error "Couldn't parse date: date-string")))

(defun compute-month (str start-index)
  ;; return the month number given a 3char rep of the string
  (case (schar str start-index)
    (#\A  
     (if* (eq (schar str (1+ start-index)) #\p)
	then 4				; april
	else 8))			; august
    (#\D 12)				; dec
    (#\F 2)				; feb
    (#\J
     (if* (eq (schar str (1+ start-index)) #\a)
	then 1				; jan
      elseif (eq (schar str (+ 2 start-index)) #\l)
	then 7				; july
	else 6))			; june
    (#\M
     (if* (eq (schar str (+ 2 start-index)) #\r)
	then 3				; march
	else 5))			; may
    (#\N 11)				; nov
    (#\O 10)				;oct
    (#\S 9)))

#+allegro
(setq *global-gc-behavior* :auto) ;; get rid of GC related messages

(format t ";; Try this:~%")
(pprint
 '(get-quake-info
   (place-to-location "Oakland, CA")
   :period :week
   :larger-than nil :within 1.0))
