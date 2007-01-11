
(eval-when (compile eval load)
  (require :regexp2)
  (require :update)			; for update-http-copy-file
  (require :measures)
  (use-package :util.measures)
  (require :zipcodes)
  (use-package :util.zipcodes)
  (require :google-maps)
  (use-package :util.google.maps))

(in-package :cl-user)

;; So we don't have to specify it in each call to the google map API
(let ((file (merge-pathnames "google-maps-key.cl" *load-pathname*)))
  (when (probe-file file) (load file)))

(defvar *default-reference-location*
    (place-to-location "555 12th St, Oakland, CA, 94607, US"))

(defvar *usgs-gov-url-prefix* 
    "http://earthquake.usgs.gov/eqcenter/recenteqsww/catalogs/")

(defun get-quake-info (&key (period :week)
			    (within
			     ;; distance in decimal degrees
			     3.0)
			    (reference-location *default-reference-location*)
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
  (format t ";; Downloading data...")
  (force-output)
  (system.update::update-http-copy-file url temp-file)
  (format t "done.~%")
  (force-output)
  (let (header-line line lines location
	(re (compile-re
	     "[^,]+,[^,]+,[^,]+,\"([^\"]+)\",([^,]+),([^,]+),([^,]+),[^,]+,[^,]+$")))
    (unwind-protect
	(with-open-file (s temp-file)
	  (setq header-line (read-line s nil s))
	  (when (eq header-line s) (error "no header?"))
	  (loop
	    (setq line (read-line s nil s))
	    (when (eq line s)
	      (return (nreverse lines)))
	    (multiple-value-bind (found whole date latitude longitude
				  magnitude)
		(match-re re line)
	      (declare (ignore whole))
	      (when found
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
		  (format t "~a: ~a: magnitude=~a~%"
			  date (location-to-place location)
			  magnitude)
		  #+ignore
		  (push (list date (location-to-place location)
			      location magnitude)
			lines))))))
      (ignore-errors (delete-file temp-file)))))

(setq *global-gc-behavior* :auto) ;; get rid of GC related messages

(format t ";; Try this:~%")
(pprint
 '(get-quake-info
   :period :week
   :larger-than nil
   :within 1.0
   :reference-location 
   (place-to-location
    "555 12th St, Oakland, CA, 94607, US")))
