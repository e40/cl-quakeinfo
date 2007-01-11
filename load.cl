
(require :asdf)

(in-package :cl-user)

(setf asdf:*central-registry*
  `(*default-pathname-defaults*
    ,(or (probe-file #p"../measures/")
	 (probe-file #p"measures/"))
    ,(or (probe-file #p"../zipcodes/")
	 (probe-file #p"zipcodes/"))
    ,(or (probe-file #p"../google/")
	 (probe-file #p"google/"))))

;; asdf:: causes the next form to be read as if the current package
;; (*package*) was (find-package :asdf').
asdf::
(defmethod source-file-type ((c cl-source-file) (s module))
  ;; We redefine this method because ASDF doesn't handle the .cl
  ;; extension.  Be sure to revert it back to "lisp" if you .lisp as your
  ;; Common Lisp source file extension.
  "cl")

(asdf:operate 'asdf:load-op 'usgs)
