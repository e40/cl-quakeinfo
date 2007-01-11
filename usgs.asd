(defsystem usgs
    :components ((:file "usgs"))
    :depends-on ("measures" "zipcodes" "google"))
