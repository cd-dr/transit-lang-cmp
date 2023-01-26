(defpackage trisp
  (:use :cl :cl-user)
  (:export :get-stop-times :get-trips :get-json-resp :get-json-resp2))

(in-package :trisp)
(ql:quickload 'split-sequence)
(ql:quickload 'shasht)
(ql:quickload 'st-json)

(declaim (optimize (speed 3) (safety 0) (space 1) (debug 1) (compilation-speed 0)))

(defstruct trip
  (trip-id "" :type string)
  (service-id "" :type string)
  (route-id "" :type string))

(defstruct stop-time
  (trip-id "" :type string)
  (stop-id "" :type string)
  (arrival "" :type string)
  (departure "" :type string))

(defconstant STPATH (truename "../MBTA_GTFS/stop_times.txt"))
(defconstant TPATH (truename "../MBTA_GTFS/trips.txt"))

(defun get-stop-times (path)
  (let ((st-array (make-array 100 :fill-pointer 0 :adjustable t))
	(trip-times-map (make-hash-table :test 'equal)))
    (with-open-file (in path)
      (do ((line #1=(read-line in nil 'eof) #1#)
	   (cnt 0 (incf cnt)))
	  ((eql line 'eof))
	(let* ((parts (split-sequence:split-sequence #\, line))
	       (tid (first parts))
	       (st (make-stop-time :trip-id tid
				   :arrival (second parts)
				   :departure (third parts)
				   :stop-id (fourth parts))))
	  (vector-push-extend st st-array)
	  (when (null (gethash tid trip-times-map))
	    (setf (gethash tid trip-times-map) '()))
	  (push cnt (gethash tid trip-times-map)))))
	  ;;   (setf (gethash tid trip-times-map) (make-array 1 :adjustable t :fill-pointer 0)))
	  ;; (vector-push-extend cnt (gethash tid trip-times-map)))))
    (values st-array trip-times-map)))
  

(defun get-trips (path)
  (let ((trip-array (make-array 80000 :fill-pointer 0))
	(trip-route-map (make-hash-table :test 'equal)))
    (with-open-file (in path)
      (do ((line #1=(read-line in nil 'eof) #1#)
	   (cnt 0 (incf cnt)))
	  ((eql line 'eof))
	(let* ((parts (split-sequence:split-sequence #\, line))
	       (rid (first parts))
	       (trp (make-trip :route-id rid
			       :service-id (second parts)
			       :trip-id (third parts))))
	  (vector-push trp trip-array)
	  (when (null (gethash rid trip-route-map))
	    (setf (gethash rid trip-route-map) '()))
	  (push cnt (gethash rid trip-route-map)))))
	  ;;   (setf (gethash rid trip-route-map) (make-array 1 :adjustable t :fill-pointer 0)))
	  ;; (vector-push-extend cnt (gethash rid trip-route-map)))))
    (values trip-array trip-route-map)))


(defstruct resp-schedule
  ;; stop-id arrival departure)
  (stop-id "" :type string)
  (arrival "" :type string)
  (departure "" :type string))

(defstruct resp-trip
  ;; trip-id service-id route-id schedules)
  (trip-id "" :type string)
  (service-id "" :type string)
  (route-id "" :type string)
  (schedules #() :type (array resp-schedule (*))))
    
(defun get-json-resp (route st-array trip-map trip-array route-map)
  "Expects (route string) (st-array (array stop-time (*))) (trip-map (hash-table string (trip-id) (array interger (*))))
   (trip-array (array trip (*))) (route-map (hash-table string (route-id) (array interger (*))))"
  (let* ((trip-ixs (gethash route route-map))
	 (resp-list (make-array (length trip-ixs))))
    (unless (null trip-ixs)
      (loop for tix across trip-ixs
	    for i from 0
	    do (let* ((tp (aref trip-array tix))
		      (st-ixs (gethash (trip-trip-id tp) trip-map))
		      (a-trip (make-resp-trip :trip-id (trip-trip-id tp)
					      :service-id (trip-service-id tp)
					      :route-id (trip-route-id tp)
					      :schedules (make-array (length st-ixs)))))
		 (unless (null st-ixs)
		   (loop for six across st-ixs
			 for j from 0
			 do (let ((st (aref st-array six)))
			      (setf (svref (resp-trip-schedules a-trip) j)
				    (make-resp-schedule :stop-id (stop-time-stop-id st)
							:arrival (stop-time-arrival st)
							:departure (stop-time-departure st))))))
		 (setf (svref resp-list i) a-trip)
		 )))
    ;; resp-list))
    (shasht:write-json resp-list nil)))
    ;; (shasht:write-json '() nil)))


(defun get-json-resp2 (route st-array trip-map trip-array route-map)
  "Expects (route string) (st-array (array stop-time (*))) (trip-map (hash-table string (trip-id) (list interger)))
   (trip-array (array trip (*))) (route-map (hash-table string (route-id) (list interger)))" 
  ;; (shasht:write-json
  (let ((r (mapcar #'(lambda (tix)
		       (let* ((tp (aref trip-array tix))
			      (schedules (mapcar #'(lambda (six)
						     (let* ((st (aref st-array six))
							    (sid (stop-time-stop-id st))
							    (arrv (stop-time-arrival st))
							    (dep (stop-time-departure st)))
						       `(:object-alist ("stop-id" . ,sid)
								       ("arrival" . ,arrv)
								       ("departure" . ,dep))))
						 (gethash (trip-trip-id tp) trip-map)))
			      (tid (trip-trip-id tp))
			      (sid (trip-service-id tp))
			      (rid (trip-route-id tp)))
			 `(:object-alist ("trip-id" . ,tid)
					 ("service-id" . ,sid)
					 ("route-id" . ,rid)
					 ("schedules" . (:array ,@schedules)))))
		   (gethash route route-map))))
    (shasht:write-json r nil)))
    ;; (shasht:write-json '() nil)))
