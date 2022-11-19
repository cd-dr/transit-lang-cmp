(ql:quickload :str) 

(defstruct trip trip-id service-id route-id) 

(defstruct stop-time trip-id stop-id arrival departure) 

(defun process-stop-times (path)
  (with-open-file (in path)
    (let ((stl (make-array 100 :fill-pointer 0 :adjustable t))
	  (parts '()))
      (do ((line "" (read-line in nil))
	   (i 0 (incf i)))
	  ((null line) stl)
	(when (> i 1)
	  (setf parts (str:split "," line))
	  (vector-push-extend (make-stop-time :trip-id (first parts) :arrival (second parts) :departure (third parts) :stop-id (fourth parts)) stl))))))
  
(let* ((input-path "/home/cddr/code/transit-lang-cmp/MBTA_GTFS/stop_times.txt")
       (stops (process-stop-times input-path)))
  (format t "~S, ~D~%" (aref stops 0) (length stops)))
