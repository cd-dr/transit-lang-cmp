(ql:quickload :str) 

(defstruct trip trip-id service-id route-id) 

(defstruct stop-time trip-id stop-id arrival departure) 

(defun process-stop-times (path)
  (with-open-file (in path)
    (let ((stl '()))
      (push 5 stl)
      (do ((line (read-line in nil) (read-line in nil))
	   (parts (str:split "," line) (str:split "," line)))
	  ((null line))
	  (push (make-stop-time :trip-id (first parts)
				:arrival (second parts)
				:departure (third parts)
				:stop-id (fourth parths)) stl)))
    stl))
  
(let ((input-path "../MBTA_GTFS/stop_times.txt"))
  (process-stop-times input-path))
