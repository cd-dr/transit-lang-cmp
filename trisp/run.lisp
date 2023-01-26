(load "trisp.lisp")
(ql:quickload 'alexandria)
(ql:quickload 'serapeum)
(ql:quickload 'ningle)
(ql:quickload 'clack)
(ql:quickload 'shasht)
;; (asdf:load-system :flamegraph)

(defun start-server ()
  (serapeum:mvlet* ((array-st map-st (trisp:get-stop-times trisp::STPATH))
		    (array-trips map-trip (trisp:get-trips trisp::TPATH))
		    (app (make-instance 'ningle:app)))
    (setf (ningle:route app "/schedules/:route")
	  #'(lambda (params)
	      ;; (trisp:get-json-resp (cdr (assoc :route params))
	      ;; 			   array-st map-st array-trips map-trip)))
	      `(200 (:content-type "application/json")
		    (,(trisp:get-json-resp2 (cdr (assoc :route params))
					    array-st map-st array-trips map-trip)))))
    ;; (format nil "~A" (trisp:get-json-resp2 (cdr (assoc :route params))
    ;; 			   array-st map-st array-trips map-trip))))
    (clack:clackup app :server :woo :port 4000 :worker-num 16)))
    ;; (clack:clackup app :port 4000)))

(start-server)

;; for debugging
;; (defconstant ROUTES #("Mattapan" "Orange" "Green-B" "Green-C" "Green-D" "Green-E" "Blue" "741" "742" "743" "751" "749" "746" "CR-Fairmount" "CR-Fitchburg" "CR-Worcester" "CR-Franklin" "CR-Greenbush" "CR-Haverhill" "CR-Kingston" "CR-Lowell" "CR-Middleborough" "CR-Needham" "CR-Newburyport" "CR-Providence" "CR-Foxboro" "Boat-F4" "Boat-F1" "Boat-EastBoston" "747" "708" "1" "4" "7" "8" "9" "10" "11" "14" "15" "16" "17" "18" "19" "21" "22" "23" "24" "26" "28" "29" "30" "31" "32" "33" "34" "34E" "35" "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "47" "50" "51" "52" "55" "57" "59" "60" "61" "62" "627" "64" "65" "66" "67" "68" "69" "70" "71" "72" "73" "74" "75" "76" "77" "78" "79" "80" "83" "84" "85"))

;; (defun profile-trisp ()
;;   (serapeum:mvlet* ((array-st map-st (trisp:get-stop-times trisp::STPATH))
;; 		    (array-trips map-trip (trisp:get-trips trisp::TPATH)))
;; 		   ;; (print (length ROUTES))
;; 		   ;; (flamegraph:save-flame-graph ("./trisp.stack")
;; 		     ;; (loop repeat 100 do
;; 		       (loop for route across ROUTES do
;; 			 (format t "~s ~a~%" route (length (trisp:build-json-response route
;; 						    array-st map-st array-trips map-trip))))))

;; (profile-trisp)

