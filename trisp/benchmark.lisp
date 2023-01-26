(load "trisp.lisp")
(ql:quickload "alexandria")

;; (time (trisp:get-stop-times trisp::STPATH))

;; read stop_times
(time (multiple-value-bind (sta stm) (trisp:get-stop-times trisp::STPATH)
  ;; (princ (length sta))
	;; (format t "~%~A~%~A" (aref sta 17) (gethash "CR-528712-1503" stm))
	))

;; read trips
(time (multiple-value-bind (tra trm) (trisp:get-trips trisp::TPATH)
  ;; (princ (length tra))
	;; (format t "~%~A~%~A" (aref tra 20) (gethash "741" trm))
	))
