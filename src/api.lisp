(in-package :abstock)

;;
;; Routes for the API.
;;

(easy-routes:defroute api-selection-route ("/api/v1/selection.json" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (jojo:to-json *selection*))
