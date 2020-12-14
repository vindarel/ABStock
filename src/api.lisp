(in-package :abstock)

;;
;; Routes for the API.
;;

(easy-routes:defroute api-selection-route ("/api/v1/selection.json" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (jojo:to-json *selection*))

(easy-routes:defroute api-newly-created-route ("/api/v1/lastcreated.json" :method :get) ()
  (setf (hunchentoot:content-type*) "application/json")
  (jojo:to-json (last-created-cards)))
