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

(defcached (search-cards-with-cache :timeout (* 60 5)) (query)
  "Search cards in stock with keywords.
  Results are cached for 5min."
  (search-cards query))

(easy-routes:defroute api-search-route ("/api/v1/search.json" :method :get) (query all)
  "Search with keywords.
If `all' is non-blank, return all cards in stock."
  (setf (hunchentoot:content-type*) "application/json")
  (cond
    ((str:non-blank-string-p all)
     (jojo:to-json (search-cards-with-cache "")))
    (t
     (jojo:to-json (if (str:non-blank-string-p query)
                       (search-cards-with-cache query)
                       (values))))))
