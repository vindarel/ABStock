(in-package :abstock)

(defparameter *server* nil)

(defparameter *port* 8899
  "We can override it in the config file.")

(defparameter *dev-mode* nil
  "If t, use a subset of all the cards.")

(defun get-cards ()
  (if *dev-mode*
      (progn
        (format t "-- *dev-mode* activated: use a small subset of the DB.")
        (subseq *cards* 0 50))
      *cards*))

;;
;; Templates.
;;
(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +welcome.html+ (djula:compile-template* "welcome.html"))
(defparameter +cards.html+ (djula:compile-template* "cards.html"))

(defparameter +panier.html+ (djula:compile-template* "panier.html"))

;;
;; Custom Djula filter to format prices.
;; There is also the "format" filter.
;;

(djula:def-filter :price (val)
  (format nil "~,2F" val))

;;
;; Routes.
;;
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template* +welcome.html+ nil
                          :title "La Palpitante en ligne"
                          :shelves *shelves*))

(easy-routes:defroute search-route ("/search" :method :get) (q rayon)
  (format t "~& /search ~a, rayon: ~a~&" q rayon)
  (let* ((rayon (when rayon (parse-integer rayon)))
         (cards (search-cards (get-cards)
                              (slug:asciify q)
                              :shelf rayon)))
    (djula:render-template* +cards.html+ nil
                            :title (format nil "La Palpitante - ~a" q)
                            :query q
                            :query-length (length (str:words q))
                            :shelf_id rayon
                            :cards cards
                            :shelves *shelves*
                            :no-results (zerop (length cards)))))

(easy-routes:defroute panier-route ("/panier" :method :get) (ids)
  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (loop for id in ids-list
                   for position = (position id (get-cards) :key (lambda (card)
                                                               (getf card :|id|)))
                   when position
                   collect  (elt *cards* position))))
    (format t "~&basket ids: ~a, cards found: ~a~&" ids-list (length cards))
    (djula:render-template* +panier.html+ nil
                            :title (format nil "La Palpitante - Mon Panier")
                            :cards cards
                            :contact *contact-infos*)))

(export 'start)
(defun start (&key port (load-init t))
  (format t "Abelujo visible stock v~a~&" *version*)
  (force-output)

  (unless *connection*
    (setf *connection* (connect)))
  (if load-init
      (progn
        (format t "Loading init file...~&")
        (load-init))
      (format t "Skipping init file.~&"))
  (force-output)

  (format t "~&Reading the DB...")
  (force-output)
  (get-all-cards)
  (get-all-shelves)
  (format t "~&Done. ~a cards found." (length *cards*))
  (force-output)

  (format t "~&Starting the web server on port ~a" (or port *port*))
  (force-output)
  (setf *server* (make-instance 'easy-routes:routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*)
  (format t "~&Ready. You can access the application!~&")
  (force-output))
