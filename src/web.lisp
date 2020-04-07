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
  (print :hello-root)
  (djula:render-template* +welcome.html+ nil
                          :title "La Palpitante en ligne"))

(easy-routes:defroute search-route ("/search" :method :get) (q)
  (format t "~& /search ~&")
  (let ((cards (search-cards (get-cards) q)))
    (djula:render-template* +cards.html+ nil
                            :title (format nil "La Palpitante - ~a" q)
                            :query q
                            :cards cards
                            :no-results (zerop (length cards)))))

(easy-routes:defroute panier-route ("/panier" :method :get) (ids)
  (format t "~& /panier ~&")
  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (allcards (subseq *cards* 0 10)) ;; DEV ;TODO:
         (cards (loop for id in (print ids-list)
                   for position = (position id allcards :key (lambda (card)
                                                               (getf card :|id|)))
                   when position
                   ;; do (setf (getf (elt *cards* position) :|basketqty|)
                   ;;          (count id ids-list))
                   collect  (elt *cards* position)))
         ;; (cards (remove-duplicates cards
         ;;                           :key (lambda (card)
         ;;                                  (getf card :|id|))))
         )
    ;; (format t "-- cards: ~a" cards)
    (djula:render-template* +panier.html+ nil
                            :title (format nil "La Palpitante - Mon Panier")
                            :cards cards
                            :contact *contact-infos*)))

(defun start ()
  (unless *connection*
    (setf *connection* (connect)))
  (load-init)

  (format t "~&Reading the DB...")
  (force-output)
  (get-all-cards)
  (format t "~&Done. ~a cards found." (length *cards*))
  (force-output)

  (format t "~&Starting the web server on port ~a" *port*)
  (force-output)
  (setf *server* (make-instance 'easy-routes:routes-acceptor
                                :port *port*))
  (hunchentoot:start *server*)
  (format t "~&Ready. You can access the application!~&")
  (force-output))
