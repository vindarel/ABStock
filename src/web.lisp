(in-package :abstock)

(defparameter *server* nil)

(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))

(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +welcome.html+ (djula:compile-template* "welcome.html"))
(defparameter +cards.html+ (djula:compile-template* "cards.html"))
(defparameter +panier.html+ (djula:compile-template* "panier.html"))

;; Custom Djula filter to format prices.
;; There is also the "format" filter.
(djula:def-filter :price (val)
  (format nil "~,2F" val))

#+nil
(defun render-base ()
  (with-output-to-string (s)
    (djula:render-template* +cards.html+ s
                            :cards (subseq *cards* 0 2))))

(setf *server* (make-instance 'easy-routes:routes-acceptor
                              :port 8899))

(easy-routes:defroute root ("/" :method :get) ()
  (print :hello-root)
  (djula:render-template* +welcome.html+ nil
                          :title "La Palpitante en ligne"))

(easy-routes:defroute search-route ("/search" :method :get) (q)
  (format t "~& /search ~&")
  (djula:render-template* +cards.html+ nil
                          :title (format nil "La Palpitante - ~a" q)
                          :cards (subseq *cards* 0 10)))

(easy-routes:defroute panier-route ("/panier" :method :get) (ids)
  (format t "~& /panier ~&")
  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (allcards (subseq *cards* 0 10)) ;; DEV
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
                          :cards cards)))

(defun start ()
  (unless *connection*
    (setf *connection* (connect)))
  (hunchentoot:start *server*))
