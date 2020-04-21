(in-package :abstock)

(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

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
(defparameter +command-confirmed.html+ (djula:compile-template* "command-confirmed.html"))

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
                          :contact *contact-infos*
                          :shelves *shelves*))

(easy-routes:defroute search-route ("/search" :method :get) (q rayon)
  (format t "~& /search ~a, rayon: ~a~&" q rayon)
  (let* ((rayon (when rayon (parse-integer rayon)))
         (cards (search-cards (get-cards)
                              (slug:asciify (str:downcase q))
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
                            :secret-question *secret-question*
                            :contact *contact-infos*)))

;;
;; Validate the basket: send an email, show a success message.
;;

(defun cards-to-txt (cards)
  "Return a string with the list of books to command."
  (with-output-to-string (s)
    (loop for card in cards
       do (format s
                  "- ~a; ~a; ~,2f€; ~a~&"
                  (getf card :|title|)
                  (getf card :|author|)
                  (getf card :|price|)
                  (getf card :|isbn|)))))

(defun email-content (name email phone payment message cards)
  (with-output-to-string (s)
    (format s "Bonjour cher libraire,~&~%~&Un nouveau client a commandé des livres.~&~%")
    (format s "Ses coordonnées sont: ~&- ~a~&- mail: ~a ~&- tél: ~a ~%"
            name email phone)
    (format s "~%Il/elle a commandé:~&~%~a~&~%" (cards-to-txt cards))
    (format s "Le total de la commande est de: ~,2F €.~&~%"
            (reduce #'+ cards :key (lambda (it) (getf it :|price|))))
    (format s "Moyen de paiement: ~a~&~%" payment)
    (when (not (str:blankp message))
      (format s "Et il/elle a ajouté ce petit mot:~%~%«~a»~%~%" (str:shorten 300 message)))
    (format s "Nous sommes le: ~a~&~%" (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
    (format s "À bientôt !~&")))

(easy-routes:defroute panier-validate-route ("/panier" :method :post) (&post name email phone payment antispam ids message)
  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (loop for id in ids-list
                   for position = (position id (get-cards) :key (lambda (card)
                                                                  (getf card :|id|)))
                   when position
                   collect  (elt *cards* position))))

    (cond
      ;; Validate the "antispam".
      ((not (string-equal *secret-answer* (str:downcase antispam)))
       (djula:render-template* +panier.html+ nil
                               :title (format nil "La Palpitante - Mon Panier")
                               :cards cards
                               :form-errors (list "La réponse à la question \"anti spam\" n'est pas bonne !")
                               :open-form t
                               :secret-question *secret-question*
                               :form-data `(:name ,name :email ,email :phone ,phone :message ,message)
                               :contact *contact-infos*))

      ;; Give one email or a phone.
      ((and (str:blankp phone)
            (str:blankp email))
       (djula:render-template* +panier.html+ nil
                               :title (format nil "La Palpitante - Mon Panier")
                               :form-errors (list "Veuillez renseigner un email ou un numéro de téléphone.")
                               :open-form t
                               :cards cards
                               :form-data `(:name ,name :email ,email :phone ,phone :message ,message)
                               :secret-question *secret-question*
                               :contact *contact-infos*))

      ;; Send email.
      (t
       (handler-case
           (progn
             (email-send :to (getf *email-config* :|to|)
                         :reply-to (list email name)
                         :subject "Commande site"
                         :content (email-content name email phone payment message cards))
             (log:info "email sent for client " name email))
         (error (c)
           (log:error "email error: sending to '~a' with ids '~a' (cards found: '~a' failed with the following error: ~a" email ids (length cards) c)

           ;; Try again to the admin, maybe it works.
           (ignore-errors
             (bt:make-thread
              (lambda ()
                (email-send :to (getf *email-config* :|from|)
                            :subject "Sending email failed"
                            :content (format nil "Sending an email failed, maybe this one passes. We tried sending to the addresse '~a', with ids '~a'.~%~%The error is:~&~a" name ids c)))
              :name :email-sender))))

       (djula:render-template* +command-confirmed.html+ nil
                               :title (format nil "La Palpitante - Commande envoyée")
                               :success-messages (list "Votre demande a bien été envoyée."))))))

(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(export 'start)
(defun start (&key (port *port*) (load-init t))
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

  (start-server :port (or port *port*))
  (format t "~&Ready. You can access the application!~&")
  (force-output))
