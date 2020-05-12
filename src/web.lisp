(in-package :abstock)

;;
;; App variables.
;;
(defvar *server* nil
  "Server instance (Hunchentoot acceptor).")

(defparameter *port* 8899
  "We can override it in the config file.")

(defvar *sentry-dsn-file* "~/.config/abstock/sentry-dsn.txt")

(defparameter *dev-mode* nil
  "If t, use a subset of all the cards.")

(defvar *selection* nil
  "List of cards for the selection page.")

;;
;; User variables.
;;
;; (defpackage :abstock-user
;;   (:use :cl)
;;   (:documentation "The package to write the user configuration in."))


;; Utils.

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
(defparameter +selection-page.html+ (djula:compile-template* "selection-page.html"))
(defparameter +card-page.html+ (djula:compile-template* "card-page.html"))

(defparameter +panier.html+ (djula:compile-template* "panier.html"))
(defparameter +command-confirmed.html+ (djula:compile-template* "command-confirmed.html"))

(defparameter +error-messages.html+ (djula:compile-template* "error-messages.html"))
(defparameter +404.html+ (djula:compile-template* "404.html"))

;;
;; Custom Djula filter to format prices.
;; There is also the "format" filter.
;;

(djula:def-filter :price (val)
  (format nil "~,2F" val))

(djula:def-filter :rest (list)
  (rest list))

(djula:def-filter :slugify (title)
  (slug:slugify title))

;; The truncatechars filter fails with a short shelf name like "BD",
;; because it is shorter than "...".
;; A PR was sent upstream. 4/4/2020
(setf djula::*elision-string* "…")

;;
;; Routes.
;;
(easy-routes:defroute root ("/" :method :get) ()
  (djula:render-template* +welcome.html+ nil
                          :contact *contact-infos*
                          :user-content *user-content*
                          ;TODO: dev
                          ;; :selection-cards (pick-cards :n 6) ;; random
                          :selection-cards (get-selection-subset :ensure-cover t)
                          :shelves *shelves*))

#+nil
(setf *selection-groups* (get-selection))
(easy-routes:defroute selection-route ("/selection-du-libraire" :method :get) ()
  (djula:render-template* +selection-page.html+ nil
                          :user-content *user-content*
                          ;; :selection *selection-groups* ;; (get-selection)
                          :selection (get-selection)
                          :shelves *shelves*))

(easy-routes:defroute search-route ("/search" :method :get) (q rayon)
  (format t "~& /search ~a, rayon: ~a~&" q rayon)
  (let* ((rayon (when rayon (ignore-errors (parse-integer rayon))))
         cards
         result-length
         isbns-not-found)
    (multiple-value-bind (res res-length not-found)
        (search-cards (slug:asciify (str:downcase q))
                      :shelf rayon)
      (setf cards res)
      (setf result-length res-length)
      (setf isbns-not-found not-found))
    (djula:render-template* +cards.html+ nil
                            :title (format nil "~a - ~a"
                                           (user-content-brand-name *user-content*)
                                           q)
                            :user-content *user-content*
                            :query q
                            :query-length (length (str:words q))
                            :shelf_id rayon
                            :cards cards
                            :isbns-not-found isbns-not-found
                            :length-isbns-not-found (length isbns-not-found)
                            :shelves *shelves*
                            :no-results (or (null result-length)
                                            (zerop result-length)))))

(easy-routes:defroute panier-route ("/panier" :method :get) (ids)
  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (filter-cards-by-ids ids-list)))
    (format t "~&basket ids: ~a, cards found: ~a~&" ids-list (length cards))
    (djula:render-template* +panier.html+ nil
                            :title (format nil "~a - ~a"
                                           (user-content-brand-name *user-content*)
                                           "Mon Panier")
                            :cards cards
                            :secret-question *secret-question*
                            :open-form t
                            :user-content *user-content*
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
                               :title (format nil "~a - ~a"
                                              (user-content-brand-name *user-content*)
                                              "Mon Panier")
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
                               :title (format nil "~a - ~a"
                                              (user-content-brand-name *user-content*)
                                              "Mon Panier")
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
             (log:info "email sent for client " name email)

             ;; Return success.
             (djula:render-template* +command-confirmed.html+ nil
                                     :title (format nil "~a - ~a"
                                                    (user-content-brand-name *user-content*)
                                                    "Commande envoyée")
                                     :success-messages (list "Votre demande a bien été envoyée."))
             )
         (error (c)
           (log:error "email error: sending to '~a' with ids '~a' (cards found: '~a' failed with the following error: ~a" email ids (length cards) c)

           ;; Try again to the admin, maybe it works.
           (ignore-errors
             (bt:make-thread
              (lambda ()
                (email-send :to (getf *email-config* :|from|)
                            :subject "Sending email failed"
                            :content (format nil "Sending an email failed, maybe this one passes. We tried sending to the addresse '~a', with ids '~a'.~%~%The error is:~&~a" name ids c)))
              :name :email-sender))

           ;; Return error message.
           (djula:render-template* +error-messages.html+ nil
                                   :messages (list "Votre demande n'a pas pu être envoyée. Merci de ré-essayer un peu plus tard."))
           ))))))

(defvar *card-url-name* "/livre/:id"
  "Name for the url that links to a book/a single product. It must contain a `:card' wildcard.")

(defun get-cards-same-author (card)
  (when card
    (sort
     (filter-cards-by-author (getf card :|author|)
                             :exclude-id (getf card :|id|))
     #+sbcl
     #'sb-unicode:unicode<
     #-sbcl
     (progn
       (log:warn "sorting by unicode string is only supported on SBCL.")
       #'string-lessp)
     :key (lambda (card)
            (getf card :|title|)))))

(easy-routes:defroute card-page ("/livre/:slug" :method :get) ()
  "Show a card.
  The slug must start with an id. The rest of the slug, the title, is not important."
  (let* ((card-id (ignore-errors
                    (parse-integer (first (str:split "-" slug)))))
         (res (when card-id
                (filter-cards-by-ids (list card-id))))
         (card (when res
                 (first res)))
         (same-author (when card
                        (get-cards-same-author card))))
    (cond
      ((null card-id)
       (djula:render-template* +404.html+ nil))
      (card
       (djula:render-template* +card-page.html+ nil
                               :card card
                               :user-content *user-content*
                               :same-author same-author))
      (t
       (djula:render-template* +404.html+ nil)))))

(easy-routes:defroute random-card ("/au-pif") ()
  (let* ((card (first (pick-cards :n 1)))
         (same-author (get-cards-same-author card)))
    (djula:render-template* +card-page.html+ nil
                            :card card
                               :user-content *user-content*
                            :same-author same-author)))

;; (easy-routes:defroute card-page (*card-url-name* :method :get) ()
;;   (djula:render-template* +card-page.html+ nil
;;                           :card card))

;;
;; Start.
;;
(defun start-server (&key (port *port*))
  (format t "~&Starting the web server on port ~a" port)
  (force-output)
  (setf *server* (make-instance 'easy-routes:routes-acceptor
                                :port (or port *port*)))
  (hunchentoot:start *server*))

(defun start (&key (port *port*) (load-init t) (load-db t))
  "If `load-db' is non t, do not load the DB, but try to load saved cards on disk."
  (format t "Abelujo visible stock v~a~&" *version*)
  (force-output)

  ;; Enable Sentry client.
  (unless *dev-mode*
    (handler-case
        (progn
          ;; sentry-client is not in Quicklisp.
          (when (uiop:file-exists-p *sentry-dsn-file*)
            (sentry-client:initialize-sentry-client
             (str:trim (str:from-file (uiop:native-namestring *sentry-dsn-file*)))
             :client-class 'sentry-client:async-sentry-client)
            (format t "~&Sentry client iniitiazied.~&")))
      (error (c)
        ;; it actually can hardly fail here since the dependency is in the .asd.
        (format *error-output* "~&*** Starting Sentry client failed: ~a~& ***" c))))

  ;; Load init file.
  (if load-init
      (progn
        (format t "Loading init file...~&")
        (load-init))
      (format t "Skipping init file.~&"))
  (force-output)

  ;; Load cards.txt if it exists.
  (uiop:format! t "Loading data from cards.txt~&")
  (setf *cards* (normalise-cards
                 (abstock/loaders:load-txt-data)))

  ;; Reload DB saved on disk.
  (format t "Reloading saved cards, before reading the DB…~&")
  (force-output)
  ;; This overwrites the previous cards from the txt loader.
  (reload-cards)
  (format t "~&Done.~&")
  (force-output)

  ;; Read the csv of cards to highlight in the selection page.
  (when (uiop:file-exists-p "selection.csv")
    (format t "Loading cards selection…~&")
    (force-output)
    (setf *selection* (read-selection))
    (format t "~&Done.~&")
    (force-output))

  ;; Start the web server.
  (start-server :port (or port *port*))
  (format t "~&Ready. You can access the application!~&")
  (force-output)

  ;; Load data from the DB.
  (if load-db
      (progn
        (unless *connection*
          (setf *connection* (connect)))

        (format t "~&Reading the DB...")
        (force-output)
        (get-all-cards)
        (get-all-shelves)
        (format t "~&Done. ~a cards found." (length *cards*))
        (force-output)
        (schedule-db-reload)
        (format t "~&Scheduled a DB reload every night.~&")
        (force-output)
        (save))
      (progn
        (format t "~&Skipped loading the DB.~&")
        (force-output))))
