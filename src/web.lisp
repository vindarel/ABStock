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
  "If non-nil, don't use Sentry and use a subset of all the cards.")

(defvar *selection* nil
  "List of cards for the selection page.")

(defparameter *theme* ""
  "Custom theme name (string).
  The theme templates are located at src/templates/<theme>/.")

;;
;; User variables.
;;
;; (defpackage :abstock-user
;;   (:use :cl)
;;   (:documentation "The package to write the user configuration in."))


;; Utils.
(defun get-template(template &optional theme)
  "Loads template from the base templates directory or from the given theme templates directory if it exists."
  (if (and (str:non-blank-string-p theme)
           (probe-file (asdf:system-relative-pathname "abstock" (str:concat "src/templates/themes/" theme "/" template))))
      ;; then
      (str:concat "themes/" theme "/" template)
      ;; else :D
      template))

(defun get-cards ()
  (if *dev-mode*
      (progn
        (format t "-- *dev-mode* activated: use a small subset of the DB.")
        (subseq *cards* 0 50))
      *cards*))

;;
;; Templates.
;;

;; Load our default templates.
(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))
;; and load the current theme's templates (if any).
(when (str:non-blank-string-p *theme*)
  (format-box t (format nil "Loading theme \"~a\" !" *theme*))
  (djula:add-template-directory
   (asdf:system-relative-pathname
    "abstock" (str:concat "src/templates/themes/" *theme* "/"))))

(defparameter +base.html+ (djula:compile-template* (get-template "base.html" *theme*)))
(defparameter +welcome.html+ (djula:compile-template* (get-template "welcome.html" *theme*)))
(defparameter +cards.html+ (djula:compile-template* (get-template "cards.html" *theme*)))
(defparameter +selection-page.html+ (djula:compile-template* (get-template "selection-page.html" *theme*)))
(defparameter +card-page.html+ (djula:compile-template* (get-template "card-page.html" *theme*)))

(defparameter +panier.html+ (djula:compile-template* (get-template "panier.html" *theme*)))
(defparameter +command-confirmed.html+ (djula:compile-template* (get-template "command-confirmed.html" *theme*)))

(defparameter +error-messages.html+ (djula:compile-template* (get-template "error-messages.html" *theme*)))
(defparameter +404.html+ (djula:compile-template* (get-template "404.html" *theme*)))

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

(djula:def-filter :url (card)
  "Create a full URL to uniquely identify this card."
  (format nil "/~a/~a-~a"
          *card-page-url-name*
          (getf card :|id|)
          ;; the slug won't actually be read back, only the id.
          (slug:slugify (getf card :|title|))))

;; The truncatechars filter fails with a short shelf name like "BD",
;; because it is shorter than "...".
;; A PR was sent upstream. 4/4/2020
(setf djula::*elision-string* "…")

;;;
;;; Serve static assets
;;;
(defparameter *default-static-directory* "src/static/"
  "The directory where to serve static assets from (STRING). If it starts with a slash, it is an absolute directory. Otherwise, it will be a subdirectory of where the system :abstock is installed.
  Static assets are reachable under the /static/ prefix.")

(defun serve-static-assets ()
  (push (hunchentoot:create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *default-static-directory*
                                     (asdf:system-source-directory :abstock)))
        hunchentoot:*dispatch-table*))

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

(easy-routes:defroute robots-route ("robots.txt" :method :get) ()
  (setf (hunchentoot:content-type*) "txt")
  (uiop:read-file-string "robots.txt"))

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

(defun total-command (cards)
  (reduce #'+ cards :key (lambda (it) (getf it :|price|))))

(defun email-content (name email phone payment message cards)
  (with-output-to-string (s)
    (format s "Bonjour cher libraire,~&~%~&Un nouveau client a commandé des livres.~&~%")
    (format s "Ses coordonnées sont: ~&- ~a~&- mail: ~a ~&- tél: ~a ~%"
            name
            email
            (format-phone-number phone))
    (format s "~%Il/elle a commandé:~&~%~a~&~%" (cards-to-txt cards))
    (format s "Le total de la commande est de: ~,2F €.~&~%"
            (total-command cards))
    (format s "Moyen de paiement: ~a~&~%" payment)
    (when (not (str:blankp message))
      (format s "Et il/elle a ajouté ce petit mot:~%~%«~a»~%~%" (str:shorten 300 message)))
    (format s "Nous sommes le: ~a~&~%" (local-time:format-timestring nil (local-time:now) :format local-time:+rfc-1123-format+))
    (format s "À bientôt !~&")))

(defun confirmation-email-content (cards)
  (with-output-to-string (s)
    (format s "Bonjour,~&~%")
    (format s "Nous vous confirmons votre commande des titres suivants:~&~%~a~&~%" (cards-to-txt cards))
    (format s "Le total de la commande est de: ~,2F €.~&~%"
            (total-command cards))
    (format s "Merci !")))

(defun send-confirmation-email (&key to name from reply-to cards)
  "Send a confirmation email to the client."
  (declare (ignorable name))
  (bt:make-thread (lambda ()
                    (email-send :to to
                                :from from
                                :reply-to reply-to
                                :subject "Confirmation de commande"
                                :content (confirmation-email-content cards)))
                  :name "confirmation-email"))

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
                               :user-content *user-content*
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
                               :user-content *user-content*
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

             ;; Send confirmation email to client.
             (send-confirmation-email :to email
                                      :name name
                                      :reply-to (list
                                                 (getf *email-config* :|to|)
                                                 (user-content-brand-name *user-content*))
                                      ;; "from" must be a validated address on SendGrid account.
                                      ;; Thankfully reply-to works with any address.
                                      :from (getf *email-config* :|from|)
                                      :cards cards)

             ;; Return success.
             (djula:render-template* +command-confirmed.html+ nil
                                     :title (format nil "~a - ~a"
                                                    (user-content-brand-name *user-content*)
                                                    "Commande envoyée")
                                     :success-messages (list "Votre demande a bien été envoyée.")
                                     :user-content *user-content*))
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
                                   :messages (list "Votre demande n'a pas pu être envoyée. Merci de ré-essayer un peu plus tard.")
                                   :user-content *user-content*)
           ))))))

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

(easy-routes:defroute card-page (#.*card-page-url* :method :get) ()
  "Show a card.
  The URL contains a :slug part.
  The slug must start with an id. The rest of the slug, the title, is not important."
  (let* ((card-id (ignore-errors
                    (parse-integer (first (str:split "-" slug)))))
         (card (when card-id
                (first
                 (filter-cards-by-ids (list card-id)))))
         (same-author (when card
                        (get-cards-same-author card)))
         (same-shelf (when card
                       (pick-cards :n 6
                                   :cards (filter-cards-by-shelf-id (getf card :|shelf_id|))
                                   :shelf-id (getf card :|shelf_id|)
                                   :exclude-id (getf card :|id|)))))
    (cond
      ((null card-id)
       (djula:render-template* +404.html+ nil))
      (card
       (djula:render-template* +card-page.html+ nil
                               :card card
                               :user-content *user-content*
                               :same-author same-author
                               :same-shelf same-shelf))
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
(defun get-port (port)
  (or port
      (ignore-errors (parse-integer (uiop:getenv "AB_PORT")))
      *port*))

(defun get-sentry-dsn ()
  "If not in `*dev-mode*', read the Sentry DSN from the SENTRY_DSN environment variable
  or the ~/.config/abstock/sentry-dsn.txt file (`*sentry-dsn-file*')."
  (unless *dev-mode*
    (or (uiop:getenv "SENTRY_DSN")
        (when (uiop:file-exists-p *sentry-dsn-file*)
          (str:trim (str:from-file (uiop:native-namestring *sentry-dsn-file*)))))))

(defun start-server (&key port)
  (let ((port (get-port port)))
    (uiop:format! t "~&Starting the web server on port ~a" port)
    (force-output)
    (setf *server* (make-instance 'easy-routes:easy-routes-acceptor
                                  :port port))
    (hunchentoot:start *server*)
    (serve-static-assets)))

(defun restart-server (&key port)
  (hunchentoot:stop *server*)
  (start-server :port (get-port port)))

(defun start (&key port (load-init t) (load-db t) (post-init t))
  "If `load-db' is non t, do not load the DB, but try to load saved cards on disk."
  (uiop:format! t "ABStock v~a~&" *version*)

  ;; Enable Sentry client.
  (unless *dev-mode*
    (handler-case
        (let ((dsn (get-sentry-dsn)))
          ;; sentry-client is not in Quicklisp.
          (if dsn
              (progn
                (sentry-client:initialize-sentry-client dsn
                                                        :client-class 'sentry-client:async-sentry-client)
                (uiop:format! t "~&Sentry client initialized.~&"))
              (uiop:format! t "~&Sentry was not initialized.~&")))
      (error (c)
        ;; it actually can hardly fail here since the dependency is in the .asd.
        (uiop:format! *error-output* "~&*** Starting Sentry client failed: ~a~& ***" c))))

  ;; Load init file.
  (if load-init
      (progn
        (uiop:format! t "Loading init file...~&")
        (load-init))
      (uiop:format! t "Skipping init file.~&"))

  ;; Load cards.txt if it exists.
  (uiop:format! t "Loading data from cards.txt~&")
  (setf *cards* (normalise-cards
                 (abstock/loaders:load-txt-data)))

  ;; Reload DB saved on disk.
  (uiop:format! t "Reloading saved cards, before reading the DB…")
  ;; This overwrites the previous cards from the txt loader.
  (reload-cards)
  (uiop:format! t "~&Done.~&")

  ;; Read the csv of cards to highlight in the selection page.
  (when (uiop:file-exists-p "selection.csv")
    (uiop:format! t "Loading cards selection…")
    (setf *selection* (read-selection))
    (uiop:format! t "~&Done.~&"))

  ;; Start the web server.
  (start-server :port port)
  (uiop:format! t "~&~a~&" (cl-ansi-text:green "✔ Ready. You can access the application!"))

  ;; Load data from the DB.
  (if load-db
      (progn
        (unless *connection*
          (setf *connection* (connect)))

        (uiop:format! t "~&Reading the DB...")
        (get-all-cards)
        (uiop:format! t "~&Reading all shelves...")
        (get-all-shelves)
        (uiop:format! t "~&Done. ~a cards found." (length *cards*))
        (schedule-db-reload)
        (uiop:format! t "~&Scheduled a DB reload every night.~&")
        (save))
      (progn
        (uiop:format! t "~&Skipped loading the DB.~&")))

  ;; Post-init: overwrite code.
  (if post-init
      (progn
        (load-post-init))
      (uiop:format! t "Skipping post-init file.~&")))

(defun stop ()
  (hunchentoot:stop *server*))
