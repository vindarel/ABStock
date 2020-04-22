;; Read Abelujo's database in memory.

;; Book objects must have:
;; - title
;; - authors' representation
;; - price
;; - publisher
;; - ISBN
;; - cover image url
;; - shelf
;;
;; Optionally:
;; - year published
;; - details' url
;; - summary
;;
;; Asking for the quantity will be done next.

;; Notes:
;; A "card" is the generic name for a book.

(defpackage abstock
  (:use :cl
        ;; :mito
        :sxql
        :log4cl))

(in-package :abstock)

(defparameter *version* "0.1")

(defparameter *verbose* nil)
(defparameter *config* #P"~/.abstock.lisp")
(defvar *contact-infos* '(:|email| "me@test.fr"
                                :|phone| ""
                                :|phone2| "")

  "Private contact information, read from the config file `*config*'.")
(defvar *secret-question* ""
  "A question for the most simple anti-spam system when the user validates his basket.")
(defvar *secret-answer* "")

(defvar *connection* nil)

(defvar *cards* nil
  "List of all books.")

(defvar *shelves* nil)


(defun connect ()
  ;TODO: needs to be run inside the directory of db.db
  (setf *connection*
        (dbi:connect :sqlite3
                     :database-name "db.db")))

(defun load-init ()
  "Read configuration variables (phone number,…) from `*config*'."
  (let ((file (uiop:native-namestring *config*)))
    (let ((*package* *package*))
      (in-package abstock)
      (load file))))

(defun card-by-id (id)
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select (:search_card.title
           :search_card.price
           :search_card.id
           :search_card.isbn
           :search_card.details_url
           :search_card.cover
           :search_card.quantity
           (:as :search_author.name :author)
           (:as :search_shelf.name :shelf)
           (:as :search_publisher.name :publisher))
    (from :search_card
          :search_author
          :search_publisher
          :search_shelf)
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id))
          :on (:= :search_card.shelf :search_shelf.id))
    (join :search_card_publishers
          :on (:and (:= :search_card.id :search_card_publishers.card_id)
                    (:= :search_publisher.id :search_card_publishers.publisher_id)))
    (where (:= :search_card.id id))))

(defun search-card (id)
  "\o/"
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (card-by-id id)))
                          (list id))))

;;
;; Get all ids.
;;
(defun all-ids ()
  (select (:search_card.id)
    (from :search_card)
    ;; (where (:<= :search_card.id 200))
    ))

(defun get-all-ids ()
  "Get all the ids of the cards in the DB."
  (dbi:fetch-all (dbi:execute (dbi:prepare *connection* (yield (all-ids))))))

;;
;; Get all cards data.
;;
(defun all-cards ()
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select ((:distinct :search_card.id)
           :search_card.title
           :search_card.price
           :search_card.isbn
           :search_card.cover
           :search_card.details_url
           :search_card.date_publication
           :search_card.summary
           :search_card.quantity
           (:as :search_author.name :author)
           (:as :search_shelf.name :shelf)
           (:as :search_shelf.id :shelf_id) ;; cannot use a -
           (:as :search_publisher.name :publisher))
    (from :search_card
          :search_author
          :search_publisher)
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id)))
    (join :search_shelf
          :on (:= :search_card.shelf_id :search_shelf.id))
    (join :search_card_publishers
          :on (:and (:= :search_card.id :search_card_publishers.card_id)
                    (:= :search_publisher.id :search_card_publishers.publisher_id)))
    (where (:> :search_card.quantity 0))))

(defun get-all-cards ()
  "Get all the ids of the cards in the DB."
  (let* ((query (dbi:prepare *connection* (yield (all-cards))))
         (query (dbi:execute query (list 0))))
    ;; caution: what's long is printing all the cards.
    (log:info "(re)loading the DB")
    (setf *cards* (normalise-cards
                   (dbi:fetch-all query)))
    t))

(defun normalise-cards (cards)
  "Add a repr key that joins title and author and removes accents."
  (loop for card in cards
     for ascii-title = (slug:asciify (getf card :|title|))
     for ascii-author = (slug:asciify (getf card :|author|))
     for ascii-publisher = (slug:asciify (getf card :|publisher|))
     do (setf (getf card :|repr|)
              (str:downcase (str:concat ascii-title " " ascii-publisher)))
     do  (setf (getf card :|repr2|)
               (str:downcase (str:concat ascii-author " " ascii-title ascii-publisher)))
     collect card))

;;
;; Shelves
;;
(defun all-shelves ()
  (select (:search_shelf.name
           :search_shelf.id)
    (from :search_shelf)))

(defun get-all-shelves ()
  "Get shelves."
  (assert *connection*)
  (let* ((query (dbi:prepare *connection* (yield (all-shelves))))
         (query (dbi:execute query)))
    (setf *shelves* (dbi:fetch-all query))
    (setf *shelves* (sort-shelves *shelves*)))
  t)

(defun sort-shelves (shelves)
  #+sbcl
  (sort shelves #'sb-unicode:unicode< :key (lambda (it)
                                               (getf it :|name|)))
  #-sbcl
  (normalize-shelves shelves))

(defun normalize-shelves (shelves)
  "Sort shelves.
  A shelf name shouldn't start with an accentued letter, or it won't be sorted correctly..."
  ;; A poor man's text normalizer…
  (loop for elt in shelves
     when (str:starts-with? "É" (getf elt :|name|))
     do (setf (getf elt :|name|)
              (str:replace-all "É" "E" (getf elt :|name|))))
  (sort shelves (lambda (x y)
                      (string-lessp
                       (getf x :|name|)
                       (getf y :|name|)))))

;;
;; Search cards
;;
(defun search-cards (cards query &key shelf)
  "cards: plist,
   query: string,
   shelf (optional): id (int)."
  (let* ((cards (if (and shelf
                         (plusp shelf))
                    ;; Filter by shelf.
                    (remove-if-not (lambda (card)
                                     (= (getf card :|shelf_id|)
                                        shelf))
                                   cards)
                    cards))
         ;; Filter by title and author(s).
         (result (if (not (str:blank? query))
                     ;; no-case: strips internal contiguous whitespace, removes accents
                     ;; and punctuation.
                     (let* ((query (slug:asciify query))
                            (query (str:replace-all " " ".*" query)))
                       (loop for card in cards
                          for isbn = (getf card :|isbn|)
                          for repr = (getf card :|repr|)
                          for repr2 = (getf card :|repr2|)
                          when (or (string-equal (str:remove-punctuation query :replacement "")
                                                 isbn)
                                   (ppcre:scan query repr)
                                   (ppcre:scan query repr2))
                          collect card))
                     cards)))
    (format t "Found: ~a~&" (length result))
    (values result
            (length result))))

(defun schedule-db-reload ()
  "Reload the DB regularly. By default, each night at 4am."
  (cl-cron:make-cron-job #'get-all-cards :minute 30
                         :hour 4)
  (log:info "Scheduled a DB reload.")
  (cl-cron:start-cron))

(defun main ()
  "Entry point of the executable."
  (handler-case
      (progn
        (start)
        (schedule-db-reload)
        (bt:join-thread
         (find-if (lambda (th)
                    (search "hunchentoot" (bt:thread-name th)))
                  (bt:all-threads))))
    (sb-sys:interactive-interrupt () (progn
                                       (format *error-output* "User abort. Bye!~&")
                                       (uiop:quit)))
    (error (c) (format *error-output* "~&An error occured: ~A~&" c))))
