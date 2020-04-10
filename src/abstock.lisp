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

(defparameter *config* #P"~/.abstock.lisp")
(defparameter *contact-infos* nil
  "Private contact information, read from the config file `*config*'.")

(defparameter *connection* nil)

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
           (:as :search_shelf.name :shelf))
    (from :search_card
          :search_author
          :search_shelf)
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id))
          :on (:= :search_card.shelf :search_shelf.id))
    (where (:= :search_card.id id))))

(defun search-card (id)
  "\o/"
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (card-by-id id)))
                          id)))

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
           (:as :search_shelf.id :shelf_id)) ;; cannot use a -
    (from :search_card
          :search_author)
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id)))
    (join :search_shelf
          :on (:= :search_card.shelf_id :search_shelf.id))
    (where (:> :search_card.quantity 0))))

(defparameter *cards* nil
  "List of all books.")

(defun get-all-cards ()
  "Get all the ids of the cards in the DB."
  (let* ((query (dbi:prepare *connection* (yield (all-cards))))
         (query (dbi:execute query 0)))
    ;; caution: what's long is printing all the cards.
    (setf *cards* (normalise-cards
                   (dbi:fetch-all query)))))

(defun normalise-cards (cards)
  "Add a repr key that joins title and author and removes accents."
  (loop for card in cards
     for ascii-title = (slug:asciify (getf card :|title|))
     for ascii-author = (slug:asciify (getf card :|author|))
     do (setf (getf card :|repr|)
              (str:concat ascii-title " " ascii-author))
     collect card))

;;
;; Shelves
;;
(defun all-shelves ()
  (select (:search_shelf.name
           :search_shelf.id)
    (from :search_shelf)))

(defparameter *shelves* nil)

(defun get-all-shelves ()
  "Get shelves."
  (let* ((query (dbi:prepare *connection* (yield (all-shelves))))
         (query (dbi:execute query)))
    (setf *shelves* (dbi:fetch-all query))
    (setf *shelves* (normalize-shelves *shelves*)))
  t)

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
(defparameter *result* nil
  "search-cards results. Avoid printing thousands of cards in the REPL.")

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
                     (loop for card in cards
                        for repr = (getf card :|repr|)
                        when (str:contains? (str:downcase query)
                                            (str:downcase repr))
                        collect card)
                     cards)))
    (format t "Found: ~a~&" (length result))
    (setf *result* result)
    (values result
            (length result))))

(defun main ()
  "Entry point of the executable."
  (handler-case
      (progn
        (start)
        (bt:join-thread
         (find-if (lambda (th)
                    (search "hunchentoot" (bt:thread-name th)))
                  (bt:all-threads))))
    (sb-sys:interactive-interrupt () (progn
                                       (format *error-output* "User abort. Bye!~&")
                                       (uiop:quit)))
    (error (c) (format *error-output* "~&An error occured: ~A~&" c))))
