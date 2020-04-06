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

(defparameter *connection*
  (dbi:connect :sqlite3
               :database-name "db.db"))

(defun card-by-id (id)
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select (:search_card.title
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
(defun all-cards (&key id)
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select (:search_card.title
           (:as :search_card.id :id)
           (:as :search_card.price :price)
           (:as :search_card.isbn :isbn)
           (:as :search_card.cover :cover)
           (:as :search_card.details_url :details_url)
           (:as :search_card.date_publication :date_publication)
           (:as :search_card.summary :summary)
           (:as :search_author.name :author)
           (:as :search_shelf.name :shelf)
           ;; (:as :search_author.publishers :publishers)
           )
    (from :search_card
          :search_author
          :search_shelf)
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id))
          :on (:= :search_card.shelf :search_shelf.id))
    (when id
      (where (:= :search_card.id id)))
    ;; (where (:< :search_card.id 100))
    ))

(defparameter *cards* nil
  "List of all books.")

(defun get-all-cards ()
  "Get all the ids of the cards in the DB."
  ;; (setf *cards*
  ;;       (dbi:fetch-all (dbi:execute (dbi:prepare *connection* (yield (all-cards))))))
  (let* ((query (dbi:prepare *connection* (yield (all-cards))))
         (query (dbi:execute query)))
    ;; caution: what's long is printing all the cards.
    (setf *cards* (dbi:fetch-all query))
    ;; (loop for row = (dbi:fetch query)
    ;;    while row
    ;;    do (print row))
    )
  t
  )
