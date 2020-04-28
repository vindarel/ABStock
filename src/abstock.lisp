(defpackage abstock
  (:use :cl
        :sxql
        :log4cl)
  (:import-from :defclass-std
                :defclass/std))

(in-package :abstock)

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


(defparameter *version* "0.4")

(defparameter *verbose* nil)
(defparameter *config* #P"~/.abstock.lisp")

(defun find-config ()
  (cond
    ((uiop:file-exists-p "config.lisp")
     "config.lisp")
    (t
     *config*)))

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

(defvar *old-cards* nil)

(defvar *shelves* nil)

(defvar *page-length* 50
  "Page length. Not used much yet.")

;;
;; Dev helpers.
;;
;; We don't want to print 3000+ strings, it hangs the editor and the server.
(setf *print-length* 10)

(defun connect ()
  ;TODO: needs to be run inside the directory of db.db
  (setf *connection*
        (dbi:connect :sqlite3
                     :database-name "db.db")))

(defun load-init ()
  "Read configuration variables (phone number,…) from the configuration file.
  Either `config.lisp' at the project's root, or `~/.abstock.lisp'. See `(find-config)'"
  (let ((file (uiop:native-namestring (find-config))))
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

;; same, search one card by isbn (dev only, unused in prod).
(defun card-by-isbn (isbn)
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
    ;TODO: needs a left-join for shelves.
    ; Currently returns a false shelf.
    (join :search_card_authors
          :on (:and (:= :search_card.id :search_card_authors.card_id)
                    (:= :search_author.id :search_card_authors.author_id))
          :on (:= :search_card.shelf :search_shelf.id))
    (join :search_card_publishers
          :on (:and (:= :search_card.id :search_card_publishers.card_id)
                    (:= :search_publisher.id :search_card_publishers.publisher_id)))
    (where (:= :search_card.isbn isbn))))

(defun search-isbn (isbn)
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (card-by-isbn isbn)))
                          (list isbn))))
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
    ;; left-join: accept cards without shelf.
    (left-join :search_shelf
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
(defun key-function (it)
  (getf it :|isbn|))

(defun search-by-isbn (cards isbn)
  (find isbn cards :key #'key-function :test #'string-equal))

(defun search-by-isbns (cards isbns)
  "Search many ISBNs.
  Return two values: the list of cards found, the list of ISBNs not found."
  (let (result collected-isbns)
    (loop for card in cards
       for card-isbn = (getf card :|isbn|)
       for res-isbn = (when (isbn-p card-isbn)
                        (find card-isbn isbns :test #'string-equal))
       when res-isbn
       do (progn
            (push res-isbn collected-isbns)
            (push card result)))
    (values result
            (set-difference isbns collected-isbns :test #'string-equal))))

(defun search-cards (query &key shelf)
  "cards: plist,
   query: string,
   shelf (optional): id (int)."
  (when (stringp shelf)
    (setf shelf (or (ignore-errors (parse-integer shelf))
                    -1)))

  (when (and (str:blank? query)
             (and shelf
                  (minusp shelf)))
    (return-from search-cards
      (values (subseq (get-cards) 0 (min *page-length*
                                         (length *cards*)))
              *page-length*)))

  (let* (isbns-not-found
         (cards (if (and shelf
                         (plusp shelf))
                    ;; Filter by shelf.
                    (remove-if-not (lambda (card)
                                     (= (or (getf card :|shelf_id|)
                                            -2)
                                        shelf))
                                   (get-cards))
                    (get-cards)))
         ;; If the query has ISBNs, search them and ignore a remaining free search.
         (query-isbns (collect-isbns (split-query query)))
         ;; Filter by title and author(s).
         (result (if (not (str:blank? query))
                     (cond
                       (query-isbns
                        (multiple-value-bind (res not-found)
                            (search-by-isbns cards query-isbns)
                          (setf isbns-not-found not-found)
                          res))
                       (t
                        ;; Strip internal contiguous whitespace,
                        ;; remove accents and punctuation.
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
                             collect card))))
                     cards)))
    (format t "Found: ~a~&" (length result))
    (values result
            (length result)
            isbns-not-found)))

(defun save (&key (file "cards.lisp"))
  "Save cards on file. Re-read with `load-cards'.
  Simply print the lisp forms."
  (with-open-file (f file
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :supersede)
    (let ((*print-pretty* nil))
      ;; Pretty printing will insert a line break at around 80 characters,
      ;; making it un-readable with uiop:read-file-form.
      (format f "~s~&" *cards*)))
  (format t "~&Cards saved on ~s.~&" file))

(defun reload-cards (&key (file "cards.lisp"))
  (if (uiop:file-exists-p file)
      (progn
        (setf *old-cards* *cards*)
        (setf *cards* (uiop:read-file-form file)))
      (format t "~&The file ~s doesn't exist.~&" file)))

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
