(defpackage abstock
  (:use :cl
        :sxql)
  (:import-from :defclass-std
                :defclass/std)
  (:import-from :function-cache
                :defcached)
  (:import-from :access
                :access)
  (:import-from :serapeum
                :dict)
  (:export :main
           :start))

(in-package :abstock)

;; Read Abelujo's database in memory.
;; XXX: in-memory SQLite.

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


(defparameter *version* "0.11")          ; duplicated in .asd

(defparameter *banner* "


   ###    ########   ######  ########  #######   ######  ##    ##
  ## ##   ##     ## ##    ##    ##    ##     ## ##    ## ##   ##
 ##   ##  ##     ## ##          ##    ##     ## ##       ##  ##
##     ## ########   ######     ##    ##     ## ##       #####
######### ##     ##       ##    ##    ##     ## ##       ##  ##
##     ## ##     ## ##    ##    ##    ##     ## ##    ## ##   ##
##     ## ########   ######     ##     #######   ######  ##    ##


")
;; banner3 on http://www.patorjk.com/software/taag/#p=display&h=1&v=1&f=Banner3&t=ABStock
;; also "big"


(defparameter *verbose* nil)
(defparameter *config* #P"config.lisp")
(defparameter *post-config* #P"post-config.lisp")

(defun find-config ()
  (cond
    ((uiop:file-exists-p (asdf:system-relative-pathname :abstock "config.lisp"))
     (asdf:system-relative-pathname :abstock "config.lisp"))
    ((uiop:file-exists-p *config*)
     *config*)
    (t
     nil)))

(defun find-post-config ()
  (cond
    ((uiop:file-exists-p *post-config*)
     *post-config*)
    (t
     nil)))

(defvar *contact-infos* '(:|email| "me@test.fr"
                                :|phone| ""
                                :|phone2| "")

  "Private contact information, read from the config file `*config*'.")
(defvar *secret-question* ""
  "A question for the most simple anti-spam system when the user validates his basket.")
(defvar *secret-answer* "")

(defvar *connection* nil)

(defvar *db-name* "db.db" "The DB name.")

(defvar *cards* nil
  "List of all books we display.")

(defparameter *deposit-cards* nil
  "The list of all cards in all deposits.")

(defvar *old-cards* nil)

(defvar *old-shelves* nil)
(defvar *shelves* nil)
(defvar *ignore-shelves-ids* nil
  "Ignore (don't show) these shelves. Cards that are only present in
  these shelves will still be shown. To correctly ignore those cards,
  we have to use proper lists for returns, which would remove their
  cards from the stock. This is not yet implemented in Abelujo.")

(defvar *ignore-shelves-starting-by* nil
  "List of strings. Ignore the shelves whose name starts by one of them.")

(defvar *page-length* 100
  "Page length: number of elements per page to show.")
;;
;; Dev helpers.
;;
;; We don't want to print 3000+ strings, it hangs the editor and the server.

(setf *print-length* 100)

(defun get-db-name ()
  "Get the db.db full path, relative to our installation"
  (asdf:system-relative-pathname :abstock *db-name*))

(defun connect ()
  ;TODO: needs to be run inside the directory of db.db
  (if (uiop:file-exists-p *db-name*)
      (setf *connection*
            (dbi:connect :sqlite3
                         :database-name (get-db-name)))
      (error "The DB file ~a does not exist." *db-name*)))

(defun load-init ()
  "Read configuration variables (phone number,…) from the configuration file.
  Either `config.lisp' at the project's root, or the CONFIG environment variable. See `(find-config)'"
  (let ((file (or (uiop:getenv "CONFIG")
                  (find-config))))
    (if file
     (let ((*package* *package*))
       (in-package abstock)
       ;; XXX: one case of failure: a symbolic link exists, but
       ;; the target file doesn't.
       (load (uiop:native-namestring file)))
     (format t "... no config file found.~&"))))

(defun load-post-init ()
  "Overwrite code.
  Load the file denoted by either `post-init.lisp' at the project's root (see `*post-config*'), or the POST_CONFIG environment variable."
  (let ((file (or (uiop:getenv "POST_CONFIG")
                  (find-post-config))))
    (if file
        (let ((*package* *package*))
          (in-package abstock)
          ;; XXX: one case of failure: a symbolic link exists, but
          ;; the target file doesn't.
          (uiop:format! t "~&Loading post-init file ~a~&" (uiop:native-namestring file))
          (load (uiop:native-namestring file)))
        (format t "... no post-config file found.~&"))))

(defun query-card-by-id (id)
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select (:search_card.title
           :search_card.price
           :search_card.id
           :search_card.isbn
           :search_card.details_url
           :search_card.cover
           :search_card.quantity
           :search_card.is_catalogue_selection
           :search_card.summary
           :search_card.width
           :search_card.height
           :search_card.thickness
           :search_card.weight
           :search_card.presedit
           :search_card.meta
           (:as :search_author.name :author)
           (:as :search_author.bio :author_bio)
           (:as :search_shelf.name :shelf)
           (:as :search_publisher.name :publisher)
           (:as :search_publisher.address :publisher_city))
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
  "Search a card by id in the DB."
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (query-card-by-id id)))
                          (list id))))
;;
;; Get all ids.
;;

(defun query-all-ids ()
  (select (:search_card.id)
    (from :search_card)
    ;; (where (:<= :search_card.id 200))
    ))

(defun get-all-ids ()
  "Get all the ids of the cards in the DB."
  (dbi:fetch-all (dbi:execute (dbi:prepare *connection* (yield (query-all-ids))))))
;; same, search one card by isbn (dev only, unused in prod).

(defun query-card-by-isbn (isbn)
  "Generates an SxQL query. (yield) generates the SQL. It is not executed."
  (select ((:distinct :search_card.id)
           :search_card.created
           :search_card.price
           :search_card.title
           :search_card.isbn
           :search_card.details_url
           :search_card.cover
           :search_card.quantity
           :search_card.is_catalogue_selection
           :search_card.summary
           :search_card.width
           :search_card.height
           :search_card.thickness
           :search_card.weight
           :search_card.presedit
           :search_card.meta
           (:as :search_author.name :author)
           (:as :search_author.bio :author_bio)
           (:as :search_shelf.name :shelf)
           (:as :search_publisher.name :publisher)
           (:as :search_publisher.address :publisher_city))
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
  "Search a card by ISBN in the DB.
  Returns a plist."
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (query-card-by-isbn isbn)))
                          (list isbn))))
;;
;; Get all cards data.
;;

(defun query-all-cards (&key (order :desc))
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select ((:distinct :search_card.id)
           :search_card.created
           :search_card.title
           :search_card.title_ascii
           :search_card.price
           :search_card.isbn
           :search_card.cover
           :search_card.details_url
           :search_card.date_publication
           :search_card.summary
           :search_card.quantity
           :search_card.is_catalogue_selection
           :search_card.summary
           :search_card.width
           :search_card.height
           :search_card.thickness
           :search_card.weight
           :search_card.presedit
           :search_card.meta
           (:as :search_author.name :author)
           (:as :search_author.name_ascii :author_ascii)
           (:as :search_author.bio :author_bio)
           (:as :search_shelf.name :shelf)
           (:as :search_shelf.id :shelf_id) ;; cannot use a -
           (:as :search_publisher.name :publisher)
           (:as :search_publisher.name_ascii :publisher_ascii)
           (:as :search_publisher.address :publisher_city))

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
          ;; DEV: don't filter on the quantity yet, to get the ones in deposits.
          ;; (where (:> :search_card.quantity 0))
          (order-by `(,order :search_card.created))))

(defun get-all-cards ()
  "Get all the ids of the cards in the DB."
  (if (uiop:file-exists-p *db-name*)
      (let* ((query (dbi:execute (dbi:prepare *connection*
                                              (yield (query-all-cards)))))
             ;; DB query:
             (cards (dbi:fetch-all query)))
        ;; caution: what's long is printing all the cards.
        (log:info "(re)loading the DB")
        (setf *cards* (normalise-cards
                       (remove-duplicated-cards
                        (merge-cards-and-deposits cards
                                                  (get-deposit-id/quantities)))))
        t)
      (warn "The DB file ~a does not exist. We can't load data from the DB.~&" *db-name*)))

(defun query-deposit-ids/quantities ()
  "Get the cards in deposits, with their current quantity in deposit.
  All cards are returned.
  We must filter them more manually."
  (select ((:distinct :search_card.id)
           (:as :search_depositstatecopies.nb_current :nb_current_deposit))
    (from :search_depositstatecopies)
    (join :search_card
          :on (:= :search_card.id :search_depositstatecopies.card_id))
    ;; where clause: don't forget dbi:execute argument (re-give the 0).
    (where (:> :nb_current 0))))

(defun get-deposit-id/quantities ()
  "Return a plist of id and nb_current of the card in deposits.
  See id/quantities-as-dict to transform the list as a hash-table, and merge-cards-and-deposits to merge and filter all cards."
  (let* ((query (dbi:execute
                 (dbi:prepare *connection*
                              (yield (query-deposit-ids/quantities)))
                 (list 0))))
    (log:info "(re)loading the deposits")
    (setf *deposit-cards* (dbi:fetch-all query))
    (values *deposit-cards* (length *deposit-cards*))))

(defun id/quantities-as-dict (plist)
  "Helper function. Create a hash-table with this plist.
   key: the :id.
   val: the :nb_current"
  (loop for elt in plist
     with ht = (dict)
     for id = (access elt :|id|)
     for nb = (access elt :|nb_current_deposit|)
     do (setf (gethash id ht)
              nb)
     finally (return ht)))

(defun merge-cards-and-deposits (cards deposit-cards)
  "Helper function. Merge the two lists, return cards that are either in stock, either in deposits.
  Return: a list of cards.

  cards: list of plist
  deposit-cards: plist with only id and nb_current (in deposit)"
  (loop for card in cards
     with id/nb = (id/quantities-as-dict deposit-cards)
     for quantity = (access card :|quantity|)
     for nb_current_deposit = (access id/nb (access card :|id|))
     if (and quantity
             (plusp quantity))
     collect card
     else
     if (and nb_current_deposit
             (plusp nb_current_deposit))
     collect card))

(defun slugify-details-url (card)
  "Return a slug to identify this card, relative to the server root URL.
  Example: /livre/1-trop-bon-la-courgette"
  ;; the reverse operation is
  ;; (parse-integer (first (str:split "-" slug)))
  ;; not in a function yet.
  (format nil "/livre/~a-~a" (access card :|id|)
          (slug:slugify (access card :|title_ascii|))))

(defun normalise-cards (cards)
  "Add a repr key that joins title and author.
  Change details_url to a short, slugified URL relative to the server root.
  XXX: we should keep the initial details_url and add a new field."
  (loop for card in cards
     ;; access is more generic than getf, it also works with uninterned symbols
     ;; (but we don't have such anymore).
     for title_ascii = (access card :|title_ascii|)
     for author_ascii = (access card :|author_ascii|)
     for publisher_ascii = (access card :|publisher_ascii|)
     do (setf (getf card :|repr|)
              (str:downcase (str:concat title_ascii " " publisher_ascii)))
     do  (setf (getf card :|repr2|)
               (str:downcase (str:concat author_ascii " " title_ascii " " publisher_ascii)))
     do (setf (getf card :|details_url|)
              (slugify-details-url card))
     collect card))

(defun sort-cards-by-creation-date (cards)
  "Return a copy of `cards' sorted by creation date, newly created first."
  (sort (copy-seq cards)
        #'string-not-lessp
        :key (lambda (it)
               (access it :|created|))))

(defcached (last-created-cards :timeout (* 60 60)) (&key (n 20))
  "Return the last 20 most recent cards added in stock.
  Results are cached for 1h."
  (subseq (sort-cards-by-creation-date *cards*) 0 n))

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
    (setf *shelves* (filter-shelves *shelves*))
    (setf *shelves* (remove-empty-shelves *shelves*))
    (setf *shelves* (sort-shelves *shelves*))
    (setf *shelves* (sort-shelves-by-number-prefix *shelves*)))
  t)

(defun shelf-name-matches-p (list-of-strings name)
  (loop for string in list-of-strings
     when (str:starts-with-p string name)
     return t))

(defun filter-shelves (shelves &key
                                 (ids *ignore-shelves-ids*)
                                 (names-starting-by *ignore-shelves-starting-by*))
  "Return a new list of shelves, without the ones matching the given ids, or whose name starts by the given prefixes."
  (loop for shelf in shelves
     unless (or (shelf-name-matches-p names-starting-by (access shelf :name))
                (find (access shelf :id) ids))
     collect shelf))

(defun remove-empty-shelves (shelves)
  "Remove shelves whose id we can't find in the current list of cards.
  Return a new list of shelves (the global *shelves* is not set here)."
  ;; Not done from SQL. Not so unefficient...
  (let ((new-list '()))
    (dolist (shelf shelves)
      (if (find (access shelf :|id|)
                *cards*
                :key (lambda (it) (access it :|shelf_id|)))
          (push shelf new-list)
          (format t "This shelf is empty: ~a (~a)~&"
                  (access shelf :|name|)
                  (access shelf :|id|))))
    (reverse new-list)))
#+abstock-tests
(let ((shelves '((:|name| "Aix-en-Provence" :|id| 68) ;; <-- present
                 (:|name| "BD" :|id| 73)))            ;; <-- NOT present
      (*cards* '((:|repr2| "collectif lisp " :|repr| "les sons de lisp'" :|id| 3078
                  :|shelf| "Aix-en-Provence"
                  :|shelf_id| 68
                  :|publisher| "Académie d'Aix"
                  :|publisher_city| NIL))))

  (let ((new-list (remove-empty-shelves shelves)))
    (assert (and (= 1 (length new-list))
                 (= 68 (access (first new-list)
                               :|id|))))))

(defun sort-shelves (shelves)
  #-sbcl
  (progn
    (log:warn "sorting shelves: on this implementations, shelves are only sorted by ASCII characters, not unicode. Thus, accentuated letters will come last.")
    (normalize-shelves shelves))
  #+sbcl
  (sort (copy-seq shelves) #'sb-unicode:unicode< :key (lambda (it)
                                                        (getf it :|name|))))

(defun sort-shelves-by-number-prefix (shelves)
  "A second pass to sort. The first pass sorted alphabetically.
  A shelf named \"10.1 xyz\" needs to come after \"1.1 abc\"."
  (sort (copy-seq shelves)
        #'<
        :key (lambda (it)
               (let ((str-prefix (ppcre:scan-to-strings "^[0-9]+" (access it :name))))
                 (or (ignore-errors (parse-float:parse-float str-prefix))
                     -1)))))

#+(or nil)
(let* ((shelves '((:|name| "Zzz")
                 (:|name| "10-12 / Foo")
                 (:|name| "Jeunesse")
                 (:|name| "Jeunesse / 10-11 ans") ))
       (sorted-shelves (sort-shelves shelves)))
  (assert (string-equal "Zzz"
                        (access (car (last sorted-shelves))
                                :|name|)))
  (assert (str:starts-with? "10"
                            (access (first sorted-shelves)
                                    :|name|))))

(defun normalize-shelves (shelves)
  "Sort shelves.
  A shelf name shouldn't start with an accentued letter, or it won't be sorted correctly..."
  ;; A poor man's text normalizer…
  (loop for elt in (copy-seq shelves)
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

(defun search-cards (query &key shelf (page 1))
  "cards: plist,
   query: string,
   shelf (optional): id (int)
   page (optional): int (or string).

  Return: multiple values:
  - the list of items to display
  - page length
  - the number of ISBNs not found (in the case of a search, otherwise nil)
  - the pagination object (make-pagination)"
  (when (stringp shelf)
    (setf shelf (or (ignore-errors (parse-integer shelf))
                    -1)))
  (setf page (or (ignore-errors (parse-integer page))
                 1))

  ;; Asking all titles. Return a subset.
  ;; Use pagination.
  (when (and (str:blank? query)
             (and shelf
                  (minusp shelf)))
    (return-from search-cards
      (let* ((results (get-cards))
             (pagination (make-pagination :page page
                                          :page-size *page-length*
                                          :nb-elements (length results))))
        (values (get-page-items results pagination)
                *page-length*
                nil ;; ISBNs not found.
                pagination))))

  ;; Searching in a shelf.
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
    (let ((pagination (make-pagination :page page
                                       :page-size *page-length*
                                       :nb-elements (length result))))
      (format t "Found: ~a. Pagination: ~S ~&" (length result) pagination)
      ;; Return a subset of the results. It blows the stack(?) with 6.000 titles.
      (values (get-page-items result pagination)
              (length result)
              isbns-not-found
              pagination))))

(defun get-page-items (elements pagination)
  "From this list of items and this pagination object, return the subset of the list.

  Supposing we have our entire list of results in memory. We are not querying a DB here."
  (assert (hash-table-p pagination) nil
          "the pagination object is expected to be a hash table. Otherwise, use access:access below.")
  (let* ((page (or (gethash :page pagination 1)
                   1))
         (start (max 0
                     (* (1- page)
                        (gethash :page-size pagination 200))))
         (end (* page
                 (gethash :page-size pagination 200))))
    ;; Be defensive.
    (when (> start end)
      (return-from get-page-items nil))
    (when (> start (gethash :nb-elements pagination))
      (return-from get-page-items nil))
    ;; subseq* doesn't care about a too large end index.
    ;; (but start must be in range).
    (alexandria-2:subseq* elements
                          ;; start: page number
                          start
                          ;; end: (page number + 1) * page size
                          end)))
#+test-abstock
(assert
 (and (let ((items '(:a :b :c :d :e)))
        (get-page-items items (get-pagination :page-size 2
                                              :page 3
                                              :NB-ELEMENTS  (length items))))
      (equal '(:a :b)
             (let ((items '(:a :b :c :d :e)))
               (get-page-items items (get-pagination :page-size 2
                                                     :page 1
                                                     :NB-ELEMENTS  (length items)))))
      (equal '(:c :d)
             (let ((items '(:a :b :c :d :e)))
               (get-page-items items (get-pagination :page-size 2
                                                     :page 2
                                                     :NB-ELEMENTS  (length items)))))
      ;; bad page number.
      (null
       (let ((items '(:a :b :c :d :e)))
         (get-page-items items (get-pagination :page-size 2
                                               :page 99
                                               :NB-ELEMENTS  (length items)))))
      (null
       (let ((items '(:a :b :c :d :e)))
         (get-page-items items (get-pagination :page-size 2
                                               :page -2
                                               :NB-ELEMENTS  (length items)))))))

(defun save (&key (file "cards.lisp"))
  "Save cards and shelves on file. Re-read with `load-cards'.
  Simply print the lisp forms."
  (when *cards*
    (with-open-file (f file
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (let ((*print-pretty* nil)
            (*print-length* nil))
        ;; Pretty printing will insert a line break at around 80 characters,
        ;; making it un-readable with uiop:read-file-form.
        (format f "~s~&" *cards*)))
    (format t "~&Cards saved on ~s.~&" file))

  (when *shelves*
    (with-open-file (f "shelves.lisp"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
      (let ((*print-pretty* nil)
            (*print-length* nil))
        ;; Pretty printing will insert a line break at around 80 characters,
        ;; making it un-readable with uiop:read-file-form.
        (format f "~s~&" *shelves*)
        (format t "~&Shelves saved on ~s.~&" "shelves.lisp")))))

(defun reload-cards (&key (file "cards.lisp") (file-shelves "shelves.lisp"))
  "Reload saved cards from file."
  (if (uiop:file-exists-p file)
      (progn
        (setf *old-cards* *cards*)
        (setf *cards* (uiop:read-file-form file)))
      (format t "~&The file ~s doesn't exist.~&" file))

  (if (uiop:file-exists-p file-shelves)
      (progn
        (setf *old-shelves* *shelves*)
        (setf *shelves* (uiop:read-file-form file-shelves)))
      (format t "~&The file ~s doesn't exist.~&" file-shelves)))

(defun help ()
  "To use in the REPL."
  (format t "~%Useful functions:~&")
  (format t "  - (reload-cards)~& ~
  - (load-init): reload the configuration~& ~
  - (load-post-init)~& ~
  - (read-selection): to get the selection from the DB or re-read it from the CSV.~&
  - (connect): when you change the DB during a session.~&
  - (load \"src/abstock.lisp\"): to reload a lisp file. HTML files are reloaded automatically.")
  (values))

(defun quit (&key (save t) (file "cards.lisp"))
  "Quit and save cards on disk, for faster restarts."
  (when save
    (save :file file))
  (uiop:quit 0))

(defun schedule-db-reload ()
  "Reload the DB regularly. By default, each night at 4am."
  (cl-cron:make-cron-job #'get-all-cards :minute 30 :hour 4)
  (cl-cron:make-cron-job #'get-all-shelves :minute 30 :hour 4)
  (log:info "Scheduled a DB reload.")
  (cl-cron:start-cron))

(defun run-app-from-shell ()
  "Start the app and ensure the web server keeps running (trick for
  the command line)."
  (handler-case
      (progn
        (start)
        (bt:join-thread
         (find-if (lambda (th)
                    (search "hunchentoot" (bt:thread-name th)))
                  (bt:all-threads))))
    #+sbcl
    (sb-sys:interactive-interrupt () (progn
                                       (format *error-output* "User abort. Bye!~&")
                                       (uiop:quit)))
    #+sbcl
    (error (c) (format *error-output* "~&An error occured: ~A~&" c))
    #-sbcl
    (error (c) (format *error-output* "~&Quitting:  ~A~&" c))))

(defun print-system-info (&optional (stream t))
  ;; see also https://github.com/40ants/cl-info
  (format stream "~&OS: ~a ~a~&" (software-type) (software-version))
  (format stream "~&Lisp: ~a ~a~&" (lisp-implementation-type) (lisp-implementation-version))
  #+asdf
  (format stream "~&ASDF: ~a~&" (asdf:asdf-version))
  #-asdf
  (format stream "NO ASDF!")
  #+quicklisp
  (format stream "~&Quicklisp: ~a~&" (ql-dist:all-dists))
  #-quicklisp
  (format stream "!! Quicklisp is not installed !!"))

(defun main ()
  "Entry point of the executable."
  (opts:define-opts
    (:name :help
           :description "print this help and exit"
           :short #\h
           :long "help")
    (:name :version
           :description "print the version and exit"
           :short #\v
           :long "version")
    (:name :verbose
           :description "print debug logs"
           :short #\V
           :long "verbose")
    (:name :pid
           :description "A filename to use to store the PID"
           :short #\p
           :arg-parser #'identity
           :long "pid"))

  (multiple-value-bind (options
                        ;; free-args
                        )
      ;; There is no error handling of malformed arguments yet.
      (opts:get-opts)

    (when (getf options :help)
      (format t *banner*)
      (opts:describe
       :prefix "ABStock usage:"
       :args "[keywords]") ;; to replace "ARG" in "--nb ARG"
      (uiop:quit))

    (when (getf options :version)
      (format t "ABStock version ~a" *version*)
      (print-system-info)
      (uiop:quit))

    (format t *banner*)

    (when (getf options :verbose)
      (print-system-info))

    (when (getf options :pid)
      (save-pid (getf options :pid)))

    (run-app-from-shell)))
