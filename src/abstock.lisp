(defpackage abstock
  (:use :cl
        :sxql)
  (:import-from :defclass-std
                :defclass/std)
  (:import-from :function-cache
                :defcached)
  (:import-from :access
                :access)
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


(defparameter *version* "0.8")          ; duplicated in .asd

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
    ((uiop:file-exists-p "config.lisp")
     "config.lisp")
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
  "List of all books.")

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

(defvar *page-length* 50
  "Page length. Not used much yet.")

;;
;; Dev helpers.
;;
;; We don't want to print 3000+ strings, it hangs the editor and the server.
(setf *print-length* 100)

(defun connect ()
  ;TODO: needs to be run inside the directory of db.db
  (if (uiop:file-exists-p *db-name*)
      (setf *connection*
            (dbi:connect :sqlite3
                         :database-name *db-name*))
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

(defun card-by-id (id)
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
  ;; also fetch-all
  (dbi:fetch (dbi:execute (dbi:prepare *connection* (yield (card-by-isbn isbn)))
                          (list isbn))))
;;
;; Get all cards data.
;;
(defun all-cards ()
  "Generates SxQL query. (yield) generates the SQL. It is not executed."
  (select ((:distinct :search_card.id)
           :search_card.created
           :search_card.title
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
           (:as :search_author.bio :author_bio)
           (:as :search_shelf.name :shelf)
           (:as :search_shelf.id :shelf_id) ;; cannot use a -
           (:as :search_publisher.name :publisher)
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
    (where (:> :search_card.quantity 0))))

(defun get-all-cards ()
  "Get all the ids of the cards in the DB."
  (if (uiop:file-exists-p *db-name*)
      (let* ((query (dbi:prepare *connection* (yield (all-cards))))
             (query (dbi:execute query (list 0))))
        ;; caution: what's long is printing all the cards.
        (log:info "(re)loading the DB")
        (setf *cards* (normalise-cards
                       (remove-duplicated-cards
                        (dbi:fetch-all query))))
        t)
      (warn "The DB file ~a does not exist. We can't load data from the DB.~&" *db-name*)))

(defun slugify-details-url (card)
  "Return a slug to identify this card, relative to the server root URL.
  Example: /livre/1-trop-bon-la-courgette"
  ;; the reverse operation is
  ;; (parse-integer (first (str:split "-" slug)))
  ;; not in a function yet.
  (format nil "/livre/~a-~a" (access card :|id|)
          (slug:slugify (access card :|title|))))

(defun normalise-cards (cards)
  "Add a repr key that joins title and author and removes accents.
  Change details_url to a short, slugified URL relative to the server root."
  (loop for card in cards
     ;; access is more generic than getf, it also works with uninterned symbols
     ;; (but we don't have such anymore).
     for ascii-title = (slug:asciify (access card :|title|))
     for ascii-author = (slug:asciify (access card :|author|))
     for ascii-publisher = (slug:asciify (access card :|publisher|))
     do (setf (getf card :|repr|)
              (str:downcase (str:concat ascii-title " " ascii-publisher)))
     do  (setf (getf card :|repr2|)
               (str:downcase (str:concat ascii-author " " ascii-title ascii-publisher)))
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
