(in-package :abstock)

;; The bookshop's selection can come from one of two sources:
;; - selection.csv
;; - the DB.
;;
;; In the DB, cards can have a boolean field that says if they are part of the selection:
;; is_catalogue_selection.
;;
;; If there is a selection.csv at ABStock's project root, we read it instead.
;; (historical method)
;;
;; We read a ;-separated CSV file with two columns, ISBN and quantity.
;; They will be on the selection page.
;; File: selection.csv
;;
;; Main function:
;; (read-selection)
;;

(defun selection-file-exists-p (&key (file "selection.csv"))
  (uiop:file-exists-p file))

(defun read-selection-file (&key (file "selection.csv"))
  (if (selection-file-exists-p :file file)
      (loop for line in (str:lines (str:from-file file))
         for isbn = (first (str:split ";" line))
         collect isbn)
      (format t "File ~a doesn't exist.~&" file)))

(defun cards-from-isbns (isbns)
 (loop for isbn in isbns
    for position = (position isbn (get-cards)
                             :key (lambda (card)
                                    (getf card :|isbn|))
                             :test #'string-equal)
    when position
    collect  (elt *cards* position)))

(defun filter-cards-without-shelf (cards)
  ;; (remove-if-not (lambda (card) (getf card :|shelf|)) cards)
  (loop for card in cards
     for shelf = (getf card :|shelf|)
     for isbn = (getf card :|isbn|)
     if (str:blankp shelf)
     do (format t "** NOTICE **: This card has no shelf: it won't be shown in the selection page: ~a~&" isbn)
     else collect card))

(defun filter-cards-with-selection (cards)
  "Get the cards that were selection for the selection page from Abelujo.
  The Card table has a is_catalogue_selection column."
  (loop for card in cards
     if (equal 1 (access card :|is_catalogue_selection|))
     collect card))

(defun read-cards-selection ()
  (cards-from-isbns (read-selection-file)))

(defun read-selection-from-file ()
  "Read the CSV file with the ISBNs selection, exclude cards without a shelf (and print them on stdout)."
  (handler-case
      (setf *selection*
            (normalise-cards
             (filter-cards-without-shelf
              (remove-duplicated-cards
               (read-cards-selection)))))
    (error (c)
      (format *error-output* "~&Error reading the selection from ~a: ~a~&" "selection.csv" c))))

(defun get-selection-subset (&key (n 12) ensure-cover)
  (unless *selection*
    (setf *selection* (read-selection)))
  (pick-cards :n n :cards *selection* :ensure-cover ensure-cover))

(defun read-selection-from-db ()
  "Get all selected cards from the current DB in memory and do some cleanup: exclude cards without a shelf."
  (handler-case
      (setf *selection*
            (normalise-cards
             (filter-cards-without-shelf
              (filter-cards-with-selection *cards*))))
    (error (c)
      (format *error-output* "~&Error while extracting the selection from the DB: ~a~&Did you update Abelujo too?~&" c))))

(defun read-selection ()
  "Get it from the DB or read it from file."
  (setf *selection*
        (cond
          ((selection-file-exists-p)
           (log:info "~&We get the selection from file.~&")
           (read-selection-from-file))
          (t
           (log:info "~&We get the selection from the DB.~&")
           (read-selection-from-db)))))

(defun get-selection (&key (n 20) (random nil))
  "Get the selection.
  (main function)"
  (when *cards*
    (let* ((cards (if random
                      (pick-cards :n n)
                      (if *selection*
                          *selection*
                          (read-selection))))
           ;; Group by shelf.
           ;; Returns an alist: (("shelf" (card1) (card2)…) ("shelf 2" (…)))
           ;; (("BD"
           ;; (:|repr2| "liv stromquist l'origine du monderackham" :|repr|…
           (grouped (group-by:group-by cards
                                       :key (lambda (it)
                                              (getf it :|shelf|))
                                       ;; the value is the whole plist.
                                       :value #'identity))
           #+sbcl
           (sorted (sort grouped #'sb-unicode:unicode< :key #'first))
           #-sbcl
           (sorted (progn
                     (uiop:format! t "INFO: the cards selection is not sorted by title on this implementation.")
                     grouped)))
      sorted)))
