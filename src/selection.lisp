(in-package :abstock)

;; Read a ;-separated CSV file with two columns, ISBN and quantity.
;; They will be on the selection page.
;; File: selection.csv
;;
;; main function:
;; (read-selection)
;;

(defun read-selection-file (&key (file "selection.csv"))
  (if (uiop:file-exists-p file)
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

(defun read-cards-selection ()
  (cards-from-isbns (read-selection-file)))

(defun read-selection ()
  "Read the csv with the ISBNs selection, exclude cards without a shelf (and print them on stdout)."
  (handler-case
      (setf *selection*
            (filter-cards-without-shelf
             (remove-duplicated-cards
              (read-cards-selection))))
    (error (c)
      (format *error-output* "~&Error reading the selection from ~a: ~a~&" "selection.csv" c))))

(defun get-selection-subset (&key (n 12) ensure-cover)
  (unless *selection*
    (setf *selection* (read-selection)))
  (pick-cards :n n :cards *selection* :ensure-cover ensure-cover))

(defun get-selection (&key (n 20) (random nil))
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
