(in-package :abstock)

(defparameter *isbn-scanner* (ppcre:create-scanner "^[0-9]+$"))

(defun isbn-p (str)
  "`str' contains digits. Hyphens are stripped off."
  (ppcre:scan *isbn-scanner* (str:replace-all "-" "" str)))

#+nil
(progn
  (assert (isbn-p "978-207-2862-106"))
  (assert (not (isbn-p "rst9782072862106"))))

(defun split-query (query)
  "Split this query (string) by spaces and commas."
  (remove-if #'str:blankp
             (ppcre:split "[#\ ,\,]" query)))

#+nil
(progn
  (assert (= 2
             (length
              (split-query "9782072862106, ,9782021418019")))))

(defun collect-isbns (list)
  (loop for elt in list
     when (isbn-p elt)
     collect elt into isbns
     finally (return isbns)))

(defun filter-cards-by-ids (ids-list)
  (loop for id in ids-list
     for position = (position id (get-cards) :key (lambda (card)
                                                    (getf card :|id|)))
     when position
     collect  (elt *cards* position)))

(defun filter-cards-by-author (author &key exclude-id)
  (remove-if-not (lambda (card)
                   (unless (or (equal (getf card :|id|)
                                      exclude-id)
                               (string= (str:downcase author)
                                        "collectif"))
                     (string= (str:downcase author)
                              (str:downcase (getf card :|author|)))))
                 (get-cards)))


(defun filter-cards-by-shelf-id (shelf-id &key exclude-id)
  (remove-if-not (lambda (card)
                   ;; use equal, = errors out with nil.
                   (unless (equal (getf card :|id|)
                                  exclude-id)
                     (equal shelf-id
                            (getf card :|shelf_id|))))
                 (get-cards)))

(defun replace-pairs (pairs str)
  "Replace all associations in pairs (plist) and return a new string.

  Example:
  (replace-pairs (list \"{{phone}}\" \"987\") \"call {{phone}}\")
  =>
  \"call 987\""
  (assert (consp pairs))
  (dotimes (i (- (length pairs)
                 1))
    (setf str (str:replace-all (nth i pairs) (nth (incf i) pairs) str)))
  str)

#+nil
(progn
  (assert (string-equal
           "call 987"
           (replace-pairs (list "{{phone}}" "987") "call {{phone}}")))
  (assert (string-equal
           "Hi Alice call 987"
           (replace-pairs (list "{{phone}}" "987"
                                "{{name}}" "Alice")
                          "Hi {{name}} call {{phone}}"))))


(defun pick-cards (&key (n 20) (cards (get-cards)) ensure-cover shelf-id exclude-id)
  "Pick `n' cards randomly. 20 by default."
  (when (stringp shelf-id)
    (setf shelf-id (ignore-errors
                     (parse-integer shelf-id))))
  (when cards
    ;; Defensive: on startup, reading the DB, the variable can be nil.
    (loop with collected-ids = '()
       with res = '()
       with l = (length cards)
       for attempts from 0
       for i = (random l)
       for card = (elt cards i)
       for card-id = (getf card :|id|)
       for card-shelf-id = (getf card :|shelf_id|)
       when (and (not (find i collected-ids))
                 ;; "temporary": some cards have no cover yet, until we re-run
                 ;; a script on Abelujo side.
                 (if ensure-cover
                     (getf card :|cover|)
                     t)
                 (if (and shelf-id
                          card-shelf-id)
                     (and
                      (equal card-shelf-id
                             shelf-id)
                      (not (equal card-id exclude-id)))

                     t))
       do (progn (push i collected-ids)
                 (push card res))
       while (and (< (length res)
                     n)
                  (< attempts 1000))
       finally (return res))))

(defun remove-duplicated-cards (cards)
  (remove-duplicates cards :key (lambda (card)
                                  (getf card :|isbn|))))
