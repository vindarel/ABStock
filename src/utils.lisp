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

(defun pick-cards (&key (n 20))
  "Pick `n' cards randomly. 20 by default."
  (loop repeat n
     with l = (length *cards*)
     collect (elt *cards* (random l))))

(defun get-selection (&key (n 20))
  (let* ((cards (pick-cards :n n))
         ;; Group by shelf.
         ;; Returns an alist: (("shelf" (card1) (card2)…) ("shelf 2" (…)))
         ;; (("BD"
         ;; (:|repr2| "liv stromquist l'origine du monderackham" :|repr|
         ;; "l'origine du monde rackham" :|id| 2645 :|title| "L'origine du monde"
         ;; :|price| 20.0d0 :|isbn| "9782878271973" :|cover|
         (grouped (group-by:group-by cards
                                     :key (lambda (it)
                                            (getf it :|shelf|))
                                     ;; the value is the whole plist.
                                     :value #'identity))
         (sorted (sort grouped  #'sb-unicode:unicode< :key #'first)))
    sorted))
