(in-package :abstock/loaders)

;; Load a list of cards in a text file, in the form key: value, one by line.
;;
;; The mandatory keys are:

(defparameter +mandatory-fields+ '(:|title|))

;; Other fields:
;; - id (int)
;; - cover (url)
;; - ISBN
;; - price
;; - author(s) (single string)
;; - etc. See +known-fields/types+

(defparameter +known-fields/types+ '((:|id| integer)
                                     "title"
                                     :|cover|
                                     :|isbn|
                                     :|price|
                                     :|author|
                                     :|publisher|
                                     :|date_publication|
                                     :|date-publication|
                                     :|shelf|
                                     :|shelf_id|
                                     :|details-url|
                                     :|summary|
                                     :|quantity|
                                     :|repr|
                                     :|repr2|
                                     ))

(defvar *known-fields* nil "List computed at run time from +known-fields/types+.")

(defun compute-fields-list ()
  (setf *known-fields*
        (loop for field in +known-fields/types+
           if (consp field)
           collect (car field)
           else collect field)))

(defvar *on-unknown-key-warning* #'print-condition
  "When `read-txt-file' signals warnings for a key that is not standard, print a warning.")

(defun new-item-p (line)
  (str:starts-with-p "title:" line))

(defun line-to-key/value (line)
  "Read a line in the form key: value and return a plist.
  Accept all keys, print a warning when one is unknown."
  (unless *known-fields*
    (compute-fields-list))
  (unless (str:blankp line)
    (let* ((first-colon-pos (position ":" line :test #'string-equal))
           (key (subseq line 0 first-colon-pos))
           (val (str:trim
                 (subseq line (1+ first-colon-pos) (length line)))))
      (when (not (find key *known-fields* :test #'string-equal))
                                        ;TODO: keys as symbols or strings
        (signal 'unknown-key-warning :key key))
      ;; (eq (intern "key" "KEYWORD") :|key|) -> T
      (list (intern key "KEYWORD") val))))

(defun collect-item (lines)
  (when (new-item-p (first lines))
    (let ((item (line-to-key/value (first lines))))
      (loop for line in (rest lines)
         for key/val = (line-to-key/value line)
         until (new-item-p line)
         when key/val
         do (setf (getf item (car key/val))
                  (cadr key/val))
         finally (return item)))))

(defun load-txt-data (&key (file "cards.txt") (on-unknown-key-warning *on-unknown-key-warning*))
  "Process FILE line by line, return a list of plists.

  If we read a key that is not used by default in the application, we throw an `unknown-key-warning'. A warning is printed on standard output by default.
  You can choose how to handle these warnings, for example by ignoring them, with the argument `on-unknown-key-warning' or the global parameter `*on-unknown-key-warning' and choosing `#'ignore-condition'."
  (if (uiop:file-exists-p file)
      (handler-bind ((unknown-key-warning on-unknown-key-warning))
        (let ((lines (uiop:read-file-lines (uiop:native-namestring file))))
          (loop for line in lines
             for index from 0
             when (new-item-p line)
             collect (collect-item (subseq lines index)))))
      (uiop:format! t "~&No txt file to load data from (~a)~&" file)))
