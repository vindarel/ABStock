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
