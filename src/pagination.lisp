
(in-package :abstock)

#|
Pagination helpers.

(get-pagination :page 2
    :page-size *page-length*
    :nb-elements (length '(:my :results)))
=>
(dict
  :PAGE 2
  :NB-ELEMENTS 2
  :PAGE-SIZE 200
  :NB-PAGES 1
  :MAX-NB-BUTTONS 1
  :TEXT-LABEL "Page 2 / 1"
 )

and give it to the template to create the pagination buttons.
|#

(defun get-nb-pages (length page-size)
  "Given a total number of elements and a page size, compute how many pages fit in there.
  (if there's a remainder, add 1 page)"
  (multiple-value-bind (nb-pages remainder)
      (floor length page-size)
    (if (plusp remainder)
        (1+ nb-pages)
        nb-pages)))
#+abstock-test
(assert (and (= 30 (get-nb-pages 6000 200))
             (= 31 (get-nb-pages 6003 200))
             (= 1 (get-nb-pages 1 200))))

(defun get-pagination (&key (page 1) (nb-elements 0) (page-size 200)
                         (max-nb-buttons 5))
  "From a current page number, a total number of elements, a page size,
  return a dict with all of that, and the total number of pages.

  Example:

(get-pagination :nb-elements 1001)
;; =>
 (dict
  :PAGE 1
  :NB-ELEMENTS 1001
  :PAGE-SIZE 200
  :NB-PAGES 6
  :TEXT-LABEL \"Page 1 / 6\"
 )
"
  (let* ((nb-pages (get-nb-pages nb-elements page-size))
         (max-nb-buttons (min nb-pages max-nb-buttons)))
    (serapeum:dict :page page
                   :nb-elements nb-elements
                   :page-size page-size
                   :nb-pages nb-pages
                   :max-nb-buttons max-nb-buttons
                   :text-label (format nil "Page ~a / ~a" page nb-pages))))
