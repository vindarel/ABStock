(in-package :abstock)

;;; Parameters to load before the rest.
;;;
;;; For example, URL names can be translated to the user's language.
;;; The parameters are read by the defroute macro at compile time with the
;;; #. "read at compile time" trick, so they must be defined in another file.

(defvar *card-page-url-name* "livre"
  "Name for the URL that links to a product card (aka a book). This name doesn't contain slashes. The final URL will be \"/livre/ID-TITLE\".")

(defparameter *card-page-url* (concatenate 'string "/" *card-page-url-name* "/:slug")
  "Full representation of the URL that links to a card page. Build on `*card-page-url-name*', adds slashes and the slug. It creates /livre/:slug, suitable to pass to easy-routes:defroute.")

(defparameter *fetch-summaries* nil
  "If t, visiting the page of a book that doesn't have a summary will look for one on an internet data source.")
