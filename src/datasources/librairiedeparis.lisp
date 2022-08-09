
(in-package :datasources/librairiedeparis)

;TODO: rename, this is not dilicom

#|
(get-summary "https://www.librairie-de-paris.fr/livre/9782749167596-beignets-de-tomates-vertes-fannie-flagg/") ; ;
|#

(defun domain-url-p (url)
  (str:starts-with-p "https://www.librairie-de-paris.fr/livre/" url))

(defparameter *base-url* "https://www.librairie-de-paris.fr/livre/{ISBN}")

(defun get-html (url)
  (handler-case
      (dex:get url)
    (error ()
      (log:error "Could not get the summary for URL ~a. Probably a 404 not found." url))))

(defun build-url (isbn)
  (str:replace-all "{ISBN}" isbn *base-url*))

(defun parse-html (html)
  (lquery:$ (initialize html)))

(defun extract-summary (node)
  "Return the summary."
  (lquery:$ node ".description" (text)))

(defun cleanup-summary-node (node)
  "Remove two buttons that contain \"lire la suite\" and \"Fermer\".
It is a destructive operation for the node."
  (lquery:$ node ".description button" (remove))
  ;; return the modified node.
  node)

(defun check-length (vector)
  "The macro some-> stops with nil elements, but a vector of length 0, #(), can be returned by lquery, and not stopping would produce an error."
  (if (plusp (length vector))
      vector))

(defun get-page-node (url)
  (arrows:some->
   url
   get-html
   parse-html
   check-length
   (elt 0)))

(defun get-summary (url)
  (unless (domain-url-p url)
    (error "we expect a URL of this domain."))
  (arrows:some->
   url
   get-page-node
   cleanup-summary-node
   extract-summary
   check-length
   (elt 0)
   ))

(defun get-summary-from-isbn (isbn)
  "Search for the summary on librairie-de-paris."
  (arrows:some->
   isbn
   build-url
   get-summary))
