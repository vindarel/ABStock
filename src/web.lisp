(in-package :abstock)

(defparameter *server* nil)

(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))

(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +welcome.html+ (djula:compile-template* "welcome.html"))
(defparameter +cards.html+ (djula:compile-template* "cards.html"))

;; Custom Djula filter to format prices.
;; There is also the "format" filter.
(djula:def-filter :price (val)
  (format nil "~,2F" val))

#+nil
(defun render-base ()
  (with-output-to-string (s)
    (djula:render-template* +cards.html+ s
                            :cards (subseq *cards* 0 2))))

(setf *server* (make-instance 'easy-routes:routes-acceptor
                              :port 8899))

(easy-routes:defroute root ("/" :method :get) ()
  (print :hello-root)
  (djula:render-template* +welcome.html+ nil
                          :title "La Palpitante en ligne"))

(easy-routes:defroute search-route ("/search" :method :get) (q)
  (djula:render-template* +cards.html+ nil
                          :cards (subseq *cards* 0 10)))

(defun start ()
  (hunchentoot:start *server*))
