(in-package :abstock)

(djula:add-template-directory
 (asdf:system-relative-pathname "abstock" "src/templates/"))

(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +cards.html+ (djula:compile-template* "cards.html"))

(defun render-base ()
  (with-output-to-string (s)
    (djula:render-template* +cards.html+ s
                            :cards (subseq *cards* 0 2))))
