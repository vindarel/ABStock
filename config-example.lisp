;;
;; Configuration variables, loaded at startup.
;;

;; (in-package :abstock-user)

;; Enable the triple-quotes pythonic syntax.
;; Easier to write text with simple quotes.
(pythonic-string-reader:enable-pythonic-string-syntax)

(setf *port* 8989)

(setf *api-token* "your secret token")

(setf *contact-infos*
  '(:|phone| "06 09 09 09 09"
    :|phone2| "06 09 09 77 77"
    :|email| "me@test.fr"))

;; SendGrid config:
(setf *email-config*
   '(:|sender-api-key| "api-key"
     :|from| "your@mail.com"
     :|to| "bookstore@mail.com"))

;; Simple anti-script-kiddy question for the validation form:
(setf *secret-question* "stupid question")
(setf *secret-answer* "answer")

;; Theme
;; Themes are defined in src/templates/themes/<yourtheme>/
(setf *theme* nil)

;; Content.

(setf (user-content-brand-name *user-content*)
      "ABStock")

(setf (user-content-brand-home-title *user-content*)
      "ABStock")

(setf (user-content-brand-link *user-content*)
      "")

(setf (user-content-brand-link-title *user-content*)
      "")

(setf (user-content-brand-contact-link *user-content*)
      "")

(setf (user-content-welcome-image *user-content*)
      nil)  ;; "path/to/img.png"

(setf (user-content-welcome-text *user-content*) " Welcome!")

(setf (user-content-welcome-second-text *user-content*)
      """"
      <p>
      You can contact us at:

      <ul>
      <li> 0098 7 8 9 </li>
      </ul>
      </p>
      """" )

;;;
;;; Sélection du libraire.
;;;
(setf (user-content-enable-product-selection *user-content*)
      t)

(setf (user-content-product-selection-short-name *user-content*)
      ;; mainly for the navbar button. Defaults to "Sélection du libraire".
      """"Our selection"""")

(setf (user-content-product-selection-intro-text *user-content*)
      nil)
;;
;; Shopping basket.
;;
(setf (user-content-basket-title *user-content*)
      ;; Basket page title
      "Your basket")

(setf (user-content-basket-short-name *user-content*)
      ;; button
      "Basket")

(setf (user-content-basket-text *user-content*)
      """"
          <p>
          You are nearly done. Fill in the validation form below and we'll come back to you. <br/>
          Thank you!
      """"
)

(setf (user-content-basket-show-validation-form *user-content*)
      ;; Show the validation form: already true by default.
      t)

;;; Additional headers.
(setf (user-content-additional-headers *user-content*)
      ;; We can put additional HTML in headers here, such as a Matomo code.
      """"
      """")

(setf *ignore-shelves-starting-by* '("test-" "TEST"))
