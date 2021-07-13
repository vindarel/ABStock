;;
;; Configuration variables, loaded at startup.
;;

;; (in-package :abstock-user)

;; Enable the triple-quotes pythonic syntax.
;; Easier to write text with simple quotes.
(pythonic-string-reader:enable-pythonic-string-syntax)

(setf *port* 8989)

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
      "My Shop")

(setf (user-content-brand-home-title *user-content*)
      "My Shop Online")

(setf (user-content-brand-link *user-content*)
      "http://foo.fr")

(setf (user-content-brand-link-title *user-content*)
      "My Shop.fr")

(setf (user-content-brand-contact-link *user-content*)
      "http://path/to/contact.html")

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
      nil)

(setf (user-content-product-selection-intro-text *user-content*)
      nil)
;;
;; Shopping basket.
;;
(setf (user-content-basket-title *user-content*)
      "Your basket")

(setf (user-content-basket-text *user-content*)
      """"
          <p>
          You are nearly don. Fill in the validation form below and we'll come back to you. <br/>
          Thank you!
      """"
)

(setf (user-content-basket-show-validation-form *user-content*)
      ;; Show the validation form: already true by default.
      t)

;;; Additional headers.
(setf (user-content-additional-headers *user-content*)
      """"
      HTML.
      you can put here Matomo code.
      """")

(setf *ignore-shelves-starting-by* '("test-" "TEST"))

;;;
;;; Stripe
;;;

;; enable/disable Stripe integration.
(setf *stripe-enabled* t)

;; Stripe settings
(setf *stripe-config*
      '(:|publishable-api-key| "pk_test_JVEvtMDiifqgpjqTL34p7DL000qGyw13EH"
        ;; do NOT publish the secret key (other than a test one).
        :|secret-api-key| "sk_test_51FZIQmJqOLQjpdKjtl8MWDPSzc4CdZGHuIMTy4wp2clT98AL0npvUZ2ELMxkBVb3UPvzVoZgkWnCfUEBv0iKbXp900KvsGcB1r"))
