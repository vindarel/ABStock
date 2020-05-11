(in-package :abstock)

;; user-content-* prefix for class slots.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf defclass-std:*with-prefix* t))

(defclass/std user-content ()
  ;; Index, brand.
  ((brand-name
    :doc "The brand/shop name or this website's name.")
   (brand-home-title
    :doc "The title for this website. Defaults to the brand name.")
   (brand-link
    :doc "Link to the shop's website.")
   (brand-link-title
    :doc "Title for the link.")
   (brand-contact-link
    :doc "Direct link to the owner's contact page.")
   (welcome-image
    :doc "Banner image on the landing page.")
   (welcome-text
    :doc "Text on the hero of the landing page. It can be HTML.")
   (welcome-second-text
    :doc "Text on a second section of the landing page. It can be HTML.")

   ;; Headers.
   (additional-headers
    :doc "Additional header tags to include in the base template. HTML. Include CSS or JS, such as a Matomo script.")

   ;; Product selection.
   (enable-product-selection
    :std nil
    :doc "Product selection: link on the front page and own page. Cards are grouped by category (shelf).")
   (product-selection-intro-text
    :doc "Intro text (HTML) at the top of the product selection page.")

   ;; Shopping basket.
   (basket-title
    :doc "The title on the shopping basket's page.")
   (basket-text
    :doc "The text at the top of the shopping basket.")
   (basket-show-validation-form
    :std t
    :doc "Show a validation form asking for the user contant and sending an email.")))

(defvar *user-content* (make-instance 'user-content))
