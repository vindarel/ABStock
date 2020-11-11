(in-package :abstock)

(defvar *stripe-config*)

(defparameter *stripe-api-url* "https://api.stripe.com/v1")

(defun stripe-post (path data)
  (dexador:post (format nil "~a~a" *stripe-api-url* path)
                :content data
                :headers
                (list (cons "Authorization" (format nil "Bearer ~a" (getf *stripe-config* :|api-key|))))))

(easy-routes:defroute create-stripe-checkout-session-route ("/stripe/create-checkout-session" :method :post :decorators (easy-routes:@json))
    (ids)

  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (filter-cards-by-ids ids-list)))
    
    (let ((session (stripe-post "/checkout/sessions"
                                (serialize-stripe-session cards))))
      (json:encode-json-to-string (list (cons :id (access session :id)))))))

(defun serialize-stripe-session (cards)
  (with-output-to-string (json:*json-output*)
    (json:with-object ()
      (json:encode-object-member "payment_methods_types" (list "card"))
      (json:as-object-member ("line_items")
        (json:with-array ()
          (dolist (card cards)
            (json:as-array-member ()
              (json:with-object ()
                (json:as-object-member ("price_data")
                  (json:with-object ()
                    (json:encode-object-member "currency" "usd")
                    (json:as-object-member ("product_data")
                      (json:with-object ()
                        (json:encode-object-member "name"
                                                   (getf card :|title|))
                        (json:encode-object-member "images"
                                                   (list (getf card :|cover|)))))
                    (json:encode-object-member "unit_amount"
                                               (getf card :|price|))))
                (json:encode-object-member "quantity" 1))))))
      (json:encode-object-member "mode" "payment")
      (json:encode-object-member "success_url" (format nil "~a/checkout/success" *hostname*))
      (json:encode-object-member "cancel_url" (format nil "~a/checkout/cancel" *hostname*)))))
