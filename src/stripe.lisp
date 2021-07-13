(in-package :abstock)

(defparameter *stripe-enabled* t
  "If non t, disable the Stripe integration.")

(defparameter *stripe-config* nil
  "Stripe configuration with keys:
- :|secret-api|
- :|publishable-api-key|")

;; testing data
#+(or)
(setf *stripe-config*
      '(:|publishable-api-key| "pk_test_JVEvtMDiifqgpjqTL34p7DL000qGyw13EH"
        :|secret-api-key| "sk_test_51FZIQmJqOLQjpdKjtl8MWDPSzc4CdZGHuIMTy4wp2clT98AL0npvUZ2ELMxkBVb3UPvzVoZgkWnCfUEBv0iKbXp900KvsGcB1r"))

;; Testing credit cards:
;; https://stripe.com/docs/testing#cards

(defparameter *stripe-api-url* "https://api.stripe.com/v1")

(defparameter +checkout-success.html+ (djula:compile-template* "checkout-success.html"))
(defparameter +checkout-cancel.html+ (djula:compile-template* "checkout-cancel.html"))

(push (cons "application" "json") drakma:*text-content-types*)  ;XXX:

(defun stripe-enabled-p ()
  (when (and *stripe-enabled*
             *stripe-config*
             (getf *stripe-config* :|publishable-api-key|)
             (getf *stripe-config* :|secret-api-key|))
    t))

(defun stripe-post (path data &rest args)
  (format t "Stripe post to: ~a ~a~%" (format nil "~a~a" *stripe-api-url* path)
          data)
  ;TODO: use dexador
  (apply #'drakma:http-request
         (format nil "~a~a" *stripe-api-url* path)
         :method :post
         :content data
         :additional-headers
         (list (cons "Authorization" (format nil "Bearer ~a" (getf *stripe-config* :|secret-api-key|))))
         args))

;; curl https://api.stripe.com/v1/checkout/sessions \
;;   -u sk_test_4eC39HqLyjWDarjtT1zdp7dc: \
;;   -d success_url="https://example.com/success" \
;;   -d cancel_url="https://example.com/cancel" \
;;   -d "payment_method_types[0]"=card \
;;   -d "line_items[0][price]"=price_H5ggYwtDq4fbrJ \
;;   -d "line_items[0][quantity]"=2 \
;;   -d mode=payment
(defun create-stripe-checkout-session (cards)
  ;TODO: use jonathan
  (json:decode-json-from-source
   (stripe-post "/checkout/sessions"
                nil
                :parameters (encode-post-parameters (serialize-stripe-session cards))
                :content-type "application/x-www-form-urlencoded")))

;; (create-stripe-checkout-session (subseq *cards* 0 3))

(easy-routes:defroute create-stripe-checkout-session-route
    ("/stripe/create-checkout-session" :method :post :decorators (easy-routes:@json))
    (ids)

  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (filter-cards-by-ids ids-list)))

    (let ((session (create-stripe-checkout-session cards)))
      (json:encode-json-to-string (list (cons :id (access session :id)))))))


(easy-routes:defroute checkout-success-route ("/checkout/success") ()
  (djula:render-template* +checkout-success.html+ nil
                          :contact *contact-infos*
                          :user-content *user-content*))

(easy-routes:defroute checkout-cancel-route ("/checkout/cancel") ()
  (djula:render-template* +checkout-cancel.html+ nil
                          :contact *contact-infos*
                          :user-content *user-content*))

(defun round-amount (amount &optional (divisor 1))
  (multiple-value-bind (quotient remainder) (truncate (/ amount divisor))
    (if (>= (abs remainder) 1/2)
        (+ quotient (truncate (signum remainder)))
        quotient)))

(defun parse-amount (string)
  "Parse an amount"
  (setf string (remove (code-char 160) string))
  (setf string (remove #\Space string))
  (setf string (substitute #\. #\, string))
  (let ((decs (or (position #\. (reverse string)) 0)))
    (round-amount (* (expt 10 2)
                     (/ (parse-integer (remove #\. string))
                        (expt 10 decs))))))
#+(or)
(assert (equal 330 (parse-amount "3.3")))

(defun decimal (number &optional (decimals 2))
  (* number (expt 10 decimals)))

(defun float-to-cents (float)
  (parse-amount (format nil (format nil "~~~a$" 2) float)))

(defun price-to-cents (price)
  (float-to-cents price))

(defun serialize-stripe-session (cards)
  `(object
    ("payment_method_types" . (array "card"))
    ("line_items" . (array ,@(loop for card in cards collect
                                   `(object
                                     ("price_data" . (object
                                                      ("currency" . "eur")
                                                      ("product_data" . (object
                                                                         ("name" . ,(getf card :|title|))
                                                                         ("images" . (array ,(getf card :|cover|)))))
                                                      ("unit_amount" . ,(price-to-cents (getf card :|price|)))))
                                     ("quantity" . 1)))))
    ("mode" . "payment")
    ("success_url" . ,(format nil "~a/checkout/success" *hostname*))
    ("cancel_url" . ,(format nil "~a/checkout/cancel" *hostname*))))

#+(or)
(serialize-stripe-session (subseq *cards* 0 3))
#| It looks like this:

(OBJECT ("payment_method_types" ARRAY "card")
 ("line_items" ARRAY
  (OBJECT
   (#1="price_data" OBJECT #2=("currency" . "eur")
    (#3="product_data" OBJECT
     (#4="name" . "Astrid Bromure Tome2 Comment Atomiser Les Fantomes")
     (#5="images" ARRAY NIL))
    (#6="unit_amount" . 1050))
   . #7=(("quantity" . 1)))
  (OBJECT
   (#1# OBJECT #2# (#3# OBJECT (#4# . "Aspirine - Tome1") (#5# ARRAY NIL))
    (#6# . 1600))
   . #7#)
  (OBJECT
   (#1# OBJECT #2#
    (#3# OBJECT (#4# . "Aspirine - Tome 3 - Monster Tinder") (#5# ARRAY NIL))
    (#6# . 1600))
   . #7#))
 ("mode" . "payment") ("success_url" . "NIL/checkout/success")
 ("cancel_url" . "NIL/checkout/cancel"))

|#

(defun encode-post-parameters (object)
  (let (parameters)
    (labels
        ((parameter-path (context)
           (let ((at-beginning t))
             (with-output-to-string (s)
               (dolist (part (reverse context))
                 (if at-beginning
                     (format s "~a" part)
                     (format s "[~a]" part))
                 (setf at-beginning nil)))))
         (encode-object-post-parameters (object context)
           (loop for key-and-value in (rest object)
                 do (%encode-post-parameters
                     (cdr key-and-value)
                     (cons (car key-and-value) context))))
         (encode-vector-post-parameters (vector context)
           (loop for i from 0
                 for x in (rest vector)
                 do (%encode-post-parameters
                     x
                     (cons i context))))
         (%encode-post-parameters (object context)
           (cond
             ((and (listp object)
                   (equalp (first object) 'object))
              (encode-object-post-parameters object context))
             ((and (listp object)
                   (equalp (first object) 'array))
              (encode-vector-post-parameters object context))
             (t (push (cons (parameter-path context) (princ-to-string object)) parameters)))))
      (%encode-post-parameters object nil)
      parameters)))

#+(or)
(encode-post-parameters (serialize-stripe-session (subseq *cards* 0 3)))

#|

(("cancel_url" . "NIL/checkout/cancel")
 ("success_url" . "NIL/checkout/success") ("mode" . "payment")
 ("line_items[2][quantity]" . "1")
 ("line_items[2][price_data][unit_amount]" . "1600")
 ("line_items[2][price_data][product_data][images][0]" . "NIL")
 ("line_items[2][price_data][product_data][name]"
  . "Aspirine - Tome 3 - Monster Tinder")
 ("line_items[2][price_data][currency]" . "eur")
 ("line_items[1][quantity]" . "1")
 ("line_items[1][price_data][unit_amount]" . "1600")
 ("line_items[1][price_data][product_data][images][0]" . "NIL")
 ("line_items[1][price_data][product_data][name]" . "Aspirine - Tome1")
 ("line_items[1][price_data][currency]" . "eur")
 ("line_items[0][quantity]" . "1")
 ("line_items[0][price_data][unit_amount]" . "1050")
 ("line_items[0][price_data][product_data][images][0]" . "NIL")
 ("line_items[0][price_data][product_data][name]"
  . "Astrid Bromure Tome2 Comment Atomiser Les Fantomes")
 ("line_items[0][price_data][currency]" . "eur")
 ("payment_method_types[0]" . "card"))
|#
