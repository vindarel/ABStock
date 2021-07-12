(in-package :abstock)

(defvar *stripe-config*)

(defparameter *stripe-api-url* "https://api.stripe.com/v1")

(push (cons "application" "json") drakma:*text-content-types*)

(defun stripe-post (path data &rest args)
  (format t "Stripe post to: ~a ~a~%" (format nil "~a~a" *stripe-api-url* path)
          data)
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
  (json:decode-json-from-source
   (stripe-post "/checkout/sessions"
                nil
                :parameters (encode-post-parameters (serialize-stripe-session cards))
                :content-type "application/x-www-form-urlencoded")))

;; (create-stripe-checkout-session (subseq *cards* 0 3))

(easy-routes:defroute create-stripe-checkout-session-route ("/stripe/create-checkout-session" :method :post :decorators (easy-routes:@json))
    (ids)

  (let* ((ids-list (str:split "," ids :omit-nulls t))
         (ids-list (mapcar (lambda (it)
                             (parse-integer it))
                           ids-list))
         (cards (filter-cards-by-ids ids-list)))
    
    (let ((session (create-stripe-checkout-session cards)))
      (json:encode-json-to-string (list (cons :id (access session :id)))))))

(defparameter +checkout-success.html+ (djula:compile-template* "checkout-success.html"))

(easy-routes:defroute checkout-success-route ("/checkout/success")
    ()
  (djula:render-template* +checkout-success.html+ nil
                          :contact *contact-infos*
                          :user-content *user-content*))

(defparameter +checkout-cancel.html+ (djula:compile-template* "checkout-cancel.html"))

(easy-routes:defroute checkout-cancel-route ("/checkout/cancel")
    ()
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

;; (serialize-stripe-session (subseq *cards* 0 3))

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

;; (encode-post-parameters (serialize-stripe-session (subseq *cards* 0 3)))
