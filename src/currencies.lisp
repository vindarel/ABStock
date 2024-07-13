(uiop:define-package :abstock/currencies
    (:use :cl)
  (:export
   #:find-default-currency
   #:format-price
   #:display-price
   #:default-currency-symbol)
  (:documentation "Handle currencies that have different symbols and different representations.

We get a DB with prices.
We want to display the prices in the right currency in the right format.

Do we have the price already correctly formatted in the DB?
In Abelujo, no, it is done at rendering time.

Do we have the currency in the DB?
=> in Abelujo, yes. See Preferences.others => JSON string => default_currency.
=> \"euro\" or \"CFA\" or other.

We have a custom function to get it,
and a function to print a price with its currency symbol:

`display-price price', which uses the default currency.

When starting ABStock, we need to call `find-default-currency' once.

Users can specialize `find-default-currency' on its APP argument,
defaultingto `*db-app-name*' (which can be set in the config file).

Currencies differ in, at least:
- acronym (EUR, USD)
- long name (Euro, Dollar, Franc CFA)
- symbol (€, $, FCFA)
- symbol position when printing a price (3€, $3)
- locale: 1,000.00 or 1 000,00 and more
- and more and more…

For each currency object that exists in this package, have a corresponding `format-price' method.

"))

(in-package :abstock/currencies)


(defvar *default-currency* nil
  "The default currency to display the articles' price. Defaults to Euro.
  Type: currency object.
  Available currencies: see *available-currencies*")

(defclass currency ()
  ((long-name :initarg :long-name)
   (short-name :initarg :short-name)
   (symbol :initarg :symbol)))

(defparameter *euro*
     (make-instance 'currency
                    :long-name "euro"
                    :short-name "euro"
                    :symbol "€"))

;; default: euro.
(unless *default-currency*
  (setf *default-currency* *euro*))

(defun default-currency-symbol ()
  (when *default-currency*
    (slot-value *default-currency* 'symbol)))

(defparameter *fcfa*
  (make-instance 'currency
                 :long-name "Franc CFA"
                 :short-name "CFA" ;; or FCFA ?
                 ;; :alt-name "CFA" ;; useless?
                 :symbol "FCFA"))

(defparameter *usd*
  (make-instance 'currency
                 :long-name "US Dollar"
                 :short-name "USD"
                 ;; :alt-name "CFA" ;; useless?
                 :symbol "$"))

(defparameter *available-currencies*
  (list *euro* *fcfa* *usd*))

(defmethod print-object ((obj currency) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots ((symbol symbol)
                 (short-name short-name)
                 (long-name long-name))
        obj
      (format stream "~a - ~a - ~a" long-name short-name symbol))))

(defun display-price (price &key (currency *default-currency*) (stream nil))
  "Format price (float) with the given currency (take the one by default).
  Dispatch to the generic function format-price."
  (format-price price currency :stream stream))

(defgeneric format-price (price currency &key stream)
  (:documentation "Format `price' according to `currency`.
  Use `display-price' as a default, to use the default currency.
  Register the default currency with `find-currency'.

  WARN: this works only if the currency is the same object as the default currencies.
  (We need EQL equality)"))

(defmethod format-price (price currency &key (stream nil))
  (format stream "~,2f~a" price (slot-value currency 'symbol)))

(defmethod format-price (price (currency (eql *fcfa*)) &key (stream nil))
  (format stream "~a ~a" (round price)
          (slot-value currency 'symbol)))

(defmethod format-price (price (currency (eql *usd*)) &key (stream nil))
  (format stream "~a~,2f" (slot-value currency 'symbol) price))


;;; Find an existing currency object by name.
(defun find-currency (s)
  "Find a currency object by long-name, short-name, symbol.

  (find-currency \"€\") => #<CURRENCY euro - €>"
  (find s *available-currencies*
        :test (lambda (s currency)
                (with-slots (long-name short-name symbol)
                    currency
                  ;; case INsensitive.
                  (when (find s (list long-name short-name symbol)
                              :test #'equalp)
                    currency)))))

;;; Find the app's default currency.

(defun db-connect-run-query (db-name query &key (db-type :sqlite3))
  "Run this SQL query against the DB, return rows of results.

  DB-NAME: string
  QUERY: raw query.
    If built with sxql, it needs to be generated with sxql:yield.
    Example:
        (sxql:select (:foo_bar.baz) (sxql:from :foo_bar))
        (db-connect-run-query \"db.db\" (sxql:yield *))
  DB-TYPE: :sqlite3 (DBI)."
  (dbi:with-connection (conn db-type :database-name db-name)
    (dbi:fetch (dbi:execute
                (dbi:prepare conn query)))))

(defgeneric find-default-currency (app &key db-name)
  (:documentation "Find the default currency in the DB. APP: symbol to dispatch given the parent app name.
  Example: (find-default-currency :abelujo)"))

(defmethod find-default-currency (app &key db-name)
  (declare (ignorable db-name))
  (error "Could not find a method to find the currency of app ~S. ~
   Please write a method for it:

  (defmethod find-default-currency ((app (eql :your-app)) :db-name …)
    :currency)" app))

(defmethod find-default-currency ((app (eql :abstock)) &key db-name)
  (declare (ignore db-name))
  "€")

(defmethod find-default-currency ((app (eql :abelujo)) &key db-name)
  "Find the default currency registered in Abelujo's DB.

  Returns: a currency object, as created in this package."
  (let* ((query (sxql:select (:search_preferences.others)
                  (sxql:from :search_preferences)))
         (res (db-connect-run-query db-name (sxql:yield query))))
    (when res
      ;; plist:
      ;; (:|others| "{\"auto_command_after_sell\": false, \"sell_discounts\": [0.0, 5.0, 9.0], \"default_currency\": \"euro\"}")
      (ignore-errors
        (let ((json (jojo:parse (cadr res))))
          (setf *default-currency*
                (find-currency
                 (getf json :|default_currency|))))))))
