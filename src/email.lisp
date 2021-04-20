(in-package :abstock)

;;; Send an email with SendGrid's API.

(defvar *email-config*
  '(:|sender-api-key| ""
    :|from| ""))

(defparameter *sendgrid-api* "https://api.sendgrid.com/v3/mail/send")

#|
The JSON looks like:
{
    "content": [
        {
            "type": "text/plain",
            "value": "and easy to do anywhere, even with cURL"
        }
    ],
    "from": {
        "email": "test@example.com"
    },
    "reply_to": {
        "email": "sam.smith@example.com",
        "name": "Sam Smith"
    },
    "personalizations": [
        {
            "to": [
                {
                    "email": "test@example.com",
                    "name": "John Doe"
                }
            ]
        }
    ],
    "subject": "Sending with SendGrid is Fun"
}
|#
(defun sendgrid-json (&key to from reply-to subject content)
  "Build the data json.
  `to': one email address or a list.
  `reply-to': a pair of email and name."
  (unless (or nil (consp reply-to))
    (error "\"reply-to\" must be a pair with an email and a name (strings)."))
  (setf to (alexandria:ensure-list to))
  (let ((json-alist
         `(("personalizations"
            ,(loop for dest in to
                collect `("to" (("email" . ,dest)))))
           ("from" ("email" . ,from))
           ("reply_to" ("email" . ,(car reply-to))
                       ("name" . ,(cadr reply-to)))
           ("subject" . ,subject)
           ("content" (("type" . "text/plain")
                       ("value" . ,content))))))
    (jonathan:to-json (print json-alist) :from :alist)))

;; test:
#+nil
(progn
  ;; Base case:
  (assert (string-equal (sendgrid-json :to "to@mail" :from "me@mail" :subject "hello" :content "yo" :reply-to '("@" "me"))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}"))

  ;; With two receivers:
  (assert (string-equal (sendgrid-json :to '("to@mail" "to-two@mail") :from "me@mail" :subject "hello" :content "yo" :reply-to '("@" "me"))
                        "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}],\"to\":[{\"email\":\"to-two@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"reply_to\":{\"email\":\"@\",\"name\":\"me\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}")))


(defun email-send (&key to (from (getf *email-config* :|from|)) subject content (verbose *verbose*) reply-to)
  "Send an email with SendGrid's API.
  `from': from the `*email-config*' by default.
  `reply-to': a pair with an email and a name.
  todo: make `to' accept multiple addresses."
  (assert (and to from subject content))
  (dex:post *sendgrid-api*
            :headers `(("Authorization" . ,(concatenate
                                            'string
                                            "Bearer "
                                            (getf *email-config* :|sender-api-key|)))
                       ("content-Type" . "application/json"))
            :verbose verbose
            :content (sendgrid-json :to to :from from :reply-to reply-to :subject subject :content content)))
