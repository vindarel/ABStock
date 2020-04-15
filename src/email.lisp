(in-package :abstock)

;;; Send an email with SendGrid's API.

(defparameter *email-config*
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
    "personalizations": [
        {
            "to": [
                {
                    "email": "test@example.com"
                }
            ]
        }
    ],
    "subject": "Sending with SendGrid is Fun"
}
|#
(defun sendgrid-json (&key to from subject content)
  (let ((json-alist `(("personalizations"
                       (("to" (("email" . ,to)))))
                      ("from" ("email" . ,from))
                      ("subject" . ,subject)
                      ("content" (("type" . "text/plain")
                                  ("value" . ,content))))))
    (jonathan:to-json json-alist :from :alist)))

#+nil
(assert (string-equal (sendgrid-json :to "to@mail" :from "me@mail" :subject "hello" :content "yo")
                      "{\"personalizations\":[{\"to\":[{\"email\":\"to@mail\"}]}],\"from\":{\"email\":\"me@mail\"},\"subject\":\"hello\",\"content\":[{\"type\":\"text/plain\",\"value\":\"yo\"}]}"))


(defun email-send (&key to (from (getf *email-config* :|from|)) subject content (verbose *verbose*))
  "Send an email with SendGrid's API.
  `from': from the `*email-config*' by default.
  todo: make `to' accept multiple addresses."
  (assert (and to from subject content))
  (dex:post *sendgrid-api*
            :headers `(("Authorization" . ,(concatenate
                                            'string
                                            "Bearer "
                                            (getf *email-config* :|sender-api-key|)))
                       ("content-Type" . "application/json"))
            :verbose verbose
            :content (sendgrid-json :to to :from from :subject subject :content content)))
