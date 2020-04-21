"
Usage:

sbcl --load run.lisp

This loads the project's asd, loads the quicklisp dependencies, and
starts the web server.

Then, we are given the lisp prompt: we can interact with the running application.

Another solution to run the app is to run the executable (see README).
"

(load "abstock.asd")

(ql:quickload "abstock")

(in-package :abstock)
(handler-case
    (abstock:start :port (uiop:getenv "AB_PORT"))
  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (uiop:quit 1)))
