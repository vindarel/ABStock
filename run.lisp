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
    (progn
      (abstock:start :port (ignore-errors (parse-integer (uiop:getenv "AB_PORT")))
                     :load-db  (if (uiop:getenv "LOAD_DB")
                                   nil
                                   t))

      ;; XXX: needed for the binary, but when run from sources this deletes access to the REPL.
      (bt:join-thread (find-if (lambda (th)
                                   (search "hunchentoot" (bt:thread-name th)))
                               (bt:all-threads))))

  (error (c)
    (format *error-output* "~&An error occured: ~a~&" c)
    (unless *dev-mode*
      ;; Quit is painful in REPL, it will quit after an error on C-c C-c.
      (uiop:quit 1))
    ))
