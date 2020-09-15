LISP ?= sbcl

deps:
	git clone https://github.com/mmontone/cl-sentry-client/ ~/quicklisp/local-projects/cl-sentry-client

build:
	$(LISP) --load abstock.asd \
	     --eval '(ql:quickload :abstock)' \
	     --eval '(asdf:make :abstock)' \
	     --eval '(quit)'

run:
	rlwrap sbcl --load run.lisp --eval '(in-package :abstock)'
