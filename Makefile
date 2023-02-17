LISP ?= sbcl
LISP_FLAGS ?= --non-interactive
USERINIT ?= /home/vince/.sbclrc  # for Systemd. Need to parametrize it.

deps:
	git clone https://github.com/mmontone/cl-sentry-client/ ~/quicklisp/local-projects/cl-sentry-client

build:
	$(LISP) $(LISP_FLAGS) --load abstock.asd \
	     --eval '(ql:quickload :abstock)' \
	     --eval '(asdf:make :abstock)' \
	     --eval '(quit)'

# for Systemd
rund:
	rlwrap $(LISP) $(LISP_FLAGS) --userinit $(USERINIT) --load run.lisp --eval '(in-package :abstock)'

# from sources
run:
	rlwrap $(LISP) $(LISP_FLAGS) --userinit $(USERINIT) --load run.lisp --eval '(in-package :abstock)'
