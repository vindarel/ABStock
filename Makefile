
build:
	sbcl --load abstock.asd \
	     --eval '(ql:quickload :abstock)' \
	     --eval '(asdf:make :abstock)' \
	     --eval '(quit)'

run:
	rlwrap sbcl --load run.lisp --eval '(in-package :abstock)'
