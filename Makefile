
build:
	sbcl --load abstock.asd \
	     --eval '(ql:quickload :abstock)' \
	     --eval '(asdf:make :abstock)' \
	     --eval '(quit)'
