# Build step-grapher

# Build ttf-utils

ttf-utils: *.lisp *.asd Makefile
	buildapp --output ttf-utils \
             --load ~/quicklisp/setup.lisp \
             --eval '(ql:quickload :step-grapher)' \
             --entry 'ttf-utils:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :step-grapher.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
