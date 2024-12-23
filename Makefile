# Build step-grapher

step-grapher: manifest.txt *.lisp *.asd
	buildapp --output step-grapher \
             --manifest-file ~/lisp/step-grapher/manifest.txt \
             --load-system asdf \
             --load-system sb-posix \
             --load-system alexandria \
             --load-system step-grapher\
             --entry 'step-grapher:main'

test: t/*.lisp *.lisp *.asd
	sbcl --eval "(ql:quickload :step-grapher.test)" \
		 --eval "(setf 5am::*on-error* :debug)" \
		 --eval "(5am:run-all-tests :summary :suite)" \
		 --eval "(quit)"

manifest.txt: *.asd
	sbcl --no-userinit \
         --no-sysinit \
         --non-interactive \
         --load ~/quicklisp/setup.lisp \
         --eval '(ql:quickload :alexandria)' \
		 --eval '(ql:write-asdf-manifest-file "~/lisp/step-grapher/manifest.txt")'

clean:
	rm -Rf manifest.txt  *.fasl

.PHONY: clean test
