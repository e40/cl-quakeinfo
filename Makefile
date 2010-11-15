
default: all

%.fasl : %.asd
	mlisp -qq -batch -W -e '(asdf:compile-system :$*)' -kill

all: clean cl-quakeinfo.fasl test

test: FORCE
	mlisp -qq -batch -W -L test.cl -kill

clean: FORCE
	rm -f *.fasl

FORCE:
