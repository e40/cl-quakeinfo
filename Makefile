
default: all

%.fasl : %.asd
	mlisp -qq -batch -W -e '(asdf:compile-system :$*)' -kill

all: clean cl-quakeinfo.fasl

clean: FORCE
	rm -f *.fasl

FORCE:
