
default: all

%.fasl : %.asd
	mlisp -qq -batch -L asdf-setup -W \
	      -e '(asdf:compile-system :$*)' \
	      -kill

all: clean cl-quakeinfo.fasl

clean: FORCE
	rm -f *.fasl

FORCE:
