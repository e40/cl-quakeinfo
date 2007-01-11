# $Id$

ROOT_SOURCE_FILES = Makefile .asdf usgs.cl usgs.asd usgs.txt

default: all

include ../makefile.include

all: usgs.fasl

usgs.fasl: ../measures/measures.fasl ../zipcodes/zipcodes.fasl \
	   ../google/google-maps.fasl

install: FORCE

clean: FORCE
	rm -f *.fasl

FORCE:
