ACLOCAL_AMFLAGS = -I m4
INCLUDES = -I$(top_srcdir)/include

EXTRA_DIST =

include_HEADERS = include/motgram.h

#TESTS = aa_test la_test

lib_LTLIBRARIES = libmotgram.la

#bin_PROGRAMS = aa_test la_test

libmotgram_la_SOURCES =                \
	libmotgram/motgram.c

distclean-local:
	-rm -rf doxygen-doc

#aa_test_SOURCES = src/test/aa_test.c
#aa_test_LDADD = libamino.la


clean-local:
	-rm -rf src/*.fas                   \
		src/*.fasl                  \
		src/*.lib

#include aminclude.am
