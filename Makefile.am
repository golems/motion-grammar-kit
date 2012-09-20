ACLOCAL_AMFLAGS = -I m4
INCLUDES = -I$(top_srcdir)/include

EXTRA_DIST =

include_HEADERS = include/motgram.h

#TESTS = aa_test la_test

lib_LTLIBRARIES = libmotgram.la

noinst_PROGRAMS = mg_test

libmotgram_la_SOURCES =                \
	libmotgram/motgram.c

distclean-local:
	-rm -rf doxygen-doc

mg_test_SOURCES = libmotgram/mg_test.c
mg_test_LDADD = libmotgram.la -lach


clean-local:
	-rm -rf src/*.fas                   \
		src/*.fasl                  \
		src/*.lib                   \
		*.mgkcore-*

lispcore: scripts/make-core.lisp
	sbcl --load scripts/make-core.lisp --eval '(sb-ext:quit)'

#include aminclude.am
