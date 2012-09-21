ACLOCAL_AMFLAGS = -I m4
INCLUDES = -I$(top_srcdir)/include

EXTRA_DIST =

bin_PROGRAMS = motgramlisp

include_HEADERS = include/motgram.h

#TESTS =

lib_LTLIBRARIES = libmotgram.la

noinst_PROGRAMS = mg_test

libmotgram_la_SOURCES =                \
	libmotgram/motgram.c

libmotgram_la_LIBADD = -lach

distclean-local:
	-rm -rf doxygen-doc

mg_test_SOURCES = libmotgram/mg_test.c
mg_test_LDADD = libmotgram.la -lach


clean-local:
	-rm -rf src/*.fas                   \
		src/*.fasl                  \
		src/*.lib                   \
		motgramlisp*

motgramlisp: $(top_srcdir)/scripts/make-core.lisp
	$(COMMONLISP) --load scripts/make-core.lisp

motgramlisp.gz: motgramlisp
	gzip -c motgramlisp > motgramlisp.gz

lisp.name: motgramlisp scripts/versioninfo.lisp
	./motgramlisp --script $(top_srcdir)/scripts/versioninfo.lisp > lisp.name

dist.lisp: lisp.name motgramlisp.gz
	rsync --progress motgramlisp.gz dreamhost:code.golems.org/pkg/mgk/core/motgramlisp.`cat lisp.name`.gz

#include aminclude.am
