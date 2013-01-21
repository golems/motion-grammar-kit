ACLOCAL_AMFLAGS = -I m4
AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = scripts/make-core.lisp

bin_SCRIPTS = motgramlisp$(EXEEXT)

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

motgramlisp$(EXEEXT): $(top_srcdir)/scripts/make-core.lisp
	$(COMMONLISP) --load config.lisp --load $(top_srcdir)/scripts/make-core.lisp

motgramlisp.gz: motgramlisp$(EXEEXT)
	gzip -c motgramlisp$(EXEEXT) > motgramlisp.gz

lisp.name: motgramlisp$(EXEEXT) scripts/versioninfo.lisp
	./motgramlisp$(EXEEXT) --script $(top_srcdir)/scripts/versioninfo.lisp > lisp.name

dist.lisp: lisp.name motgramlisp.gz
	rsync --progress motgramlisp.gz golems@dreamhost:code.golems.org/pkg/mgk/core/motgramlisp.`cat lisp.name`.gz

doc/install.html: $(top_srcdir)/INSTALL
	mkdir -p doc
	markdown $(top_srcdir)/INSTALL | \
	  sed -e 's!\(http://\([[:alnum:]]\|[./#\-]\)*\)!<a href="\1">\1</a>!' \
	  > doc/install.html

include doxygen.am
