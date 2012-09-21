#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ([2.65])
AC_INIT([motgram], [0.20120921], [ntd@gatech.edu])
AM_INIT_AUTOMAKE

AC_CONFIG_SRCDIR([include/motgram.h])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])


# Checks for programs.
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_LIBTOOL
AC_PROG_CC_C99


# Figure out the lisp

AC_ARG_VAR([COMMONLISP],[Common Lisp command, e.g. sbcl, ccl])

AC_DEFUN([CHECK_LISP],
         [AS_IF([test "x$COMMONLISP" = "x"], AC_CHECK_PROGS([COMMONLISP], [$1]))])

CHECK_LISP([sbcl])
CHECK_LISP([ccl])
CHECK_LISP([clisp])
CHECK_LISP([ecl])

# Checks for libraries.

# Checks for header files.

AC_ARG_WITH([ach], AS_HELP_STRING([--without-ach], [Disable Ach support]))
AS_IF([test "x$with_ach" != "xno"],
      [AC_CHECK_HEADERS([ach.h],
                        [AC_CHECK_LIB([ach],[ach_open],
                                      [have_ach=yes],
                                      [have_ach=no])
                        ],
                        [have_ach=no])],
      [AC_MSG_NOTICE([disabling ach])
       [have_ach=no]
      ])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

#AC_CHECK_LIB([m],[sqrt])
#AC_SEARCH_LIBS([cblas_dgemm],[blas], [true], [echo "Failed: need BLAS library" && exit 1])
#AC_SEARCH_LIBS([dgesvd_],[lapack], [true], [echo "Failed: need LAPACK library" && exit 1])
#AC_SEARCH_LIBS([clock_gettime],[rt])

#AC_CHECK_FUNCS([clock_gettime memset pow sqrt sincos])

# Check for thread local storage
AX_TLS

# Doxygen
#DX_HTML_FEATURE(ON)
#DX_PDF_FEATURE(OFF)
#DX_PS_FEATURE(OFF)
#DX_INIT_DOXYGEN(amino)

# Enable maximum warnings
AX_CFLAGS_WARN_ALL

dnl APPEND_FLAG( [FLAG] )
dnl Check if FLAG is supported, and if so, add it
AC_DEFUN([APPEND_CFLAG],
         [AX_CHECK_COMPILER_FLAGS([$1], [CFLAGS="$1 $CFLAGS"])])

AC_DEFUN([APPEND_FCFLAG],
         [AX_CHECK_COMPILER_FLAGS([$1], [FCFLAGS="$1 $FCFLAGS"])])

AC_LANG([C])
APPEND_CFLAG([-Wextra])
APPEND_CFLAG([-Wconversion])
APPEND_CFLAG([-Wpointer-arith])
APPEND_CFLAG([-Wfloat-equal])
APPEND_CFLAG([-Wshadow])
APPEND_CFLAG([-Wwrite-strings])
APPEND_CFLAG([-Wc++-compat])

AC_ARG_ENABLE([debug],
        AS_HELP_STRING([--enable-debug], [Build for debugging]))

AS_IF([test "x$enable_debug" = "xyes"],[
    AC_LANG([C])
    APPEND_CFLAG([-fstack-check])
])


AC_CONFIG_FILES([Makefile])
AC_OUTPUT

## Print summary
AC_MSG_NOTICE([])
AC_MSG_NOTICE([CONFIGURATION SUMMARY])
AC_MSG_NOTICE([=====================])
AC_MSG_NOTICE([COMMONLISP:  $COMMONLISP])
AC_MSG_NOTICE([USE ACH:     $have_ach])
