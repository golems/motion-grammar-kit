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

AC_CHECK_PROGS([SBCL], [sbcl])
AC_CHECK_PROGS([CLISP], [clisp])
AC_CHECK_PROGS([ECL], [ecl])
AC_CHECK_PROGS([CCL], [ccl])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([ach.h])

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
