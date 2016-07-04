#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT(libpqtypes, 1.5.1, libpqtypes@esilo.com)
AC_CANONICAL_TARGET
AC_PROG_MAKE_SET
SET_MAKE="MAKE=make --no-print-directory"
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE(libpqtypes,1.5.1)
AC_SUBST([LIBPQTYPES_SO_VERSION], [1:5:1])
AC_DISABLE_STATIC
AC_PROG_LIBTOOL
AC_PROG_INSTALL
AC_CONFIG_SRCDIR([src/param.c])
AM_CONFIG_HEADER([src/pqt_config.h:src/pqt_config.h.in])
AC_PROG_CXX
AC_PROG_CC

CPPFLAGS="$CPPFLAGS -Isrc"
if test "$CC" = "gcc" ; then
	# always want reentrant funcs, not just thread-safe mode. -s
	CFLAGS="$CFLAGS -O3 -Wall -Wpointer-arith -D_GNU_SOURCE -D_REENTRANT -fsigned-char"

	# gcc doesn't indicate an error for unknown options when they are
	# not warning/feature options, -Wxxx or -fxxx.  We have to test manually.
	cprog="int main(void){return 0;}"
	gcctest="gcc -x c -c - -o /dev/null"

	AC_MSG_CHECKING(CFLAGS for gcc -std=gnu99...)
	result=`echo "$cprog" | $gcctest -std=gnu99 2>&1`
	if test -z "$result" ; then
		CFLAGS="$CFLAGS -std=gnu99"
		AC_MSG_RESULT(-std=gnu99)
	else
		AC_MSG_RESULT([no, unknown])
	fi

	# turn on as many warnings as possible
	AX_CFLAGS_GCC_OPTION([-Wclobbered])
	AX_CFLAGS_GCC_OPTION([-Wempty-body])
	AX_CFLAGS_GCC_OPTION([-Wignored-qualifiers])
	#AX_CFLAGS_GCC_OPTION([-Wmissing-field-initializers]) # annoying
	AX_CFLAGS_GCC_OPTION([-Wmissing-parameter-type])
	AX_CFLAGS_GCC_OPTION([-Wold-style-declaration])
	AX_CFLAGS_GCC_OPTION([-Woverride-init])
	AX_CFLAGS_GCC_OPTION([-Wsign-compare])
	AX_CFLAGS_GCC_OPTION([-Wtype-limits])
	AX_CFLAGS_GCC_OPTION([-Wuninitialized])
	AX_CFLAGS_GCC_OPTION([-fomit-frame-pointer])
	AX_CFLAGS_GCC_OPTION([-fno-strict-aliasing])
	AX_CFLAGS_GCC_OPTION([-funroll-all-loops])
	AX_CFLAGS_GCC_OPTION([-funit-at-a-time])
else
	AC_MSG_ERROR([Using $CC instead of gcc, currently not supported])
fi

# Each case has an example of what target_os contains
case ${target_os} in
	# solaris
	solaris*)
		CFLAGS="$CFLAGS -D_STDC_C99"
	;;

	# SCO UnixWare
	*UnixWare*|*unixware*)
		CFLAGS="$CFLAGS -D__UnixWare__"
	;;

	# SCO OpenServer
	sco*)
		CFLAGS="$CFLAGS -D__OpenServer__"
	;;
esac

AC_ARG_ENABLE(thread-safety, [  --enable-thread-safety  Enable threads],
	[have_threads=yes], [have_threads=no])

if test "$have_threads" = "yes" ; then
	AC_SEARCH_LIBS(pthread_create, [pthread thread],,
		[AC_MSG_ERROR([Missing libpthread.so or libthread.so, cannot use --enable-thread-safety])])
	CFLAGS="$CFLAGS -DPQT_THREAD_SAFE -D_THREAD_SAFE"

	# SCO OpenServer 5, possibly other platforms, requires using FSU pthreads.
	# We detect this by looking at the prototype for pthread_getspecific.
	# There are a few other prototypes that are different as well but detecting
	# one is plenty.
	AC_MSG_CHECKING(whether FSU pthreads is being used)
	AC_TRY_COMPILE(
	[#include <pthread.h>],
	[pthread_getspecific((pthread_key_t)(0), (void *)(0))],
	[AC_DEFINE(PTHREAD_FSU, 1,
	  [Define to 1 if using FSU pthreads]) AC_MSG_RESULT(yes)],
	[AC_MSG_RESULT(no)]
	)
fi

# If strict memory alignment is required, this test will produce a
# Bus error.  Most riscs CPUs (old and modern) are strict.
save_CFLAGS=$CFLAGS
CFLAGS=""
AC_MSG_CHECKING(if strict memory alignment is required)
AC_RUN_IFELSE([int main(void){char b[[8]];int*i=(int*)(b+1);*i=0;return 0;}],
[AC_MSG_RESULT(no)], [AC_DEFINE(STRICT_MEMORY_ALIGNMENT, 1,
  [Define to 1 if architecture requires strict memory alignment]) AC_MSG_RESULT(yes)]
)
CFLAGS=$save_CFLAGS

AC_HEADER_STDC
AC_CHECK_HEADERS([ \
arpa/inet.h limits.h netdb.h netinet/in.h stddef.h \
sys/socket.h time.h math.h sys/time.h strings.h])

AC_STRUCT_TIMEZONE
AC_CHECK_MEMBERS([struct tm.tm_gmtoff],,, \
[#ifdef HAVE_TIME_H
#include <time.h>
#endif
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif])

AC_CHECK_TYPES(socklen_t,,,
[#include <unistd.h>
#ifdef HAVE_APRA_INET_H
#include <arpa/inet.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif])

AC_CHECK_MEMBERS([struct sockaddr_storage.ss_len],,, \
[#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

AC_C_CONST
AC_FUNC_MEMCMP
AC_FUNC_STRTOD
AC_HEADER_TIME
AC_TYPE_SIZE_T

AC_SEARCH_LIBS(pow, m)
AC_SEARCH_LIBS(getaddrinfo, [nsl socket], \
	[AC_DEFINE([HAVE_GETADDRINFO], [], \
	[Define if getaddrinfo exists])], [], [])

AC_CHECK_FUNCS([vsnprintf floor ceil rint hstrerror \
localtime_r strtol strtoll strtoul])

AC_OUTPUT(Makefile)



