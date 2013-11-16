AM_LDFLAGS =
AUTOMAKE_OPTIONS = foreign
@SET_MAKE@

srcfiles = src/array.c src/datetime.c src/error.c \
	src/events.c src/exec.c src/geo.c src/handler.c \
	src/misc.c src/network.c src/numerics.c src/param.c \
	src/port.c src/record.c src/spec.c src/utils.c src/varlena.c \
	src/libpqtypes.h src/libpqtypes-int.h

manpages = docs/man3/PQgeterror.3 docs/man3/PQgetf.3 \
	docs/man3/PQlocalTZInfo.3 docs/man3/PQparamClear.3 \
	docs/man3/PQparamCreate.3 docs/man3/PQparamExec.3 \
	docs/man3/PQparamExecPrepared.3 docs/man3/PQparamReset.3 \
	docs/man3/PQparamSendQuery.3 docs/man3/PQparamSendQueryPrepared.3 \
	docs/man3/PQputf.3 docs/man3/PQputvf.3 \
	docs/man3/PQseterror.3 docs/man3/pqt-composites.3 docs/man3/pqt-handlers.3 \
	docs/man3/PQgetErrorField.3 docs/man3/pqt-specs.3 \
	docs/man3/PQtypesRegister.3 docs/man3/PQparamCount.3 \
	docs/man3/PQexecf.3 docs/man3/PQexecvf.3 docs/man3/PQspecPrepare.3 \
	docs/man3/PQsendf.3 docs/man3/PQsendvf.3 docs/man3/PQgetvf.3 \
	docs/man3/PQregisterComposites.3 docs/man3/PQregisterUserDefinedTypes.3 \
	docs/man3/PQregisterSubClasses.3 docs/man3/PQregisterTypes.3 \
	docs/man3/PQregisterResult.3 docs/man3/PQinitTypes.3 \
	docs/man3/PQclearTypes.3 docs/man3/PQclearSpecs.3

lib_LTLIBRARIES = libpqtypes.la
libpqtypes_la_SOURCES = $(srcfiles)
libpqtypes_la_LDFLAGS = -version-info $(LIBPQTYPES_SO_VERSION)
include_HEADERS = src/libpqtypes.h
dist_man_MANS = $(manpages)
noinst = src/libpqtypes-int.h src/getaddrinfo.h

EXTRA_DIST = install-sh src/regression-test.c src/getaddrinfo.h \
	win32.mak groff2html Makefile.win32 \
	AUTHORS ChangeLog INSTALL LICENSE README 

.PHONY: test
test:
	$(CC) $(CFLAGS) $(CPPFLAGS) -DHAVE_CONFIG_H \
		-o test src/regression-test.c $(LDFLAGS) $(LIBS) -lpq -lpqtypes
	@rm -f regression-test.o

.PHONY: docs
docs:
	@find docs/man3 -name '*.3' -type f | xargs -i ./groff2html \{\}

uninstall:
	rm -f ${prefix}/include/libpqtypes.h
	rm -f ${prefix}/lib/libpqtypes.*
	@if [ 1 ] ; then \
		_manpath=`man -w PQgetf`; \
		if test ! -z $$_manpath ; then \
			manpath=`dirname $$_manpath`; \
			for man in $(manpages); do \
				man=`basename $$man`; \
				if test -f $${manpath}/$$man ; then \
					echo rm -f $${manpath}/$$man; \
					rm -f $${manpath}/$$man; \
				fi; \
			done \
		fi; \
	fi;





