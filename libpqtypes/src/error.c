
/*
 * error.c
 *   The functions in this file represent the libpqtypes error
 *   system.  It offers the API user the ability to set/get errors.
 *   The error system uses a per-thread global error, implemented
 *   using TLS (via declspec(thread) or pthread TLS).  For
 *   non-PQT_THREAD_SAFE builds, a static buffer is used.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

/* Cannot use allocated memory for the pqterr_t members.  This is
 * because windows has no way of freeing per thread allocated
 * memory automatically, other than DllMain which won't work for
 * a third-party library such as libpqtypes.  All error messages
 * and fields are truncated to fit their buffers via pqt_strcpy().
 */
typedef struct
{
	/* Room for user message and a full libpq message */
	char msg[1024 + 8192];

	/* Error Fields (a little less than 8K for all) */
	char severity[16];
	char sqlstate[16];
	char message_primary[2048];
	char message_detail[1024];
	char message_hint[512];
	char stmt_position[16];
	char internal_position[16];
	char internal_query[2048];
	char context[2048];
	char source_file[256];
	char source_line[16];
	char source_function[80];
} pqterr_t;

/* non thread-safe, use a static */
#ifndef PQT_THREAD_SAFE
static pqterr_t lasterr = {{0}};

/* MSVC always uses __declspec(thread) */
#elif defined(PQT_MSVC)
static __declspec(thread) pqterr_t lasterr = {0};

/* Unix-variant, MinGW or Cygwin. */
#else
#include <pthread.h>
static pthread_key_t tlskey_lasterr;

/* When the thread dies, this will get called. */
static void tls_free_lasterr(void *value)
{
	if (value)
	{
		free(value);
		pthread_setspecific(tlskey_lasterr, NULL);

	}
}

/* called before main.  This attribute is available since gcc 2.7.0.
 * It replaced the obsolete _init ... destructor replaced _fini.
 * We used to use pthread_once, but solaris doesn't support this
 * in older versions (its simply a stub returning 0, ouch!).
 */
void __attribute__((constructor)) __InItErRoRkEy__(void)
{
	pthread_key_create(&tlskey_lasterr, tls_free_lasterr);
}
#endif

static void
vseterror(const char *format, va_list ap, int append);

static pqterr_t *
geterr(void)
{
	pqterr_t *err = NULL;

	/* Non thread-safe mode or windows msvc. */
#if !defined(PQT_THREAD_SAFE) || defined(PQT_MSVC)
	err = &lasterr;

	/* systems requiring pthread TLS keys */
#else

	/* System is using FSU Threads, like SCO OpenServer 5, which
	 * has minor prototype differences.
	 */
#	ifdef PTHREAD_FSU
		pthread_getspecific(tlskey_lasterr, (void **) &err);
#	else
		err = (pqterr_t *) pthread_getspecific(tlskey_lasterr);
#	endif

	if (!err)
	{
		err = (pqterr_t *) malloc(sizeof(pqterr_t));
		if (!err)
			return NULL;
		memset(err, 0, sizeof(pqterr_t));
		pthread_setspecific(tlskey_lasterr, err);
	}

#endif

	return err;
}

char *
PQgeterror(void)
{
	static char _empty[1] = {0};
	pqterr_t *err = geterr();
	return err ? err->msg : _empty;
}

void
PQseterror(const char *format, ...)
{
	/* clear error message by passing in NULL, PQseterror(NULL).*/
	if (!format)
	{
		pqterr_t *err = geterr();

		if (err)
			memset(err, 0, sizeof(pqterr_t));
	}
	else
	{
		va_list ap;
		va_start(ap, format);
		vseterror(format, ap, FALSE);
	}
}

char *
PQgetErrorField(int fieldcode)
{
	pqterr_t *err = geterr();

	if (!err)
		return NULL;

	switch (fieldcode)
	{
		case PG_DIAG_SEVERITY:
			return err->severity;

		case PG_DIAG_SQLSTATE:
			return err->sqlstate;

		case PG_DIAG_MESSAGE_PRIMARY:
			return err->message_primary;

		case PG_DIAG_MESSAGE_DETAIL:
			return err->message_detail;

		case PG_DIAG_MESSAGE_HINT:
			return err->message_hint;

		case PG_DIAG_STATEMENT_POSITION:
			return err->stmt_position;

		case PG_DIAG_INTERNAL_POSITION:
			return err->internal_position;

		case PG_DIAG_INTERNAL_QUERY:
			return err->internal_query;

		case PG_DIAG_CONTEXT:
			return err->context;

		case PG_DIAG_SOURCE_FILE:
			return err->source_file;

		case PG_DIAG_SOURCE_LINE:
			return err->source_line;

		case PG_DIAG_SOURCE_FUNCTION:
			return err->source_function;

		default:
			return NULL;
	}
}

/* Used by pqt_setresultfields */
#define geterrfield(buf, name) do{ \
	if ((value  = PQresultErrorField(res, name))) \
		pqt_strcpy(buf, sizeof(buf), value); \
	else \
		*buf = 0; \
}while (0)

void
pqt_setresultfields(const PGresult *res)
{
	char *value;
	pqterr_t *err = geterr();

	if (!err)
		return;

	geterrfield(err->severity,          PG_DIAG_SEVERITY);
	geterrfield(err->sqlstate,          PG_DIAG_SQLSTATE);
	geterrfield(err->message_primary,   PG_DIAG_MESSAGE_PRIMARY);
	geterrfield(err->message_detail,    PG_DIAG_MESSAGE_DETAIL);
	geterrfield(err->message_hint,      PG_DIAG_MESSAGE_HINT);
	geterrfield(err->stmt_position,     PG_DIAG_STATEMENT_POSITION);
	geterrfield(err->internal_position, PG_DIAG_INTERNAL_POSITION);
	geterrfield(err->internal_query,    PG_DIAG_INTERNAL_QUERY);
	geterrfield(err->context,           PG_DIAG_CONTEXT);
	geterrfield(err->source_file,       PG_DIAG_SOURCE_FILE);
	geterrfield(err->source_line,       PG_DIAG_SOURCE_LINE);
	geterrfield(err->source_function,   PG_DIAG_SOURCE_FUNCTION);
}

/* errorf() callback for PGtypeArgs, see PQputf() and PQgetf().
 * Always returns -1.
 */
int
pqt_argserrorf(PGtypeArgs *args, const char *format, ...)
{
	va_list ap;
	char fqtn[200];

	if (!args || !format || !*format)
		return -1;

	pqt_fqtn(fqtn, sizeof(fqtn), args->typhandler->typschema,
		args->typhandler->typname);

	/* put the header */
	PQseterror("%s[pos:%d] - ", fqtn, args->typpos);

	/* append message from type handler */
	va_start(ap, format);
	vseterror(format, ap, TRUE);
	return -1;
}

static void
vseterror(const char *format, va_list ap, int append)
{
	int n;
	int curlen = 0;
	int size;
	va_list ap2;
	char *msg = NULL;
	pqterr_t *err = geterr();

	if (!err)
		return;

	if (append)
		curlen = (int) strlen(err->msg);
	else
		*err->msg = 0;

	va_copy(ap2, ap);
	n = pqt_vsnprintf(err->msg + curlen, sizeof(err->msg) - curlen, format, ap2);
	va_end(ap2);

	if (n > -1)
		return;

	/* pqterr_t msg buffer is too small for the complete message.  We have
	 * use a temporary buffer to get a successful sprintf so we can
	 * pqt_strcpy() the result; which truncates to fit.
	 */
	size = (int) sizeof(err->msg) * 2;
	if (!(msg = (char *) malloc(size)))
		return;

  while (1)
	{
		char *p;

		va_copy(ap2, ap);
		n = pqt_vsnprintf(msg + curlen, size - curlen, format, ap2);
		va_end(ap2);

		/* success */
		if (n > -1)
			break;

		/* need more space */
		n = size * 2;
		if (!(p = pqt_realloc(msg, n)))
		{
			/* we're here because sprintf failed, don't trust buffer contents */
			*msg = 0;
			break;
		}

		msg = p;
		size = n;
	}

	pqt_strcpy(err->msg, sizeof(err->msg), msg);
	free(msg);
}

