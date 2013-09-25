
/*
 * error.c
 *   The functions in this file represent the libpqtypes error
 *   system.  It offers the API user the ability to set/get errors.
 *   The error system uses local PGerror structure.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include <assert.h>

#include "libpqtypes-int.h"

static void
vseterror(PGerror *err, const char *format, va_list ap, int append);

void
PQseterror(PGerror *err, const char *format, ...)
{
	assert(format != NULL);
	va_list ap;
	va_start(ap, format);
	vseterror(err, format, ap, FALSE);
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
	PQseterror(args->err, "%s[pos:%d] - ", fqtn, args->typpos);

	/* append message from type handler */
	va_start(ap, format);
	vseterror(args->err, format, ap, TRUE);
	return -1;
}

static void
vseterror(PGerror *err, const char *format, va_list ap, int append)
{
	int n;
	int curlen = 0;
	int size;
	va_list ap2;
	char *msg = NULL;

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

