
/*
 * numeric.c
 *   Type handler for the numeric data types.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"
#include <errno.h>

/* Extremely annoying warning "conversion from 'int ' to 'short'" on
 * Visual Studio 6.  Normally this warning is useful but not in this
 * case.  Disable it.
 */
#if defined(PQT_MSVC) && PQT_MSVC <= 1200
#	pragma warning (disable : 4244)
#endif

int
pqt_put_int2(PGtypeArgs *args)
{
	PGint2 *i2p = va_arg(args->ap, PGint2 *);
	PUTNULLCHK(args, i2p);
	pqt_buf_putint2(args->put.out, *i2p);
	return 2;
}

int
pqt_get_int2(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGint2 *i2p = va_arg(args->ap, PGint2 *);

	CHKGETVALS(args, i2p);

	if (args->format == TEXTFMT)
	{
		int n;

		errno = 0;
		if ((n = (int) strtol(value, NULL, 10)) == 0 && errno)
			RERR_STR2INT(args);

		*i2p = (PGint2)n;
		return 0;
	}

	*i2p = pqt_buf_getint2(value);
	return 0;
}

/* handles oid as well */
int
pqt_put_int4(PGtypeArgs *args)
{
	PGint4 *i4p = va_arg(args->ap, PGint4 *);
	PUTNULLCHK(args, i4p);
	*(PGint4 *)args->put.out = (PGint4) htonl(*i4p);
	return 4;
}

/* handles oid as well */
int
pqt_get_int4(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGint4 *i4p = va_arg(args->ap, PGint4 *);

	CHKGETVALS(args, i4p);

	if (args->format == TEXTFMT)
	{
		PGint4 n;

		/* Use strtoul so this can support Oid */
		if ((n = (PGint4) strtoul(value, NULL, 10)) == 0 && errno)
			RERR_STR2INT(args);

		*i4p = n;
		return 0;
	}

	*i4p = (PGint4) pqt_buf_getint4(value);
	return 0;
}

int
pqt_put_int8(PGtypeArgs *args)
{
	PGint8 *i8p = va_arg(args->ap, PGint8 *);
	PUTNULLCHK(args, i8p);
	pqt_swap8(args->put.out, i8p, 1);
	return 8;
}

int
pqt_get_int8(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGint8 *i8p = va_arg(args->ap, PGint8 *);

	CHKGETVALS(args, i8p);

	if (args->format == TEXTFMT)
	{
		if (pqt_text_to_int8(value, i8p) == -1)
			RERR_STR2INT(args);
		return 0;
	}

	pqt_swap8(i8p, value, 0);
	return 0;
}

int
pqt_put_float4(PGtypeArgs *args)
{
	PGfloat4 *f4p = va_arg(args->ap, PGfloat4 *);
	PUTNULLCHK(args, f4p);
	void *vp = (void *)f4p;
	pqt_buf_putint4(args->put.out, *(int *) vp);
	return 4;
}

int
pqt_get_float4(PGtypeArgs *args)
{
	DECLVALUE(args);
	int *f4p = (int *) va_arg(args->ap, PGfloat4 *);

	CHKGETVALS(args, f4p);

	if (args->format == TEXTFMT)
	{
		PGfloat4 f;

		errno = 0;
		if ((f = (PGfloat4) strtod(value, NULL)) == 0 && errno)
			RERR_STR2INT(args);

		*(PGfloat4 *) f4p = f;
		return 0;
	}

	*f4p = pqt_buf_getint4(value);
	return 0;
}

int
pqt_put_float8(PGtypeArgs *args)
{
	PGfloat8 *f8p = va_arg(args->ap, PGfloat8 *);
	PUTNULLCHK(args, f8p);
	pqt_swap8(args->put.out, f8p, 1);
	return 8;
}

int
pqt_get_float8(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGfloat8 *f8p = va_arg(args->ap, PGfloat8 *);

	CHKGETVALS(args, f8p);

	if (args->format == TEXTFMT)
	{
		if (!pqt_text_to_float8(f8p, value, NULL))
			RERR_STR2INT(args);

		return 0;
	}

	pqt_swap8(f8p, value, 0);
	return 0;
}

int
pqt_put_numeric(PGtypeArgs *args)
{
	NumericVar *num = va_arg(args->ap, NumericVar *);
	PUTNULLCHK(args, num);

	/* variable length data type, grow args->put.out buffer if needed */
	int numlen = (int) sizeof(short) * (4 + num->ndigits);
	if (args->put.expandBuffer(args, numlen) == -1)
		return -1;

	short *out = (short *) args->put.out;
	*out++ = htons(num->ndigits);
	*out++ = htons(num->weight);
	*out++ = htons(num->sign);
	*out++ = htons(num->dscale);

	for (int i = 0; i < num->ndigits; ++i)
		*out++ = num->digits[i];

	return numlen;
}

int
pqt_get_numeric(PGtypeArgs *args)
{
	DECLVALUE(args);
	NumericVar *num = va_arg(args->ap, NumericVar *);

	CHKGETVALS(args, num);

	if (args->format == TEXTFMT)
		return args->errorf(args, "text format is not supported");

	short *s = (short *) value;
	num->ndigits = ntohs(*s); s++;
	num->weight  = ntohs(*s); s++;
	num->sign    = ntohs(*s); s++;
	num->dscale  = ntohs(*s); s++;
	num->digits  = s;

	return 0;
}
