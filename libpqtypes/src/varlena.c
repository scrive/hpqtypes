
/*
 * varlena.c
 *   Type handler for the variable length data types.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

/* put a type in its string representation, text format */
int
pqt_put_str(PGtypeArgs *args)
{
	args->format = TEXTFMT;
	args->put.out = va_arg(args->ap, char *);
	return args->put.out ? (int)strlen(args->put.out)+1 : 0;
}

/* varchar, bpchar and name use the text handlers */
int
pqt_put_text(PGtypeArgs *args)
{
	args->put.out = va_arg(args->ap, PGtext);
	return args->put.out ? (int)strlen(args->put.out) : 0;
}

/* varchar, bpchar and name use the text handlers */
int
pqt_get_text(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGtext *textp = va_arg(args->ap, PGtext *);
	CHKGETVALS(args, textp);
	*textp = value;
	return 0;
}

int
pqt_put_bytea(PGtypeArgs *args)
{
	PGbytea *bytea = va_arg(args->ap, PGbytea *);
	PUTNULLCHK(args, bytea);
	args->put.out = bytea->data;
	return bytea->len;
}

int
pqt_get_bytea(PGtypeArgs *args)
{
	DECLVALUE(args);
	DECLLENGTH(args);
	PGbytea *bytea = va_arg(args->ap, PGbytea *);

	CHKGETVALS(args, bytea);

	if (args->format == TEXTFMT)
	{
		size_t len = 0;

		value = (char *) PQunescapeBytea((const unsigned char *) value, &len);
		if (!value)
			RERR_STR2INT(args);

		bytea->data = (char *) PQresultAlloc(args->get.result, len);
		if (!bytea->data)
		{
			PQfreemem(value);
			RERR_MEM(args);
		}

		bytea->len = (int) len;
		memcpy(bytea->data, value, len);
		PQfreemem(value);
		return 0;
  }

	/* binary format */
  bytea->len = valuel;
  bytea->data = value;
	return 0;
}



