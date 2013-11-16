
/*
 * misc.c
 *   Type handler for CHAR, BOOL, MONEY, UUID data types.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

int
pqt_put_char(PGtypeArgs *args)
{
	*args->put.out = (PGchar) va_arg(args->ap, int);
	return 1;
}

int
pqt_get_char(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGchar *chp = va_arg(args->ap, PGchar *);
	CHKGETVALS(args, chp);
	*chp = *value;
	return 0;
}

int
pqt_put_bool(PGtypeArgs *args)
{
	*args->put.out = ((char) va_arg(args->ap, PGbool)!=0) ? 1 : 0;
	return 1;
}

int
pqt_get_bool(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGbool *boolp = va_arg(args->ap, PGbool *);

	CHKGETVALS(args, boolp);

	if (args->format == TEXTFMT)
		*boolp = (*value == 't') ? 1 : 0;
	else
		*boolp = (*value)!=0 ? 1 : 0;
	return 0;
}

int
pqt_put_money(PGtypeArgs *args)
{
	PGmoney money = va_arg(args->ap, PGmoney);
	int len = args->fmtinfo->sversion >= 80300 ? 8 : 4;

	if (len == 8)
		pqt_swap8(args->put.out, &money, 1);
	else /* truncate: pre 8.3 server expecting a 4 byte money */
		pqt_buf_putint4(args->put.out, (int) money);

	return len;
}

int
pqt_get_money(PGtypeArgs *args)
{
	DECLVALUE(args);
	DECLLENGTH(args);
	PGmoney *moneyp = va_arg(args->ap, PGmoney *);

	CHKGETVALS(args, moneyp);

	if (args->format == TEXTFMT)
	{
		char buf[64];
		char c, *p = buf;
		char *bufend = buf + sizeof(buf);

		for (; (c = *value) != '\0' && p<bufend; ++value)
		{
			if (isdigit(c) || c == '-')
			{
				*p = c;
				p++;
			}
		}

		buf[p - buf] = 0;

		if (pqt_text_to_int8(buf, moneyp) == -1)
			RERR_STR2INT(args);

		return 0;
	}

	/* 8.3 uses a 64-bit money type. Need compatibility
	 * for communicating with older servers.
	 */
	if (valuel == 4)
		pqt_buf_putint4(moneyp, pqt_buf_getint4(value));
	else
		pqt_swap8(moneyp, value, 0);

	return 0;
}

int
pqt_put_uuid(PGtypeArgs *args)
{
	PGuuid uuid = va_arg(args->ap, PGuuid);
	PUTNULLCHK(args, uuid);
	args->put.out = uuid;
	return 16;
}

int
pqt_get_uuid(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGuuid *uuid = va_arg(args->ap, PGuuid *);

	CHKGETVALS(args, uuid);

	if (args->format == TEXTFMT)
	{
		int i;
		char buf[128];
		char *p = buf;
		DECLLENGTH(args);

		/* format: a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11 */
		for (i=0; i < valuel; i++)
		{
			if (value[i] != '-')
			{
				*p++ = (pqt_hex_to_dec(value[i]) << 4) | pqt_hex_to_dec(value[i+1]);
				i++;
			}
		}

		*uuid = (char *) PQresultAlloc(args->get.result, p - buf);
		if (!*uuid)
			RERR_MEM(args);

		memcpy(*uuid, buf, p - buf);
		return 0;
	}

	*uuid = value;
	return 0;
}

int
pqt_put_null(PGtypeArgs *args)
{
	args->put.out = NULL;
	return 0;
}

