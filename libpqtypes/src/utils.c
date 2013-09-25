
/*
 * util.c
 *   Utility functions.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

PGresult *
pqt_copyresult(PGtypeArgs *args, int nattrs)
{
	int i;
	PGresult *res;
	int tableid, columnid, format;
	PGresAttDesc *ad = (PGresAttDesc *) malloc(nattrs * sizeof(PGresAttDesc));

	if (!ad)
	{
		PQseterror(args->err, PQT_OUTOFMEMORY);
		return NULL;
	}

	tableid  = PQftable(args->get.result, args->get.field_num);
	columnid = PQftablecol(args->get.result, args->get.field_num);
	format   = PQfformat(args->get.result, args->get.field_num);

	for (i=0; i < nattrs; i++)
	{
		ad[i].tableid  = tableid;
		ad[i].columnid = columnid;
		ad[i].format   = format;

		/* simple array */
		if (args->typhandler->nattrs == 0)
		{
			ad[i].typid     = args->typhandler->typoid;
			ad[i].typlen    = args->typhandler->typlen;
			ad[i].name      = NULL;
			ad[i].atttypmod = -1;
		}
		/* composite/record */
		else
		{
			ad[i].typid     = args->typhandler->attDescs[i].attoid;
			ad[i].typlen    = args->typhandler->attDescs[i].attlen;
			ad[i].name      = args->typhandler->attDescs[i].attname;
			ad[i].atttypmod = args->typhandler->attDescs[i].atttypmod;
		}
	}

	res = PQcopyResult(args->get.result,
		PG_COPYRES_EVENTS | PG_COPYRES_NOTICEHOOKS);

	if (!res)
	{
		free(ad);
		PQseterror(args->err, PQT_OUTOFMEMORY);
		return NULL;
	}

	if (!PQsetResultAttrs(res, nattrs, ad))
	{
		PQclear(res);
		PQseterror(args->err, PQT_OUTOFMEMORY);
		res = NULL;
	}

	free(ad);
	return res;
}

#ifdef STRICT_MEMORY_ALIGNMENT
short
pqt_buf_getint2(char *buffer)
{
	short n;
	memcpy(&n, buffer, 2);
	return (short) ntohs(n);
}

int
pqt_buf_getint4(char *buffer)
{
	int n;
	memcpy(&n, buffer, 4);
	return (int) ntohl(n);
}
#endif

void
pqt_swap8(void *outp, void *inp, int tonet)
{
	static int n = 1;

#ifdef STRICT_MEMORY_ALIGNMENT
	unsigned int in[2];
	unsigned int out[2];
	memcpy(&in, inp, 8);
#else
	unsigned int *in = (unsigned int *) inp;
	unsigned int *out = (unsigned int *) outp;
#endif

	/* swap when needed */
	if (*(char *)&n == 1)
	{
		out[0] = (unsigned int) (tonet ? htonl(in[1]) : ntohl(in[1]));
		out[1] = (unsigned int) (tonet ? htonl(in[0]) : ntohl(in[0]));
	}
	else
	{
		out[0] = in[0];
		out[1] = in[1];
	}

#ifdef STRICT_MEMORY_ALIGNMENT
	memcpy(outp, out, 8);
#endif
}

int
pqt_text_to_int8(char *val, void *out)
{
	PGint8 n;

	/* NOTE: port version of strtoll in port.c. */
	errno = 0;
	if ((n = (PGint8) strtoll(val, NULL, 10)) == 0 && errno)
		return -1;

	*(PGint8 *) out = n;
	return 0;
}

int
pqt_text_to_float8(double *f8, char *text, char **endptr)
{
	double d;

	errno = 0;
	if ((d = strtod(text, endptr)) == 0 && errno)
		return 0;

	*f8 = d;
	return 1;
}

/* Checks buffer and truncates 'src' if 'dest' is too small. */
char *
pqt_strcpy(char *dest, size_t size, const char *src)
{
	size_t src_len = strlen(src);

	/* truncate if needed */
	if (src_len >= size)
		src_len = size - 1;

	memcpy(dest, src, src_len);
	dest[src_len] = 0;
	return dest;
}


/* ---------------------------
 * Everything below was taken from postgresql project
 * A couple of changes here and there.
 */

#ifndef HIGHBIT
#	define HIGHBIT (0x80)
#endif

#ifndef IS_HIGHBIT_SET
#	define IS_HIGHBIT_SET(ch) ((unsigned char)(ch) & HIGHBIT)
#endif

/*
 * Fold a character to lower case.
 *
 * Unlike some versions of tolower(), this is safe to apply to characters
 * that aren't upper case letters.  Note however that the whole thing is
 * a bit bogus for multibyte character sets.
 */
unsigned char
pqt_tolower(unsigned char ch)
{
	if (ch >= 'A' && ch <= 'Z')
		ch += 'a' - 'A';
	else if (IS_HIGHBIT_SET(ch) && isupper(ch))
		ch = (unsigned char) tolower(ch);
	return ch;
}

/*
 * Case-independent comparison of two null-terminated strings.
 */
int
pqt_strcasecmp(const char *s1, const char *s2)
{
	for (;;)
	{
		unsigned char ch1 = (unsigned char) *s1++;
		unsigned char ch2 = (unsigned char) *s2++;

		if (ch1 != ch2)
		{
			if (ch1 >= 'A' && ch1 <= 'Z')
				ch1 += 'a' - 'A';
			else if (IS_HIGHBIT_SET(ch1) && isupper(ch1))
				ch1 = (unsigned char)tolower(ch1);

			if (ch2 >= 'A' && ch2 <= 'Z')
				ch2 += 'a' - 'A';
			else if (IS_HIGHBIT_SET(ch2) && isupper(ch2))
				ch2 = (unsigned char)tolower(ch2);

			if (ch1 != ch2)
				return (int) ch1 - (int) ch2;
		}
		if (ch1 == 0)
			break;
	}
	return 0;
}

/*
 * Case-independent comparison of two not-necessarily-null-terminated
 * strings. At most n bytes will be examined from each string.
 */
int
pqt_strncasecmp(const char *s1, const char *s2, size_t n)
{
	while (n-- > 0)
	{
		unsigned char ch1 = (unsigned char) *s1++;
		unsigned char ch2 = (unsigned char) *s2++;

		if (ch1 != ch2)
		{
			if (ch1 >= 'A' && ch1 <= 'Z')
				ch1 += 'a' - 'A';
			else if (IS_HIGHBIT_SET(ch1) && isupper(ch1))
				ch1 = (unsigned char)tolower(ch1);

			if (ch2 >= 'A' && ch2 <= 'Z')
				ch2 += 'a' - 'A';
			else if (IS_HIGHBIT_SET(ch2) && isupper(ch2))
				ch2 = (unsigned char)tolower(ch2);

			if (ch1 != ch2)
				return (int) ch1 - (int) ch2;
		}
		if (ch1 == 0)
			break;
	}
	return 0;
}



