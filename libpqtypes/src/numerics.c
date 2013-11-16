
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

/*
 * Macros and structures for receiving numeric field in binary
 */
#define NBASE		10000
#define HALF_NBASE	5000
#define DEC_DIGITS	4			/* decimal digits per NBASE digit */
#define MUL_GUARD_DIGITS	2	/* these are measured in NBASE digits */
#define DIV_GUARD_DIGITS	4

/*
 * Hardcoded precision limit - arbitrary, but must be small enough that
 * dscale values will fit in 14 bits.
 */
#define NUMERIC_MAX_PRECISION		1000

/*
 * Sign values and macros to deal with packing/unpacking n_sign_dscale
 */
#define NUMERIC_SIGN_MASK	0xC000
#define NUMERIC_POS			0x0000
#define NUMERIC_NEG			0x4000
#define NUMERIC_NAN			0xC000
#define NUMERIC_DSCALE_MASK 0x3FFF
#define NUMERIC_SIGN(n)		((n)->n_sign_dscale & NUMERIC_SIGN_MASK)
#define NUMERIC_DSCALE(n)	((n)->n_sign_dscale & NUMERIC_DSCALE_MASK)
#define NUMERIC_IS_NAN(n)	(NUMERIC_SIGN(n) != NUMERIC_POS &&	\
							 NUMERIC_SIGN(n) != NUMERIC_NEG)

typedef short NumericDigit;
static const int round_powers[4] = {0, 1000, 100, 10};

typedef struct NumericVar
{
	int ndigits;            /* # of digits in digits[] - can be 0! */
	int weight;             /* weight of first digit */
	int sign;               /* NUMERIC_POS, NUMERIC_NEG, or NUMERIC_NAN */
	int dscale;             /* display scale */
	NumericDigit *buf;			/* start of palloc'd space for digits[] */
	NumericDigit *digits;		/* base-NBASE digits */
} NumericVar;

static int str2num(PGtypeArgs *args, const char *str, NumericVar *dest);
static int num2str(char *out, size_t outl, NumericVar *var, int dscale);

int
pqt_put_int2(PGtypeArgs *args)
{
	PGint2 n = (PGint2) va_arg(args->ap, int);
	pqt_buf_putint2(args->put.out, n);
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
	PGint4 n = va_arg(args->ap, PGint4);
	*(PGint4 *)args->put.out = (PGint4) htonl(n);
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
	PGint8 i8 = va_arg(args->ap, PGint8);
	pqt_swap8(args->put.out, &i8, 1);
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
	PGfloat4 f = (PGfloat4) va_arg(args->ap, double);
	void *vp = (void *)&f;
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
	PGfloat8 d = va_arg(args->ap, PGfloat8);
	pqt_swap8(args->put.out, &d, 1);
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

/* exposing a NumericVar struct to a libpq user, or something similar,
 * doesn't seem useful w/o a library to operate on it.  Instead, we
 * always expose a numeric in text format and let the API user decide
 * how to use it .. like strod or a 3rd party big number library.  We
 * always send a numeric in binary though.
 */
int
pqt_put_numeric(PGtypeArgs *args)
{
	int numlen;
	NumericVar num = {0};
	short *out;
	PGnumeric str = va_arg(args->ap, PGnumeric);

	if (str2num(args, str, &num))
	{
		if (num.digits)
			free(num.digits);
		return -1;
	}

	/* variable length data type, grow args->put.out buffer if needed */
	numlen = (int) sizeof(short) * (4 + num.ndigits);
	if (args->put.expandBuffer(args, numlen) == -1)
		return -1;

	out = (short *) args->put.out;
	*out++ = htons((short) num.ndigits);
	*out++ = htons((short) num.weight);
	*out++ = htons((short) num.sign);
	*out++ = htons((short) num.dscale);

	if (num.digits)
	{
		int i;
		for (i=0; i < num.ndigits; i++)
			*out++ = htons(num.digits[i]);
		free(num.digits);
	}

	return numlen;
}

/* exposing a NumericVar struct to a libpq user, or something similar,
 * doesn't seem useful w/o a library to operate on it.  Instead, we
 * always expose a numeric in text format and let the API user decide
 * how to use it .. like strod or a 3rd party big number library.
 */
int
pqt_get_numeric(PGtypeArgs *args)
{
	int i;
	short *s;
	NumericVar num;
	DECLVALUE(args);
	char buf[4096];
	size_t len;
	PGnumeric *str = va_arg(args->ap, PGnumeric *);

	CHKGETVALS(args, str);

	if (args->format == TEXTFMT)
	{
		*str = value;
		return 0;
	}

	s = (short *) value;
	num.ndigits = ntohs(*s); s++;
	num.weight  = ntohs(*s); s++;
	num.sign    = ntohs(*s); s++;
	num.dscale  = ntohs(*s); s++;
	num.digits  = (short *) malloc(num.ndigits * sizeof(short));
	if (!num.digits)
		RERR_MEM(args);

	for (i=0; i < num.ndigits; i++)
	{
		num.digits[i] = ntohs(*s);
		s++;
	}

	i = num2str(buf, sizeof(buf), &num, num.dscale);
	free(num.digits);

	/* num2str failed, only fails when 'str' is too small */
	if (i == -1)
		RERR(args, "out buffer is too small");

	len = strlen(buf)+1;
	*str = PQresultAlloc(args->get.result, len);
	if (!*str)
		RERR_MEM(args);

	memcpy(*str, buf, len);
	return 0;

}


/*
 * round_var
 *
 * Round the value of a variable to no more than rscale decimal digits
 * after the decimal point.  NOTE: we allow rscale < 0 here, implying
 * rounding before the decimal point.
 */
static void
round_var(NumericVar *var, int rscale)
{
	NumericDigit *digits = var->digits;
	int			di;
	int			ndigits;
	int			carry;

	var->dscale = rscale;

	/* decimal digits wanted */
	di = (var->weight + 1) * DEC_DIGITS + rscale;

	/*
	 * If di = 0, the value loses all digits, but could round up to 1 if its
	 * first extra digit is >= 5.  If di < 0 the result must be 0.
	 */
	if (di < 0)
	{
		var->ndigits = 0;
		var->weight = 0;
		var->sign = NUMERIC_POS;
	}
	else
	{
		/* NBASE digits wanted */
		ndigits = (di + DEC_DIGITS - 1) / DEC_DIGITS;

		/* 0, or number of decimal digits to keep in last NBASE digit */
		di %= DEC_DIGITS;

		if (ndigits < var->ndigits ||
			(ndigits == var->ndigits && di > 0))
		{
			var->ndigits = ndigits;

			if (di == 0)
				carry = (digits[ndigits] >= HALF_NBASE) ? 1 : 0;
			else
			{
				/* Must round within last NBASE digit */
				int			extra,
							pow10;

				pow10 = round_powers[di];
				extra = digits[--ndigits] % pow10;
				digits[ndigits] = digits[ndigits] - (NumericDigit) extra;
				carry = 0;
				if (extra >= pow10 / 2)
				{
					pow10 += digits[ndigits];
					if (pow10 >= NBASE)
					{
						pow10 -= NBASE;
						carry = 1;
					}
					digits[ndigits] = (NumericDigit) pow10;
				}
			}

			/* Propagate carry if needed */
			while (carry)
			{
				carry += digits[--ndigits];
				if (carry >= NBASE)
				{
					digits[ndigits] = (NumericDigit) (carry - NBASE);
					carry = 1;
				}
				else
				{
					digits[ndigits] = (NumericDigit) carry;
					carry = 0;
				}
			}

			if (ndigits < 0)
			{
				var->digits--;
				var->ndigits++;
				var->weight++;
			}
		}
	}
}

/*
 * strip_var
 *
 * Strip any leading and trailing zeroes from a numeric variable
 */
static void
strip_var(NumericVar *var)
{
	NumericDigit *digits = var->digits;
	int			ndigits = var->ndigits;

	/* Strip leading zeroes */
	while (ndigits > 0 && *digits == 0)
	{
		digits++;
		var->weight--;
		ndigits--;
	}

	/* Strip trailing zeroes */
	while (ndigits > 0 && digits[ndigits - 1] == 0)
		ndigits--;

	/* If it's zero, normalize the sign and weight */
	if (ndigits == 0)
	{
		var->sign = NUMERIC_POS;
		var->weight = 0;
	}

	var->digits = digits;
	var->ndigits = ndigits;
}

/*
 * str2num()
 *
 *	Parse a string and put the number into a variable
 *  returns -1 on error and 0 for success.
 */
static int
str2num(PGtypeArgs *args, const char *str, NumericVar *dest)
{
	const char *cp = str;
	int		have_dp = FALSE;
	int			i;
	unsigned char *decdigits;
	int			sign = NUMERIC_POS;
	int			dweight = -1;
	int			ddigits;
	int			dscale = 0;
	int			weight;
	int			ndigits;
	int			offset;
	NumericDigit *digits;

	/*
	 * We first parse the string to extract decimal digits and determine the
	 * correct decimal weight.	Then convert to NBASE representation.
	 */

	/* skip leading spaces */
	while (*cp)
	{
		if (!isspace((unsigned char) *cp))
			break;
		cp++;
	}

	switch (*cp)
	{
		case '+':
			sign = NUMERIC_POS;
			cp++;
			break;

		case '-':
			sign = NUMERIC_NEG;
			cp++;
			break;
	}

	if (*cp == '.')
	{
		have_dp = TRUE;
		cp++;
	}

	if (!isdigit((unsigned char) *cp))
		return args->errorf(args,
			"invalid input syntax for type numeric: '%s'", str);

	decdigits = (unsigned char *) malloc(strlen(cp) + DEC_DIGITS * 2);

	/* leading padding for digit alignment later */
	memset(decdigits, 0, DEC_DIGITS);
	i = DEC_DIGITS;

	while (*cp)
	{
		if (isdigit((unsigned char) *cp))
		{
			decdigits[i++] = *cp++ - '0';
			if (!have_dp)
				dweight++;
			else
				dscale++;
		}
		else if (*cp == '.')
		{
			if (have_dp)
			{
				free(decdigits);
				return args->errorf(args,
					"invalid input syntax for type numeric: '%s'", str);
			}

			have_dp = TRUE;
			cp++;
		}
		else
			break;
	}

	ddigits = i - DEC_DIGITS;
	/* trailing padding for digit alignment later */
	memset(decdigits + i, 0, DEC_DIGITS - 1);

	/* Handle exponent, if any */
	if (*cp == 'e' || *cp == 'E')
	{
		long		exponent;
		char	   *endptr;

		cp++;
		exponent = strtol(cp, &endptr, 10);
		if (endptr == cp)
		{
			free(decdigits);
			return args->errorf(args,
				"invalid input syntax for type numeric: '%s'", str);
		}

		cp = endptr;
		if (exponent > NUMERIC_MAX_PRECISION ||
			exponent < -NUMERIC_MAX_PRECISION)
		{
			free(decdigits);
			return args->errorf(args,
				"invalid input syntax for type numeric: '%s'", str);
		}

		dweight += (int) exponent;
		dscale -= (int) exponent;
		if (dscale < 0)
			dscale = 0;
	}

	/* Should be nothing left but spaces */
	while (*cp)
	{
		if (!isspace((unsigned char) *cp))
		{
			free(decdigits);
			return args->errorf(args,
				"invalid input syntax for type numeric: '%s'", str);
		}
		cp++;
	}

	/*
	 * Okay, convert pure-decimal representation to base NBASE.  First we need
	 * to determine the converted weight and ndigits.  offset is the number of
	 * decimal zeroes to insert before the first given digit to have a
	 * correctly aligned first NBASE digit.
	 */
	if (dweight >= 0)
		weight = (dweight + 1 + DEC_DIGITS - 1) / DEC_DIGITS - 1;
	else
		weight = -((-dweight - 1) / DEC_DIGITS + 1);
	offset = (weight + 1) * DEC_DIGITS - (dweight + 1);
	ndigits = (ddigits + offset + DEC_DIGITS - 1) / DEC_DIGITS;

	dest->digits = (NumericDigit *) malloc((ndigits) * sizeof(NumericDigit));
	dest->ndigits = ndigits;
	dest->sign = sign;
	dest->weight = weight;
	dest->dscale = dscale;

	i = DEC_DIGITS - offset;
	digits = dest->digits;

	while (ndigits-- > 0)
	{
		*digits++ = ((decdigits[i] * 10 + decdigits[i + 1]) * 10 +
					 decdigits[i + 2]) * 10 + decdigits[i + 3];
		i += DEC_DIGITS;
	}

	free(decdigits);

	/* Strip any leading/trailing zeroes, and normalize weight if zero */
	strip_var(dest);
	return 0;
}

/*
 * num2str() -
 *
 *	Convert a var to text representation (guts of numeric_out).
 *	CAUTION: var's contents may be modified by rounding!
 *	returns -1 on error and 0 for success.
 */
static int
num2str(char *out, size_t outl, NumericVar *var, int dscale)
{
	//char	   *str;
	char	   *cp;
	char	   *endcp;
	int			i;
	int			d;
	NumericDigit dig;
	NumericDigit d1;

	if (dscale < 0)
		dscale = 0;

	/*
	 * Check if we must round up before printing the value and do so.
	 */
	round_var(var, dscale);

	/*
	 * Allocate space for the result.
	 *
	 * i is set to to # of decimal digits before decimal point. dscale is the
	 * # of decimal digits we will print after decimal point. We may generate
	 * as many as DEC_DIGITS-1 excess digits at the end, and in addition we
	 * need room for sign, decimal point, null terminator.
	 */
	i = (var->weight + 1) * DEC_DIGITS;
	if (i <= 0)
		i = 1;

	if (outl <= (size_t) (i + dscale + DEC_DIGITS + 2))
		return -1;

	//str = palloc(i + dscale + DEC_DIGITS + 2);
	//cp = str
	cp = out;

	/*
	 * Output a dash for negative values
	 */
	if (var->sign == NUMERIC_NEG)
		*cp++ = '-';

	/*
	 * Output all digits before the decimal point
	 */
	if (var->weight < 0)
	{
		d = var->weight + 1;
		*cp++ = '0';
	}
	else
	{
		for (d = 0; d <= var->weight; d++)
		{
			dig = (d < var->ndigits) ? var->digits[d] : 0;
			/* In the first digit, suppress extra leading decimal zeroes */
			{
				int		putit = (d > 0);

				d1 = dig / 1000;
				dig -= d1 * 1000;
				putit |= (d1 > 0);
				if (putit)
					*cp++ = (char) (d1 + '0');
				d1 = dig / 100;
				dig -= d1 * 100;
				putit |= (d1 > 0);
				if (putit)
					*cp++ = (char) (d1 + '0');
				d1 = dig / 10;
				dig -= d1 * 10;
				putit |= (d1 > 0);
				if (putit)
					*cp++ = (char) (d1 + '0');
				*cp++ = (char) (dig + '0');
			}
		}
	}

	/*
	 * If requested, output a decimal point and all the digits that follow it.
	 * We initially put out a multiple of DEC_DIGITS digits, then truncate if
	 * needed.
	 */
	if (dscale > 0)
	{
		*cp++ = '.';
		endcp = cp + dscale;
		for (i = 0; i < dscale; d++, i += DEC_DIGITS)
		{
			dig = (d >= 0 && d < var->ndigits) ? var->digits[d] : 0;
			d1 = dig / 1000;
			dig -= d1 * 1000;
			*cp++ = (char) (d1 + '0');
			d1 = dig / 100;
			dig -= d1 * 100;
			*cp++ = (char) (d1 + '0');
			d1 = dig / 10;
			dig -= d1 * 10;
			*cp++ = (char) (d1 + '0');
			*cp++ = (char) (dig + '0');
		}
		cp = endcp;
	}

	/*
	 * terminate the string and return it
	 */
	*cp = '\0';
	return 0;
}


