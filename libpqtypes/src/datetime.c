
/*
 * datetime.c
 *   Type handler for TIME, TIMETZ, DATE, TIMESTAMP and TIMESTAMPTZ
 *   data types.  Also includes public function PQlocalTZInfo.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

/* Microsoft went overboard with these warnings.  This avoids
 * warnings about _timezone and _tzname.  The alternatives
 * provided by microsoft are terrible.  All other warnings,
 * like strdup, snprintf, strcpy, etc.. have been avoided
 * by using Microsoft alternate functions.  This whole thing
 * is completely moronic.
 */
#define _CRT_SECURE_NO_WARNINGS

#include "libpqtypes-int.h"

#if defined(PQT_WIN32) || defined(HAVE_TIME_H)
#	include <time.h>
#endif

#if defined(HAVE_CONFIG_H) && defined(HAVE_SYS_TIME_H)
#	include <sys/time.h>
#endif

#define MINGMTOFF -53940 /* -1459 */
#define MAXGMTOFF  53940 /* +1459 */
#define ISVALIDGMTOFF(off) ((off) >= MINGMTOFF && (off) <= MAXGMTOFF)

#define CHECKDATEVALS(_args, _d) do{ \
	if ((_d)->year < 0) \
		RERR(_args, "invalid year value ... cannot be negative"); \
	if ((_d)->mon < 0 || (_d)->mon > 11) \
		RERR(_args, "invalid month value ... range is 0 to 11"); \
	if ((_d)->mday < 1 || (_d)->mday > 31) \
		RERR(_args, "invalid day value ... range is 1 to 31"); \
}while (0)

#define CHECKTIMEVALS(_args, _t, _withtz) do{ \
	if ((_t)->hour < 0 || (_t)->hour > 23) \
		RERR(_args, "invalid hour value ... range is 0 to 23"); \
	if ((_t)->min < 0 || (_t)->min > 59) \
		RERR(_args, "invalid minute value ... range is 0 to 59"); \
	if ((_t)->sec < 0 || (_t)->sec > 59) \
		RERR(_args, "invalid second value ... range is 0 to 59"); \
	if ((_t)->usec < 0 || (_t)->usec > 999999) \
		RERR(_args, "invalid microsecond value ... range is 0 to 999999"); \
	if ((_withtz) && !ISVALIDGMTOFF((_t)->gmtoff)) \
		return (_args)->errorf(_args, \
			"invalid gmtoff value ... range is %d to %d", MINGMTOFF, MAXGMTOFF); \
}while (0)

#define MONTHS_PER_YEAR 12
#define SECS_PER_YEAR	(36525 * 864)	/* avoid floating-point computation */
#define SECS_PER_DAY	86400
#define SECS_PER_HOUR	3600
#define SECS_PER_MINUTE 60
#define MINS_PER_HOUR	60
#define USECS_PER_DAY	PQT_INT64CONST(86400000000)
#define USECS_PER_HOUR	PQT_INT64CONST(3600000000)
#define USECS_PER_MINUTE PQT_INT64CONST(60000000)
#define USECS_PER_SEC	PQT_INT64CONST(1000000)
#define UNIX_EPOCH_JDATE		2440588 /* == date2j(1970, 1, 1) */
#define POSTGRES_EPOCH_JDATE	2451545 /* == date2j(2000, 1, 1) */

#define TS_PREC_INV 1000000.0
#define TSROUND(j) (rint(((double) (j)) * TS_PREC_INV) / TS_PREC_INV)

/* round off to MAX_TIME_PRECISION decimal places */
#define TIME_PREC_INV 10000000000.0
#define TIMEROUND(j) (rint(((double) (j)) * TIME_PREC_INV) / TIME_PREC_INV)

#define TMODULO(t,q,u) \
do { \
	(q) = (int) (((t) < 0) ? ceil((t) / (u)) : floor((t) / (u))); \
	if ((q) != 0) (t) -= rint((q) * (u)); \
} while (0)

/* Converts "1209 BC" notation to "-1208" */
#define BC2YEAR(_isbc, _yr) (_isbc) ? -((_yr) - 1) : (_yr)
/* Converts "-1208" to "1209 BC" notation */
#define YEAR2BC(_yr) ((_yr) <= 0) ? -((_yr) - 1) : (_yr)

/* date & time functions */
static int date2j(int y, int m, int d);
static void gmtoff2name(int gmtoff, char *tzNameBuf, size_t size);
static void tzabbr2info(const char *tzabbr, int *gmtoff, int *isdst);
static int text2time(char *timestr, PGtime *time, int withtz);
static int text2interval(char *intvlstr, PGinterval *interval);
static int text2date(const char *datestr, PGdate *date,
	const char *datestyle);
static int bin2pgdate(int d, PGdate *date);
static int bin2pgtime(double t, int gmtoff, PGtime *time, int withtz);
static int bin2pginterval(double tval, int days, int mons,
	PGinterval *interval);
static int bin2pgts(double ts, PGtimestamp *pgts, int withtz);
static char *time2t(PGtypeArgs *args, PGtime *t, void *val);

#ifdef PQT_WINAPI
static char *_win32_tzabbr(const char *fullname);
#endif

#if defined(HAVE_CONFIG_H) && !defined(HAVE_FLOOR)
static double
floor(double x)
{
	/* also works if modf always returns a positive fractional part */
	double val;
	return modf(x, &val) < 0 ? val - 1.0 : val ;
}
#endif

#if defined(HAVE_CONFIG_H) && !defined(HAVE_CEIL)
static double
ceil(double x)
{
	/* also works if modf always returns a positive fractional part */
	double val;
	return modf(x, &val) > 0 ? val + 1.0 : val ;
}
#endif

/* MSVC and unixes w/o rint */
#if defined(PQT_MSVC) || (defined(HAVE_CONFIG_H) && !defined(HAVE_RINT))
static double
rint(double x)
{
	double f, n = 0.;

	f = modf(x, &n);

	if (x > 0.)
	{
		if (f > .5)
			n += 1.;
	}
	else if (x < 0.)
	{
		if (f < -.5)
			n -= 1.;
	}

	return n;
}
#endif

/*
 * time:   PGtime members must be set: hour, min, sec, usec.
 * timetz: PGtime members must be set: hour, min, sec, usec, gmtoff.
 */
static int
put_time2(PGtypeArgs *args, int withtz)
{
	int r=8;
	char tbuf[8];
	PGtime *time = va_arg(args->ap, PGtime *);

	PUTNULLCHK(args, time);
	CHECKTIMEVALS(args, time, withtz);

	pqt_swap8(args->put.out, time2t(args, time, tbuf), 1);

	if (withtz)
	{
		pqt_buf_putint4(args->put.out + 8, -time->gmtoff);
		r += 4;
	}

	return r;
}

int
pqt_put_time(PGtypeArgs *args)
{
	return put_time2(args, 0);
}

int
pqt_put_timetz(PGtypeArgs *args)
{
	return put_time2(args, 1);
}

/* Also handles timetz */
static int
get_time2(PGtypeArgs *args, int withtz)
{
	DECLVALUE(args);
	double t;
	int gmtoff=0;
	char tbuf[8];
	PGtime *time = va_arg(args->ap, PGtime *);

	CHKGETVALS(args, time);

	if (args->format == TEXTFMT)
	{
		if (text2time(value, time, withtz) == -1)
			RERR(args, "invalid time format");
		return 0;
	}

	/* read and convert binary data */
	pqt_swap8(tbuf, value, 0);
	if (withtz)
		gmtoff = -pqt_buf_getint4(value + 8);

	/* Convert 8 byte integer format to double precision. */
	if (args->fmtinfo->integer_datetimes)
	{
		PGint8 n = *(PGint8 *) tbuf;
		t = (double) n / 1000000.0;
	}
	else
	{
		t = *(double *) tbuf;
	}

	/* convert binary data to a PGtime */
	if (bin2pgtime(t, gmtoff, time, withtz) == -1)
		RERR(args, "negative julian day detected");

	return 0;
}

int
pqt_get_time(PGtypeArgs *args)
{
	return get_time2(args, 0);
}

int
pqt_get_timetz(PGtypeArgs *args)
{
	return get_time2(args, 1);
}

/* PGdate members required isbc, year, mon, mday */
int
pqt_put_date(PGtypeArgs *args)
{
	int dval;
	PGdate *date = va_arg(args->ap, PGdate *);

	PUTNULLCHK(args, date);
	CHECKDATEVALS(args, date);

	dval = date2j(BC2YEAR(date->isbc, date->year), date->mon+1, date->mday)
		- POSTGRES_EPOCH_JDATE;

	pqt_buf_putint4(args->put.out, dval);
	return 4;
}

int
pqt_get_date(PGtypeArgs *args)
{
	DECLVALUE(args);
	int d;
	PGdate *date = va_arg(args->ap, PGdate *);

	CHKGETVALS(args, date);

	if (args->format == TEXTFMT)
	{
		if (text2date(value, date, args->fmtinfo->datestyle) == -1)
			RERR(args, "invalid date format");
		return 0;
	}

	d = pqt_buf_getint4(value);
	if (bin2pgdate(d, date) == -1)
		RERR(args, "binary date conversion failed");

	return 0;
}

/*
 * timestamp: The timestamp is never converted from what is provided.  The
 *   timezone info is dropped as pgtype 'time' has no concept of timezones.
 *   The following PGtimestamp members must be set: date.isbc, date.year,
 *   date.mon, date.mday, time.hour, time.min, time.sec, time.usec.
 *
 * timestamptz: The timestamptz is always converted to GMT.  Its adjusted
 *   by PGtime.gmtoff, so set this value to zero if you already have GMT.
 *   The following PGtimestamp members must be set: date.isbc, date.year,
 *   date.mon, date.mday, time.hour, time.min, time.sec, time.usec,
 *   time.gmtoff.
 */
static int
put_timestamp2(PGtypeArgs *args, int withtz)
{
	char tbuf[8];
	int year, mon;
	PGtimestamp *pgts = va_arg(args->ap, PGtimestamp *);
	PGtime *t = &pgts->time;

	PUTNULLCHK(args, pgts);
	CHECKDATEVALS(args, &pgts->date);
	CHECKTIMEVALS(args, t, withtz);

	mon = pgts->date.mon + 1;
	year = BC2YEAR(pgts->date.isbc, pgts->date.year);

	time2t(args, t, tbuf);

	if (args->fmtinfo->integer_datetimes)
	{
		PGint8 time = *(PGint8 *) tbuf;
		int date = date2j(year, mon, pgts->date.mday) - POSTGRES_EPOCH_JDATE;
		PGint8 val = (PGint8) (date * USECS_PER_DAY + time);

		/* check for major overflow */
		if ((val - time) / USECS_PER_DAY != date)
			RERR(args, "timestamp overflow");

		/* check for just-barely overflow (okay except time-of-day wraps) */
		if ((val < 0 && date >= 0) || (val >= 0 && date < 0))
			RERR(args, "timestamp overflow");

		/* currently in localtime, convert to GMT */
		if (withtz)
			val -= (t->gmtoff * USECS_PER_SEC);

		pqt_swap8(args->put.out, &val, 1);
	}
	else
	{
		double time = *(double *) tbuf;
		double date = date2j(year, mon, pgts->date.mday) - POSTGRES_EPOCH_JDATE;
		double val = (double) (date * SECS_PER_DAY + time);

		/* currently in localtime, convert to GMT */
		if (withtz)
			val -= (t->gmtoff);

		pqt_swap8(args->put.out, &val, 1);
	}

	return 8;
}

int
pqt_put_timestamp(PGtypeArgs *args)
{
	return put_timestamp2(args, 0);
}

int
pqt_put_timestamptz(PGtypeArgs *args)
{
	return put_timestamp2(args, 1);
}

/* Also handles timestamptz */
static int
get_timestamp2(PGtypeArgs *args, int withtz)
{
	DECLVALUE(args);
	char tsbuf[8];
	double ts;
	PGtimestamp *pgts = va_arg(args->ap, PGtimestamp *);

	CHKGETVALS(args, pgts);

	if (args->format == TEXTFMT)
	{
		double time;
		double date;
		int year;

		if (text2date(value, &pgts->date, args->fmtinfo->datestyle) == -1)
			RERR(args, "invalid date format");

		if (text2time(value, &pgts->time, withtz) == -1)
			RERR(args, "invalid time format");

		/* compute ts & epoch values. */
		time2t(NULL, &pgts->time, &time);
		year = BC2YEAR(pgts->date.isbc, pgts->date.year);
		date = date2j(year, pgts->date.mon+1, pgts->date.mday) -
			POSTGRES_EPOCH_JDATE;
		ts  = (double) (date * SECS_PER_DAY + time);

		/* currently in localtime, convert to GMT */
		if (pgts->time.withtz)
			ts -= (pgts->time.gmtoff);

		pgts->epoch = (PGint8) rint(ts - ((double) pgts->time.usec/1000000.0) +
			(POSTGRES_EPOCH_JDATE - UNIX_EPOCH_JDATE) * SECS_PER_DAY);

		return 0;
	}

	pqt_swap8(tsbuf, value, 0);

	/* Convert 8 byte integer format to double precision. */
	if (args->fmtinfo->integer_datetimes)
	{
		PGint8 n = *(PGint8 *) tsbuf;
		ts = (double)n / 1000000.0;
	}
	else
	{
		ts = *(double *) tsbuf;
	}

	if (bin2pgts(ts, pgts, withtz) == -1)
		RERR(args, "negative julian day detected");

	return 0;
}

int
pqt_get_timestamp(PGtypeArgs *args)
{
	return get_timestamp2(args, 0);
}

int
pqt_get_timestamptz(PGtypeArgs *args)
{
	return get_timestamp2(args, 1);
}

int
pqt_put_interval(PGtypeArgs *args)
{
	int day;
	int month;
	PGinterval *intvl = va_arg(args->ap, PGinterval *);

	PUTNULLCHK(args, intvl);

	month = intvl->years * MONTHS_PER_YEAR + intvl->mons;
	day = intvl->days;

	if (args->fmtinfo->integer_datetimes)
	{
		PGint8 val = (PGint8) ((((((intvl->hours * PQT_INT64CONST(60)) +
			intvl->mins) * PQT_INT64CONST(60)) + intvl->secs) * USECS_PER_SEC) +
			intvl->usecs);

		pqt_swap8(args->put.out, &val, 1);
	}
	else
	{
		double val = (double) ((((intvl->hours * (double) MINS_PER_HOUR) +
			intvl->mins) * (double) SECS_PER_MINUTE) + (double) intvl->secs
				+ ((double) intvl->usecs/1000000.0));

		pqt_swap8(args->put.out, &val, 1);
	}

	pqt_buf_putint4(args->put.out + 8, day);
	pqt_buf_putint4(args->put.out + 12, month);
	return 16; /* val(8) + day(4) + mon(4) */

}

int
pqt_get_interval(PGtypeArgs *args)
{
	DECLVALUE(args);
	double tval;
	int mons;
	int days;
	char tvalbuf[8];
	PGinterval *interval = va_arg(args->ap, PGinterval *);

	CHKGETVALS(args, interval);

	if (args->format == TEXTFMT)
	{
		if (text2interval(value, interval)== -1)
			RERR(args, "invalid interval format");
		return 0;
	}

	pqt_swap8(tvalbuf, value, 0);
	days = pqt_buf_getint4(value + 8);
	mons = pqt_buf_getint4(value + 12);

	/* Convert 8 byte integer format to double precision. */
	if (args->fmtinfo->integer_datetimes)
	{
		PGint8 n = *(PGint8 *) tvalbuf;
		tval = (double) n / 1000000.0;
	}
	else
	{
		tval = *(double *) tvalbuf;
	}

	if (bin2pginterval(tval, days, mons, interval) == -1)
		RERR(args, "binary interval conversion failed");
	return 0;
}

void
PQlocalTZInfo(time_t *t, int *gmtoff, int *isdst, char **tzabbr)
{
#if defined(HAVE_STRUCT_TM_TM_ZONE) || defined(HAVE_TM_ZONE) || \
		defined(HAVE_TZNAME)
	time_t tbuf;
	struct tm *loc;
	struct tm buf;

	if (!t)
	{
		tbuf = time(NULL);
		t = &tbuf;
	}

#	ifdef HAVE_LOCALTIME_R
	/* return value inconsistent. for instance: HP-UX 10 returns an int */
	loc = &buf;
	localtime_r(t, loc);
#	else
	buf.tm_hour = 0; /* avoid compiler warning about unreferenced variable */
	loc = localtime(t);
#	endif

	*gmtoff = 0;
	*isdst = loc->tm_isdst==1 ? 1 : loc->tm_isdst==0 ? 0 : -1;
	if (tzabbr)
		*tzabbr = "";

#	ifdef HAVE_STRUCT_TM_TM_GMTOFF
	*gmtoff = (int) loc->tm_gmtoff;
#	endif

#	if defined(HAVE_STRUCT_TM_TM_ZONE) || defined(HAVE_TM_ZONE)
	if (tzabbr)
		*tzabbr = (char *) loc->tm_zone;

	/* Use global timezone and tzname variables */
#	elif defined(HAVE_TZNAME)
	*gmtoff = -((loc->tm_isdst > 0) ? pqt_timezone - SECS_PER_HOUR :
		pqt_timezone);
	if (tzabbr)
		*tzabbr = pqt_tzname[(loc->tm_isdst > 0)];

	/* When winapi is defined, msvc/mingw, tzname contains the windows
	 * timezone names, like "Eastern Daylight Time" instead of EDT.  This
	 * function will map these names to the standard abbrev.
	 */
#	ifdef PQT_WINAPI
	if (tzabbr)
		*tzabbr = _win32_tzabbr(*tzabbr);
#	endif
#	endif /* HAVE_TZNAME */

#else
	*gmtoff = 0;
	*isdst = -1;
	if (tzabbr)
		*tzabbr = "";
#endif /* HAVE_STRUCT_TM_TM_ZONE || HAVE_TM_ZONE || HAVE_TZNAME */
}

static char *
time2t(PGtypeArgs *args, PGtime *t, void *val)
{
	if (args && args->fmtinfo->integer_datetimes)
		*(PGint8 *) val = (PGint8) (((((t->hour * MINS_PER_HOUR + t->min) *
			SECS_PER_MINUTE) + t->sec) * USECS_PER_SEC) + t->usec);
	else
		*(double *) val = (double) (((t->hour * MINS_PER_HOUR + t->min) *
			SECS_PER_MINUTE) + t->sec + (t->usec/1000000.0));
	return val;
}

static int
text2date(const char *datestr, PGdate *date, const char *datestyle)
{
	int year;

	while (isspace((int) *datestr))
		++datestr;

	errno = 0;

	/* ISO, SQL or German ('1997-12-17', '12/17/1997', '17.12.1997') */
	if (isdigit((int) *datestr))
	{
		int n[3];

		n[0] = (int) strtol(datestr, (char **) &datestr, 10);
		if (errno || (*datestr!='-' && *datestr!='/' && *datestr!='.'))
			return -1;

		n[1] = (int) strtol(datestr+1, (char **) &datestr, 10);
		if (errno || (*datestr!='-' && *datestr!='/' && *datestr!='.'))
			return -1;

		n[2] = (int) strtol(datestr+1, (char **) &datestr, 10);
		if (errno)
			return -1;

		if (!*datestyle || strstr(datestyle, "ISO") || strstr(datestyle, "YMD"))
		{
			date->year = n[0];
			date->mon  = n[1];
			date->mday = n[2];
		}
		else if (strstr(datestyle, "DMY"))
		{
			date->mday = n[0];
			date->mon  = n[1];
			date->year = n[2];
		}
		else /* MDY */
		{
			date->mon  = n[0];
			date->mday = n[1];
			date->year = n[2];
		}

		date->mon--;
	}
	/* Postgres style: 'Thu Dec 13 19:42:52.442126 2007 EST' */
	else
	{
		static char *monnames[] = {"jan", "feb", "mar", "apr", "may",
			"jun", "jul", "aug", "sep", "oct","nov", "dec"};

		int i;

		/* skip day of week name */
		if (!(datestr = strchr(datestr, ' ')))
			return -1;
		datestr++;

		/* datestr is the day DMY */
		if (isdigit((int) *datestr))
		{
			date->mday = (int) strtol(datestr, (char **) &datestr, 10);
			if (errno)
				return -1;

			datestr++;
			for (i=0; i < 12; i++)
			{
				if (pqt_strncasecmp(datestr, monnames[i], 3)==0)
				{
					date->mon = i;
					break;
				}
			}

			if (i == 12)
				return -1;

			datestr += 4; /* skip space after monname */
		}
		/* datestr is the monname MDY */
		else
		{
			int i;

			for (i=0; i < 12; i++)
			{
				if (pqt_strncasecmp(datestr, monnames[i], 3)==0)
				{
					date->mon = i;
					break;
				}
			}

			if (i == 12)
				return -1;

			datestr += 4; /* skip space after monname */
			date->mday = (int) strtol(datestr, (char **) &datestr, 10);
			if (errno)
				return -1;
			datestr++; /* space after month day */
		}

		/* skip time */
		if (!(datestr = strchr(datestr, ' ')))
			return -1;

		date->year = (int) strtol(datestr, NULL, 10);
		if (errno)
			return -1;
	}

	if (date->mday < 0 || date->mday > 31)
		return -1;

	if (date->mon < 0 || date->mon > 11)
		return -1;

	date->isbc = strstr(datestr, " BC") ? 1 : 0;
	year = BC2YEAR(date->isbc, date->year);
	date->jday = date2j(year, date->mon+1, date->mday);
	date->wday = (date->jday + 1) % 7;
	date->yday = date->jday - date2j(year, 1, 1);
	return 0;
}

/* timestr can be a valid timestamp[tz] or time[tz]. */
static int
text2time(char *timestr, PGtime *time, int withtz)
{
	char *p;

	memset(time, 0, sizeof(PGtime));
	time->isdst = -1;

	if (!(p = strchr(timestr, ':')))
		return -1;

	timestr = p - 2;
	errno = 0;

	time->hour = (int) strtol(timestr, &timestr, 10);
	if (errno || *timestr != ':' || time->hour < 0 || time->hour > 23)
		return -1;

	time->min = (int) strtol(timestr+1, &timestr, 10);
	if (errno || *timestr != ':' || time->min < 0 || time->min > 59)
		return -1;

	time->sec = (int) strtol(timestr+1, &timestr, 10);
	if (errno || time->sec < 0 || time->sec > 59)
		return -1;

	if (*timestr == '.')
	{
		int i=0;
		char buf[7];

		timestr++;

		/* Need 6 digits for usecs to work with strtol, so pad with zeros. */
		memset(buf, '0', 6);
		buf[6] = 0;
		while (isdigit((int) *timestr))
			buf[i++] = *timestr++;

		time->usec = (int) strtol(buf, NULL, 10);
		if (errno || time->usec < 0 || time->usec > 999999)
			return -1;
	}

	/* no timezone present */
	if (!withtz)
		return 0;


	/* ---------------------------------
	 * Following ISO 8601, also detecting timezone abbrev.
	 */

	time->withtz = 1;

	/* UTC, using 'Z' encoding '00:00:00Z' */
	if (*timestr == 'Z')
	{
		pqt_strcpy(time->tzabbr, sizeof(time->tzabbr), "UTC");
		timestr++;
	}
	/* have gmtoff */
	else if (*timestr=='-' || *timestr=='+')
	{
		int hour = 0;
		int min = 0;
		int sec = 0;
		char sign = *timestr++;
		char buf[3];

		buf[0] = *timestr++;
		buf[1] = *timestr++;
		buf[2] = 0;
		hour = (int) strtol(buf, NULL, 10);
		if (errno)
			return -1;

		/* +/-hh:mm:ss case (support seconds) */
		if (*timestr == ':')
			timestr++;

		/* have minute field */
		if (isdigit((int) *timestr))
		{
			buf[0] = *timestr++;
			buf[1] = *timestr++;
			min = (int) strtol(buf, NULL, 10);
			if (errno)
				return -1;

			if (*timestr == ':')
				timestr++;

			/* have second field */
			if (isdigit((int) *timestr))
			{
				buf[0] = *timestr++;
				buf[1] = *timestr++;
				sec = (int) strtol(buf, NULL, 10);
				if (errno)
					return -1;
			}
		}

		time->gmtoff = (hour * 3600) + (min * 60) + sec;
		if (sign == '-')
			time->gmtoff = -time->gmtoff;

		if (!ISVALIDGMTOFF(time->gmtoff))
			return -1;
	}

	/* find the beginning of tzname */
	while (*timestr && !isalpha((int) *timestr))
		timestr++;

	if (*timestr)
	{
		/* find timezone abbrev end */
		if (!(p = strchr(timestr, ' ')))
			p = timestr + strlen(timestr);

		memcpy(time->tzabbr, timestr, p - timestr);
		time->tzabbr[p - timestr] = 0;

		/* BC is not a timezone, false-positive */
		if (strcmp(time->tzabbr, "BC")==0)
			*time->tzabbr = 0;
	}

	if (time->gmtoff != 0 && !*time->tzabbr)
		gmtoff2name(time->gmtoff, time->tzabbr, sizeof(time->tzabbr));
	else if (time->gmtoff==0 && *time->tzabbr)
		tzabbr2info(time->tzabbr, &time->gmtoff, &time->isdst);

	return 0;
}

static int
text2interval(char *istr, PGinterval *interval)
{
	char *s;
	PGtime pgtime;
	int is_before = 0;

	errno = 0;
	memset(interval, 0, sizeof(PGinterval));

	is_before = strstr(istr, "ago") ? 1 : 0;

	if ((s = strstr(istr, " year")))
	{
		for (s=s-1; s > istr && !isspace((int) *s); --s) ;
		interval->years = (int)strtol(s, NULL, 10);
		if (errno)
			return -1;
		if (!is_before)
			is_before = interval->years < 0;
	}

	if ((s = strstr(istr, " mon")))
	{
		for (s=s-1; s > istr && !isspace((int) *s); --s) ;
		interval->mons = (int)strtol(s, NULL, 10);
		if (errno)
			return -1;
		if (!is_before)
			is_before = interval->mons < 0;
	}

	if ((s = strstr(istr, " day")))
	{
		for (s=s-1; s > istr && !isspace((int) *s); --s) ;
		interval->days = (int) strtol(s, NULL, 10);
		if (errno)
			return -1;
		if (!is_before)
			is_before = interval->days < 0;
	}

	/* Means ISO DateStyle is in use, uses the 00:00:00 time format */
	if (text2time(istr, &pgtime, 0) == 0)
	{
		interval->usecs = pgtime.usec;
		interval->secs  = pgtime.sec;
		interval->mins  = pgtime.min;
		interval->hours = pgtime.hour;

		/* text2time doesn't support negative values, detect is_before here.
		 * Find first ':' and subtract two-digit year + possible minus sign.
		 */
		if (!is_before)
			is_before = (s = strchr(istr, ':')) && (*(s - 3) == '-');
	}
	/* Means POSTGRES, SQL or GERMAN DateStyle is in use. */
	else
	{
		if ((s = strstr(istr, " hour")))
		{
			for (s=s-1; s > istr && !isspace((int) *s); --s) ;
			interval->hours = (int) strtol(s, NULL, 10);
			if (errno)
				return -1;
		}

		if ((s = strstr(istr, " min")))
		{
			for (s=s-1; s > istr && !isspace((int) *s); --s) ;
			interval->mins = (int) strtol(s, NULL, 10);
			if (errno)
				return -1;
		}

		if ((s = strstr(istr, " sec")))
		{
			for (s=s-1; s > istr && !isspace((int) *s); --s) ;
			interval->secs = (int) strtol(s, &s, 10);
			if (errno)
				return -1;

			/* usecs */
			if (*s == '.')
			{
				int i=0;
				char buf[7];
				memset(buf, '0', 6);
				buf[6] = 0;
				for (s=s+1; isdigit((int) *s); s++)
					buf[i++] = *s;
				interval->usecs = atoi(buf);
			}
		}
	}

	/* flip unit signs where still needed */
	if (is_before)
	{
		if (interval->years > 0)
			interval->years = -interval->years;
		if (interval->mons > 0)
			interval->mons = -interval->mons;
		if (interval->days > 0)
			interval->days = -interval->days;
		if (interval->hours > 0)
			interval->hours = -interval->hours;
		if (interval->mins > 0)
			interval->mins = -interval->mins;
		if (interval->secs > 0)
			interval->secs = -interval->secs;
		if (interval->usecs > 0)
			interval->usecs = -interval->usecs;
	}

	return 0;
}

/* Uses ISO 8601 GMT+/-hhmmss notation.  See tzabbr2info as well. */
static void
gmtoff2name(int gmtoff, char *buf, size_t size)
{
	char sign;
	int mins;
	int secs;

	if (gmtoff < 0)
	{
		sign = '-';
		gmtoff = -gmtoff;
	}
	else
	{
		sign = '+';
	}

	mins = gmtoff % 3600;
	secs = mins % 60;

	if (secs == 0)
		pqt_snprintf(buf, size, "GMT%c%02d%02d", sign, gmtoff/3600, mins/60);
	else
		pqt_snprintf(buf, size, "GMT%c%02d%02d%02d",
			sign, gmtoff/3600, mins/60, secs);
}

static int
date2j(int y, int m, int d)
{
	int	julian;
	int	century;

	if (m > 2)
	{
		m += 1;
		y += 4800;
	}
	else
	{
		m += 13;
		y += 4799;
	}

	century = y / 100;
	julian = y * 365 - 32167;
	julian += y / 4 - century + century / 4;
	julian += 7834 * m / 256 + d;

	return julian;
}

static void
j2date(int jd, int *year, int *month, int *day)
{
	unsigned int julian;
	unsigned int quad;
	unsigned int extra;
	int	y;

	julian = jd;
	julian += 32044;
	quad = julian / 146097;
	extra = (julian - quad * 146097) * 4 + 3;
	julian += 60 + quad * 3 + extra / 146097;
	quad = julian / 1461;
	julian -= quad * 1461;
	y = julian * 4 / 1461;
	julian = ((y != 0) ? ((julian + 305) % 365) : ((julian + 306) % 366))
		+ 123;
	y += quad * 4;
	*year = y - 4800;
	quad = julian * 2141 / 65536;
	*day = julian - 7834 * quad / 256;
	*month = (quad + 10) % 12 + 1;
}

static void
dt2time(double jd, int *hour, int *min, int *sec, double *fsec)
{
	*hour = (int) (jd / SECS_PER_HOUR);
	jd -= (*hour) * SECS_PER_HOUR;
	*min = (int) (jd / SECS_PER_MINUTE);
	jd -= (*min) * SECS_PER_MINUTE;
	*sec = (int) jd;
	*fsec = jd - *sec;
}

static int
bin2pgdate(int dval, PGdate *date)
{
	memset(date, 0, sizeof(PGdate));

	j2date(dval + POSTGRES_EPOCH_JDATE, &date->year, &date->mon, &date->mday);
	date->jday = date2j(date->year, date->mon, date->mday);
	date->yday = date->jday - date2j(date->year, 1, 1);
	date->wday = (date->jday + 1) % 7;

	if (date->year <= 0)
	{
	  date->isbc = 1;
	  date->year = -(date->year-1);
	}

	date->mon--;
	return 0;
}

/* gmtoff is seconds east of GMT, adjusted prior to call. */
static int
bin2pgtime(double tval, int gmtoff, PGtime *time, int withtz)
{
	double rem;

	memset(time, 0, sizeof(PGtime));
	time->isdst = -1; /* no way of knowing */

	if (withtz)
	{
		time->gmtoff = gmtoff;
		gmtoff2name(time->gmtoff, time->tzabbr, sizeof(time->tzabbr));
		time->withtz = 1;
	}

	RECALC:
	rem = tval;
	TMODULO(rem, time->hour, (double) SECS_PER_HOUR);
	TMODULO(rem, time->min, (double) SECS_PER_MINUTE);
	TMODULO(rem, time->sec, 1.0);
	rem = TIMEROUND(rem);

	/* roundoff may need to propagate to higher-order fields */
	if (rem >= 1.0)
	{
		tval = ceil(tval);
		goto RECALC;
	}

	time->usec = (int) (rem * 1000000.0);

	return 0;
}

static int
breakdown_time(double time, double *datep, PGdate *pgdate,
	PGtime *pgtime, double *fsec)
{
	double date;

	TMODULO(time, date, (double) SECS_PER_DAY);

	if (time < 0)
	{
		time += SECS_PER_DAY;
		date -= 1;
	}

	/* add offset to go from J2000 back to standard Julian date */
	date += POSTGRES_EPOCH_JDATE;

	RECALC_DATE:
	/* Julian day routine does not work for negative Julian days */
	if (date < 0 || date > (PGint8) INT_MAX)
		return -1;

	j2date((int)date, &pgdate->year, &pgdate->mon, &pgdate->mday);

	RECALC_TIME:
	dt2time(time, &pgtime->hour, &pgtime->min, &pgtime->sec, fsec);
	*fsec = TSROUND(*fsec);

	/* roundoff may need to propagate to higher-order fields */
	if (*fsec >= 1.0)
	{
		time = ceil(time);
		if (time >= (double) SECS_PER_DAY)
		{
			time = 0;
			date += 1;
			goto RECALC_DATE;
		}
		goto RECALC_TIME;
	}

	if (datep)
		*datep = date;
	return 0;
}

static int
bin2pgts(double ts, PGtimestamp *pgts, int withtz)
{
	double fsec;
	double date;
	double time;

	memset(pgts, 0, sizeof(PGtimestamp));
	pgts->time.isdst = -1;

	time = ts;

	/* server sent UTC, convert to localtime.  Need to determine the
	 * gmtoff for the timestamp epoch value (daylight or not).  This
	 * can differ from what zone the machine is in.  If its July in
	 * NY but the timestamp is Jan, we need to apply EST to timestamp
	 * rather than EDT.
	 */
	if (withtz)
	{
		char *tzabbr;
		time_t tepoch;
		PGtime pgtime;
		PGdate pgdate;
		PGint8 t64 = (PGint8) rint(ts);

		pgts->epoch = (PGint8) rint((double) t64 +
			(POSTGRES_EPOCH_JDATE - UNIX_EPOCH_JDATE) * SECS_PER_DAY);

		/* Safe range for a time_t is 0 - INT_MAX.  Some system don't support
		 * negative values at all.  System with 64-bit time_t can support
		 * wider ranges, but there is no way of determining what that is.
		 * NOTE: 64-bit time_t systems aren't limited to 31-bit upper values.
		 */
		if (pgts->epoch < 0 || (sizeof(time_t)==4 && pgts->epoch > INT_MAX))
		{
			struct tm tm;

			/* pull out the date and time parts so we can issue a mktime
			 * call, which gets us epoch.
			 */
			if (breakdown_time(ts, NULL, &pgdate, &pgtime, &fsec) < 0)
				return -1;

			memset(&tm, 0, sizeof(struct tm));
			tm.tm_year  = 70; /* out-of-safe-range values use 1970 */

			/* Daylight time was first used by Germany April 30, 1916. So,
			 * we only recognize its existence after this date.
			 */
			if (pgdate.year >= 1916)
				tm.tm_mon = pgdate.mon - 1;
			else  /* Ignore month before Apr 1916, use Jan for standard time */
				tm.tm_mon = 0;

			tm.tm_mday  = pgdate.mday;
			tm.tm_hour  = pgtime.hour;
			tm.tm_min   = pgtime.min;
			tm.tm_sec   = pgtime.sec;
			tm.tm_isdst = -1;

			tepoch = mktime(&tm);
		}
		else
		{
			tepoch = (time_t) pgts->epoch;
		}

		/* Have an epoch value that can be used to get TZInfo */
		PQlocalTZInfo(&tepoch, &pgts->time.gmtoff, &pgts->time.isdst, &tzabbr);
		pqt_strcpy(pgts->time.tzabbr, sizeof(pgts->time.tzabbr), tzabbr);

		/* adjust postgres timestamp by gmtoff. */
		time += pgts->time.gmtoff;
		pgts->time.withtz = 1;
	}

	/* breakdown the `localtime' adjusted timestamp */
	if (breakdown_time(time, &date, &pgts->date, &pgts->time, &fsec) < 0)
		return -1;

	/* jullian day */
	pgts->date.jday = (int) date;

	/* get the epoch, more accurate than previous calculation */
	pgts->epoch = (PGint8) rint(
		ts - fsec + (POSTGRES_EPOCH_JDATE - UNIX_EPOCH_JDATE) * SECS_PER_DAY);

	/* NOTE: dbl_epsilon is too small "2.2204460492503131e-16" */
	pgts->time.usec = (int) (fsec * (1000000.0 + 1E-9));

	/* use julian day calculations for wday and yday */
	pgts->date.wday = ((int) date + 1) % 7;
	pgts->date.yday = (int) date - date2j(pgts->date.year, 1, 1);

	/* Adjust year if this is BC */
	if (pgts->date.year <= 0)
	{
		pgts->date.year = -(pgts->date.year-1);
		pgts->date.isbc = 1;
	}

	pgts->date.mon--;
	return 0;
}

static int
bin2pginterval(double tval, int days, int mons, PGinterval *interval)
{
	double time;
	double tfrac;

	memset(interval, 0, sizeof(PGinterval));

	interval->years = mons / MONTHS_PER_YEAR;
	interval->mons = mons % MONTHS_PER_YEAR;
	interval->days = days;
	time = tval;

	RECALC:
	TMODULO(time, tfrac, (double) SECS_PER_HOUR);
	interval->hours = (int)tfrac;		/* could overflow ... */

	TMODULO(time, tfrac, (double) SECS_PER_MINUTE);
	interval->mins = (int)tfrac;

	TMODULO(time, tfrac, 1.0);
	interval->secs = (int)tfrac;

	time = TSROUND(time);
	/* roundoff may need to propagate to higher-order fields */
	if (time >= 1.0)
	{
		time = ceil(tval);
		goto RECALC;
	}

	/* NOTE: dbl_epsilon is too small "2.2204460492503131e-16" */
	interval->usecs = (int) (time * (1000000.0 + 1E-9));

	return 0;
}


/* Generated by the below query:
 *
 * SELECT '{"' || abbrev || '", ' ||
 *   ((EXTRACT(HOUR FROM utc_offset) * 3600) +
 *     (EXTRACT(MINUTE FROM utc_offset) * 60) +
 *     EXTRACT(SECOND FROM utc_offset)) ||
 *   ', ' || (CASE WHEN is_dst THEN 1 ELSE 0 END) || '},'
 *   FROM pg_timezone_abbrevs;
 */
struct _tzmap_
{
	const char *abbrev;
	int gmtoff;
	unsigned char isdst;
};

static struct _tzmap_ tzmap[] = {
	{"ACSST", 37800, 1},
	{"ACST", -14400, 1},
	{"ACT", -18000, 0},
	{"ADT", -10800, 1},
	{"AESST", 39600, 1},
	{"AEST", 36000, 0},
	{"AFT", 16200, 0},
	{"AKDT", -28800, 1},
	{"AKST", -32400, 0},
	{"ALMST", 25200, 1},
	{"ALMT", 21600, 0},
	{"AMST", 18000, 1},
	{"AMT", 14400, 0},
	{"ANAST", 46800, 1},
	{"ANAT", 43200, 0},
	{"ART", -10800, 0},
	{"AST", -14400, 0},
	{"AWSST", 32400, 1},
	{"AWST", 28800, 0},
	{"AZOST", 0, 1},
	{"AZOT", -3600, 0},
	{"AZST", 18000, 1},
	{"AZT", 14400, 0},
	{"BDST", 7200, 1},
	{"BDT", 21600, 0},
	{"BNT", 28800, 0},
	{"BORT", 28800, 0},
	{"BOT", -14400, 0},
	{"BRA", -10800, 0},
	{"BRST", -7200, 1},
	{"BRT", -10800, 0},
	{"BST", 3600, 1},
	{"BTT", 21600, 0},
	{"CADT", 37800, 1},
	{"CAST", 34200, 0},
	{"CCT", 28800, 0},
	{"CDT", -18000, 1},
	{"CEST", 7200, 1},
	{"CET", 3600, 0},
	{"CETDST", 7200, 1},
	{"CHADT", 49500, 1},
	{"CHAST", 45900, 0},
	{"CKT", 43200, 0},
	{"CLST", -10800, 1},
	{"CLT", -14400, 0},
	{"COT", -18000, 0},
	{"CST", -21600, 0},
	{"CXT", 25200, 0},
	{"DAVT", 25200, 0},
	{"DDUT", 36000, 0},
	{"EASST", -18000, 1},
	{"EAST", -21600, 0},
	{"EAT", 10800, 0},
	{"EDT", -14400, 1},
	{"EEST", 10800, 1},
	{"EET", 7200, 0},
	{"EETDST", 10800, 1},
	{"EGST", 0, 1},
	{"EGT", -3600, 0},
	{"EST", -18000, 0},
	{"FJST", -46800, 1},
	{"FJT", -43200, 0},
	{"FKST", -10800, 1},
	{"FKT", -14400, 0},
	{"FNST", -3600, 1},
	{"FNT", -7200, 0},
	{"GALT", -21600, 0},
	{"GAMT", -32400, 0},
	{"GEST", 14400, 1},
	{"GET", 10800, 0},
	{"GFT", -10800, 0},
	{"GILT", 43200, 0},
	{"GMT", 0, 0},
	{"GYT", -14400, 0},
	{"HKT", 28800, 0},
	{"HST", -36000, 0},
	{"ICT", 25200, 0},
	{"IOT", 21600, 0},
	{"IRKST", 32400, 1},
	{"IRKT", 28800, 0},
	{"IRT", 12600, 0},
	{"IST", 7200, 0},
	{"JAYT", 32400, 0},
	{"JST", 32400, 0},
	{"KDT", 36000, 1},
	{"KGST", 21600, 1},
	{"KGT", 18000, 0},
	{"KOST", 39600, 0},
	{"KRAST", 28800, 1},
	{"KRAT", 25200, 0},
	{"KST", 32400, 0},
	{"LHDT", 39600, 1},
	{"LHST", 37800, 0},
	{"LIGT", 36000, 0},
	{"LINT", 50400, 0},
	{"LKT", 21600, 0},
	{"MAGST", 43200, 1},
	{"MAGT", 39600, 0},
	{"MART", -34200, 0},
	{"MAWT", 21600, 0},
	{"MDT", -21600, 1},
	{"MEST", 7200, 1},
	{"MET", 3600, 0},
	{"METDST", 7200, 1},
	{"MEZ", 3600, 0},
	{"MHT", 43200, 0},
	{"MMT", 23400, 0},
	{"MPT", 36000, 0},
	{"MSD", 14400, 1},
	{"MSK", 10800, 0},
	{"MST", -25200, 0},
	{"MUT", 14400, 0},
	{"MVT", 18000, 0},
	{"MYT", 28800, 0},
	{"NDT", -9000, 1},
	{"NFT", -12600, 0},
	{"NOVST", 25200, 1},
	{"NOVT", 21600, 0},
	{"NPT", 20700, 0},
	{"NST", -12600, 0},
	{"NUT", -39600, 0},
	{"NZDT", 46800, 1},
	{"NZST", 43200, 0},
	{"NZT", 43200, 0},
	{"OMSST", 25200, 1},
	{"OMST", 21600, 0},
	{"PDT", -25200, 1},
	{"PET", -18000, 0},
	{"PETST", 46800, 1},
	{"PETT", 43200, 0},
	{"PGT", 36000, 0},
	{"PHOT", 46800, 0},
	{"PHT", 28800, 0},
	{"PKT", 18000, 0},
	{"PMDT", -7200, 1},
	{"PMST", -10800, 0},
	{"PONT", 39600, 0},
	{"PST", -28800, 0},
	{"PWT", 32400, 0},
	{"PYST", -10800, 1},
	{"PYT", -14400, 0},
	{"RET", 14400, 0},
	{"SADT", 37800, 1},
	{"SAST", 34200, 0},
	{"SCT", 14400, 0},
	{"TAHT", -36000, 0},
	{"TFT", 18000, 0},
	{"TJT", 18000, 0},
	{"TKT", -36000, 0},
	{"TMT", 18000, 0},
	{"TOT", 46800, 0},
	{"TRUT", 36000, 0},
	{"TVT", 43200, 0},
	{"UCT", 0, 0},
	{"ULAST", 32400, 1},
	{"ULAT", 28800, 0},
	{"UT", 0, 0},
	{"UTC", 0, 0},
	{"UYST", -7200, 1},
	{"UYT", -10800, 0},
	{"UZST", 21600, 1},
	{"UZT", 18000, 0},
	{"VET", -14400, 0},
	{"VLAST", 39600, 1},
	{"VLAT", 36000, 0},
	{"VUT", 39600, 0},
	{"WADT", 28800, 1},
	{"WAKT", 43200, 0},
	{"WAST", 25200, 0},
	{"WAT", 3600, 0},
	{"WDT", 32400, 1},
	{"WET", 0, 0},
	{"WETDST", 3600, 1},
	{"WFT", 43200, 0},
	{"WGST", -7200, 1},
	{"WGT", -10800, 0},
	{"YAKST", 36000, 1},
	{"YAKT", 32400, 0},
	{"YAPT", 36000, 0},
	{"YEKST", 21600, 1},
	{"YEKT", 18000, 0},
	{"Z", 0, 0},
	{"ZULU", 0, 0}
};

static void
tzabbr2info(const char *tzabbr, int *gmtoff, int *isdst)
{
	int i=0;

	*gmtoff = 0;
	*isdst = -1;

	for (i=0; i < countof(tzmap); i++)
	{
		if (strcmp(tzabbr, tzmap[i].abbrev)==0)
		{
			*gmtoff = tzmap[i].gmtoff;
			*isdst = (int) tzmap[i].isdst;
			break;
		}
	}
}


#ifdef PQT_WINAPI
/* ##############################################################
	TIME ZONE MAPPING

	Windows doesn't use any abbreviated time zone names, like
	EST for Eastern Standard Time.	The below list was required
	to offer these abbreviations.

	This list was compiled from:

		http://www.date-time-zone.com/

	which is the same list used by Java.	It is a mapping from the windows
	registry: "HKLM\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Time Zones"
	to the Olson Timezone Database:

		http://www.twinsun.com/tz/tz-link.htm

	NOTE: The below two time zones had a different display name in the
	windows registry.	The list was adjusted.

		Signapore Standard Time - Malay Peninsula Standard Time
		Israel Standard Time		- Jerusalem Standard Time
*/

static struct {
	const char *abbr;
	const char *fullname;
} _tzmap[] = {
  {"EST", "AUS Eastern Standard Time"             },
  {"EST", "AUS Eastern Daylight Time"             },
  {"AFT", "Afghanistan Standard Time"             },
  {"AKST", "Alaskan Standard Time"                },
  {"AKDT", "Alaskan Daylight Time"                },
  {"AST", "Arab Standard Time"                    },
  {"GST", "Arabian Standard Time"                 },
  {"AST", "Arabic Standard Time"                  },
  {"ADT", "Arabic Daylight Time"                  },
  {"AST", "Atlantic Standard Time"                },
  {"ADT", "Atlantic Daylight Time"                },
  {"AZOT", "Azores Standard Time"                 },
  {"AZOST", "Azores Daylight Time"                },
  {"CST", "Canada Central Standard Time"          },
  {"MDT", "Canada Central Daylight Time"          },
  {"CVT", "Cape Verde Standard Time"              },
  {"CVST", "Cape Verde Daylight Time"             },
  {"GET", "Caucasus Standard Time"                },
  {"GEST", "Caucasus Daylight Time"               },
  {"CST", "Cen. Australia Standard Time"          },
  {"CST", "Cen. Australia Daylight Time"          },
  {"CST", "Central Standard Time"                 },
  {"CDT", "Central Daylight Time"                 },
  {"CST", "Central Standard Time (Mexico)"        },
  {"CDT", "Central Daylight Time (Mexico)"        },
  {"CST", "Central America Standard Time"         },
  {"CDT", "Central America Daylight Time"         },
  {"BDT", "Central Asia Standard Time"            },
  {"CET", "Central Europe Standard Time"          },
  {"CEST", "Central Europe Daylight Time"         },
  {"CET", "Central European Standard Time"        },
  {"CEST", "Central European Daylight Time"       },
  {"MAGT", "Central Pacific Standard Time"        },
  {"MAGST", "Central Pacific Daylight Time"       },
  {"CST", "China Standard Time"                   },
  {"CDT", "China Daylight Time"                   },
  {"GMT+12", "Dateline Standard Time"             },
  {"EAT", "E. Africa Standard Time"               },
  {"EST", "E. Australia Standard Time"            },
  {"EST", "E. Australia Daylight Time"            },
  {"EET", "E. Europe Standard Time"               },
  {"EEST", "E. Europe Daylight Time"              },
  {"BRT", "E. South America Standard Time"        },
  {"BRST", "E. South America Daylight Time"       },
  {"EST", "Eastern Standard Time"                 },
  {"EDT", "Eastern Daylight Time"                 },
  {"EET", "Egypt Standard Time"                   },
  {"EEST", "Egypt Daylight Time"                  },
  {"YEKT", "Ekaterinburg Standard Time"           },
  {"YEKST", "Ekaterinburg Daylight Time"          },
  {"EET", "FLE Standard Time"                     },
  {"EEST", "FLE Daylight Time"                    },
  {"FJT", "Fiji Standard Time"                    },
  {"FJST", "Fiji Daylight Time"                   },
  {"GMT", "GMT Standard Time"                     },
  {"BST", "GMT Daylight Time"                     },
  {"EET", "GTB Standard Time"                     },
  {"EEST", "GTB Daylight Time"                    },
  {"WGT", "Greenland Standard Time"               },
  {"WGST", "Greenland Daylight Time"              },
  {"WET", "Greenwich Standard Time"               },
  {"WEST", "Greenwich Daylight Time"              },
  {"HST", "Hawaiian Standard Time"                },
  {"HPT", "Hawaiian Daylight Time"                },
  {"IST", "India Standard Time"                   },
  {"IST", "India Daylight Time"                   },
  {"IRST", "Iran Standard Time"                   },
  {"IRDT", "Iran Daylight Time"                   },
  {"IST", "Jerusalem Standard Time"               },
  {"IDT", "Jerusalem Daylight Time"               },
  {"KST", "Korea Standard Time"                   },
  {"KDT", "Korea Daylight Time"                   },
  {"CST", "Mexico Standard Time"                  },
  {"CDT", "Mexico Daylight Time"                  },
  {"MST", "Mexico Standard Time 2",               },
  {"MDT", "Mexico Daylight Time 2",               },
  {"FNT", "Mid-Atlantic Standard Time"            },
  {"FNST", "Mid-Atlantic Daylight Time"           },
  {"MST", "Mountain Standard Time"                },
  {"MDT", "Mountain Daylight Time"                },
  {"MST", "Mountain Standard Time (Mexico)"       },
  {"MDT", "Mountain Daylight Time (Mexico)"       },
  {"MMT", "Myanmar Standard Time"                 },
  {"NOVT", "N. Central Asia Standard Time"        },
  {"NOVST", "N. Central Asia Daylight Time"       },
  {"NPT", "Nepal Standard Time"                   },
  {"NZST", "New Zealand Standard Time"            },
  {"NZDT", "New Zealand Daylight Time"            },
  {"NST", "Newfoundland Standard Time"            },
  {"NDT", "Newfoundland Daylight Time"            },
  {"KRAT", "North Asia Standard Time"             },
  {"KRAST", "North Asia Daylight Time"            },
  {"IRKT", "North Asia East Standard Time"        },
  {"IRKST", "North Asia East Daylight Time"       },
  {"PST", "Pacific Standard Time"                 },
  {"PDT", "Pacific Daylight Time"                 },
  {"CLT", "Pacific SA Standard Time"              },
  {"CLST", "Pacific SA Daylight Time"             },
  {"CET", "Romance Standard Time"                 },
  {"CEST", "Romance Daylight Time"                },
  {"MSK", "Russian Standard Time"                 },
  {"MSD", "Russian Daylight Time"                 },
  {"ART", "SA Eastern Standard Time"              },
  {"ARST", "SA Eastern Daylight Time"             },
  {"COT", "SA Pacific Standard Time"              },
  {"COST", "SA Pacific Daylight Time"             },
  {"VET", "SA Western Standard Time"              },
  {"ICT", "SE Asia Standard Time"                 },
  {"WST", "Samoa Standard Time"                   },
  {"SGT", "Malay Peninsula Standard Time"         },
  {"MALST", "Malay Peninsula Daylight Time"       },
  {"SAST", "South Africa Standard Time"           },
  {"SAST", "South Africa Daylight Time"           },
  {"IST", "Sri Lanka Standard Time"               },
  {"IST", "Sri Lanka Daylight Time"               },
  {"CST", "Taipei Standard Time"                  },
  {"CDT", "Taipei Daylight Time"                  },
  {"EST", "Tasmania Standard Time"                },
  {"EST", "Tasmania Daylight Time"                },
  {"JST", "Tokyo Standard Time"                   },
  {"JDT", "Tokyo Daylight Time"                   },
  {"TOT", "Tonga Standard Time"                   },
  {"TOST", "Tonga Daylight Time"                  },
  {"EST", "US Eastern Standard Time"              },
  {"EDT", "US Eastern Daylight Time"              },
  {"MST", "US Mountain Standard Time"             },
  {"MDT", "US Mountain Daylight Time"             },
  {"VLAT", "Vladivostok Standard Time"            },
  {"VLAST", "Vladivostok Daylight Time"           },
  {"WST", "W. Australia Standard Time"            },
  {"WST", "W. Australia Daylight Time"            },
  {"WAT", "W. Central Africa Standard Time"       },
  {"CET", "W. Europe Standard Time"               },
  {"CEST", "W. Europe Daylight Time"              },
  {"PKT", "West Asia Standard Time"               },
  {"PKST", "West Asia Daylight Time"              },
  {"ChST", "West Pacific Standard Time"           },
  {"YAKT", "Yakutsk Standard Time"                },
  {"YAKST", "Yakutsk Daylight Time"               }
};

static char *
_win32_tzabbr(const char *fullname)
{
	int i;
	static char empty_string[1] = {0};

	for (i=0; i < countof(tzmap); i++)
		if (pqt_strcasecmp(fullname, _tzmap[i].fullname)==0)
			return (char *) _tzmap[i].abbr;

	return empty_string;
}

#endif

