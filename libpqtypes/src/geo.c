
/*
 * geo.c
 *   Type handler for the geometric data types.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

static int text2point(PGpoint *pt, char *text, char **endptr);
static int text2points(PGtypeArgs *args, PGpoint **pts, int *npts);
static int bin2points(PGtypeArgs *args, char *valp, int ptcnt,
	PGpoint **pts, int *npts);
static int putpoints(PGtypeArgs *args, int npts, PGpoint *pts,
	int is_path, int closed);

int
pqt_put_point(PGtypeArgs *args)
{
	unsigned int *buf;
	PGpoint *pt = va_arg(args->ap, PGpoint *);

	PUTNULLCHK(args, pt);

	buf = (unsigned int *) args->put.out;
	pqt_swap8(buf,	   &pt->x, 1);
	pqt_swap8(buf + 2, &pt->y, 1);
	return 16;
}

int
pqt_get_point(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGpoint *pt = va_arg(args->ap, PGpoint *);

	CHKGETVALS(args, pt);

	if (args->format == TEXTFMT)
	{
		if (!text2point(pt, value, NULL))
			RERR_STR2INT(args);

		return 0;
	}

	pqt_swap8(&pt->x, (unsigned int *) value, 0);
	pqt_swap8(&pt->y, ((unsigned int *) (value)) + 2, 0);
	return 0;
}

int
pqt_put_lseg(PGtypeArgs *args)
{
	unsigned int *buf;
	PGlseg *lseg = va_arg(args->ap, PGlseg *);

	PUTNULLCHK(args, lseg);

	buf = (unsigned int *) args->put.out;
	pqt_swap8(buf,     &lseg->pts[0].x, 1);
	pqt_swap8(buf + 2, &lseg->pts[0].y, 1);
	pqt_swap8(buf + 4, &lseg->pts[1].x, 1);
	pqt_swap8(buf + 6, &lseg->pts[1].y, 1);
	return 32;
}

int
pqt_get_lseg(PGtypeArgs *args)
{
	DECLVALUE(args);
	unsigned int *v;
	PGlseg *lseg = va_arg(args->ap, PGlseg *);

	CHKGETVALS(args, lseg);

	if (args->format == TEXTFMT)
	{
		PGpoint *pts = (PGpoint *)lseg;

		if (*value++ != '[' ||
			 !text2point(pts, value, &value) ||
			 *value++ != ',' ||
			 !text2point(pts + 1, value, &value) ||
			 *value != ']')
			RERR_STR2INT(args);

		return 0;
	}

	v = (unsigned int *) value;
	pqt_swap8(&lseg->pts[0].x, v, 0);
	pqt_swap8(&lseg->pts[0].y, v + 2, 0);
	pqt_swap8(&lseg->pts[1].x, v + 4, 0);
	pqt_swap8(&lseg->pts[1].y, v + 6, 0);
	return 0;
}

int
pqt_put_box(PGtypeArgs *args)
{
	unsigned int *buf;
	PGbox *box = va_arg(args->ap, PGbox *);

	PUTNULLCHK(args, box);

	buf = (unsigned int *) args->put.out;
	pqt_swap8(buf,     &box->high.x, 1);
	pqt_swap8(buf + 2, &box->high.y, 1);
	pqt_swap8(buf + 4, &box->low.x,  1);
	pqt_swap8(buf + 6, &box->low.y,  1);
	return 32;
}

int
pqt_get_box(PGtypeArgs *args)
{
	DECLVALUE(args);
	unsigned int *v;
	PGbox *box = va_arg(args->ap, PGbox *);

	CHKGETVALS(args, box);

	if (args->format == TEXTFMT)
	{
		PGpoint *pts = (PGpoint *)box;

		if (!text2point(pts, value, &value) ||
			 *value++ != ',' ||
			 !text2point(pts + 1, value, NULL))
			RERR_STR2INT(args);

		return 0;
	}

	v = (unsigned int *) value;
	pqt_swap8(&box->high.x, v, 0);
	pqt_swap8(&box->high.y, v + 2, 0);
	pqt_swap8(&box->low.x,  v + 4, 0);
	pqt_swap8(&box->low.y,  v + 6, 0);
	return 0;
}

int
pqt_put_circle(PGtypeArgs *args)
{
	unsigned int *buf;
	PGcircle *circle = va_arg(args->ap, PGcircle *);

	PUTNULLCHK(args, circle);

	buf = (unsigned int *) args->put.out;
	pqt_swap8(buf,     &circle->center.x, 1);
	pqt_swap8(buf + 2, &circle->center.y, 1);
	pqt_swap8(buf + 4, &circle->radius,   1);
	return 24;
}

int
pqt_get_circle(PGtypeArgs *args)
{
	DECLVALUE(args);
	unsigned int *v;
	PGcircle *circle = va_arg(args->ap, PGcircle *);

	CHKGETVALS(args, circle);

	if (args->format == TEXTFMT)
	{
		if (*value++ != '<' ||
			 !text2point((PGpoint *)circle, value, &value) ||
			 *value++ != ',' ||
			 !pqt_text_to_float8(&circle->radius, value, &value) ||
			 *value != '>')
			RERR_STR2INT(args);

		return 0;
	}

	v = (unsigned int *) value;
	pqt_swap8(&circle->center.x, v, 0);
	pqt_swap8(&circle->center.y, v + 2, 0);
	pqt_swap8(&circle->radius,   v + 4, 0);
	return 0;
}

int
pqt_put_path(PGtypeArgs *args)
{
	PGpath *path = va_arg(args->ap, PGpath *);
	PUTNULLCHK(args, path);
	return putpoints(args, path->npts, path->pts, 1, path->closed ? 1 : 0);
}

int
pqt_get_path(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGpath *path = va_arg(args->ap, PGpath *);

	CHKGETVALS(args, path);

	if (args->format == TEXTFMT)
	{
		path->closed = *value == '(' ? 1 : 0;
		return text2points(args, &path->pts, &path->npts);
	}

	path->closed = *value ? 1 : 0;
	value++;

	return bin2points(args,
		value + sizeof(int), /* beginning of point array */
		pqt_buf_getint4(value),  /* number of points */
		&path->pts, &path->npts);
}

int
pqt_put_polygon(PGtypeArgs *args)
{
	PGpolygon *polygon = va_arg(args->ap, PGpolygon *);
	PUTNULLCHK(args, polygon);
	return putpoints(args, polygon->npts, polygon->pts, 0, 0);
}

int
pqt_get_polygon(PGtypeArgs *args)
{
	DECLVALUE(args);
	PGpolygon *polygon = va_arg(args->ap, PGpolygon *);

	CHKGETVALS(args, polygon);

	if (args->format == TEXTFMT)
		return text2points(args, &polygon->pts, &polygon->npts);

	return bin2points(args,
		value + sizeof(int), /* beginning of point array */
		pqt_buf_getint4(value),  /* number of points */
		&polygon->pts,
		&polygon->npts);
}


static int
putpoints(PGtypeArgs *args, int npts, PGpoint *pts,
	int is_path, int closed)
{
	int i;
	int datal;
	int hdr = (int) sizeof(int);
	char *out;

	/* pts is for a path, include 1 byte open/closed flag */
	if (is_path)
		hdr++;

	/* length of binary formated path */
	datal = (npts * sizeof(PGpoint)) + hdr;

	/* make sure out is large enough */
	if (args->put.expandBuffer(args, datal) == -1)
		return -1;

	out = args->put.out;
	if (is_path)
		*out++ = closed ? 1 : 0; /* path open/closed flag */

	/* write the number of points as an int32 */
	pqt_buf_putint4(out, npts);
	out += 4;

	/* assign points to the data 'out' buffer */
	for (i=0; i < npts; i++)
	{
		pqt_swap8(out, &pts[i].x, 1);
		out += sizeof(double);

		pqt_swap8(out, &pts[i].y, 1);
		out += sizeof(double);
	}

	return datal;
}

static int
text2points(PGtypeArgs *args, PGpoint **pts, int *npts)
{
	DECLVALUE(args);
	char *s;
	int cnt = 0;
	PGpoint *p = NULL;

	*pts = NULL;
	*npts = 0;

	if (*value != '(' && *value != '[')
		RERR(args, "malformed point array");

	/* get the number of points by counting the '(' */
	for (s=value+1; *s; s++)
	{
		if (*s == '(')
		{
			if (!(s = strchr(s, ')'))) /* skip point contents */
				break;
			cnt++;
		}
	}

	if (cnt == 0)
		return 0; /* empty point list */

	p = (PGpoint *) PQresultAlloc((PGresult *) args->get.result,
		cnt * sizeof(PGpoint));
	if (!p)
		RERR_MEM(args);

	for (cnt=0; *++value; )
	{
		if (!text2point(&p[cnt++], value, &value))
			RERR_STR2INT(args);

		/* done */
		if (*value != ',')
			break;
	}

	*pts = p;
	*npts = cnt;
	return 0;
}

static int
bin2points(PGtypeArgs *args, char *valp, int ptcnt,
	PGpoint **pts, int *npts)
{
	int i;
	PGpoint *p;

	*pts = NULL;
	*npts = 0;

	if (ptcnt == 0)
		return 0;

	p = (PGpoint *) PQresultAlloc((PGresult *) args->get.result,
		ptcnt * sizeof(PGpoint));

	if (!p)
		RERR_MEM(args);

	for (i=0; i < ptcnt; i++)
	{
		pqt_swap8(&p[i].x, valp, 0);
		valp += sizeof(double);

		pqt_swap8(&p[i].y, valp, 0);
		valp += sizeof(double);
	}

	*pts = p;
	*npts = ptcnt;
	return 0;
}

static int
text2point(PGpoint *pt, char *text, char **endptr)
{
	if (*text++ != '(')
		return 0;

	if (!pqt_text_to_float8(&pt->x, text, &text) || *text++ != ',')
		return 0;

	if (!pqt_text_to_float8(&pt->y, text, &text) || *text++ != ')')
		return 0;

	if (endptr)
		*endptr = text;
	return 1;
}





