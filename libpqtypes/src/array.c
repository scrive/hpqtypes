
/*
 * array.c
 *   Type handler for the array data type.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

int
pqt_put_array(PGtypeArgs *args)
{
	int i;
	int hasnull=0;
	int ndims;
	int nitems;
	int arrsize;
	char *out;
	int lbound[MAXDIM];
	int dims[MAXDIM];
	PGarray *arr = va_arg(args->ap, PGarray *);

	PUTNULLCHK(args, arr);

	if (arr->ndims < 0)
		return args->errorf(args, "arr.ndims is invalid - %d", arr->ndims);

  /* auto configure when ndims is 0 to 1d array */
	if (arr->ndims == 0)
	{
		ndims = 1;
		dims[0] = arr->param->vcnt;
		lbound[0] = 1;
	}
	else
	{
		ndims = arr->ndims;
		memcpy(lbound, arr->lbound, sizeof(lbound));
		memcpy(dims, arr->dims, sizeof(dims));
	}

	nitems = 1;
	for (i=0; i < ndims; i++)
		nitems *= dims[i];

	/* make sure array is on the same page as the param */
	if (nitems != arr->param->vcnt)
		return args->errorf(args,
			"param element count %d is different than array's %d",
			arr->param->vcnt, nitems);

	/* header: ndims + hasnull + elemtype + ((dims + lbound) * ndims) */
	arrsize = 4 + 4 + 4 + (8 * ndims);

	/* compute data length, also  get the hasnull flag */
	for (i=0; i < arr->param->vcnt; i++)
	{
		if (arr->param->vals[i].format == 0)
			return args->errorf(args, "Cannot put array elements in text format");

		arrsize += 4;
		if (arr->param->vals[i].datal == NULL_LEN)
			hasnull = 1;
		else
			arrsize += arr->param->vals[i].datal;
	}

	/* make sure args->put.out is large enough */
	if (args->put.expandBuffer(args, arrsize) == -1)
		return -1;

	out = args->put.out;

	/* number od dimensions */
	pqt_buf_putint4(out, ndims);
	out += 4;

	/* array hasnull flag */
	pqt_buf_putint4(out, hasnull);
	out += 4;

	/* array element oid */
	pqt_buf_putint4(out, args->typhandler->typoid);
	out += 4;

	/* dims and lbound */
	for (i=0; i < ndims; i++)
	{
		pqt_buf_putint4(out, dims[i]);
		out += 4;

		pqt_buf_putint4(out, lbound[i]);
		out += 4;
	}

	/* write the element lengths and data */
	for (i=0; i < arr->param->vcnt; i++)
	{
		pqt_buf_putint4(out, arr->param->vals[i].datal);
		out += 4;

		if (arr->param->vals[i].datal > 0)
		{
			memcpy(out, arr->param->vals[i].data, arr->param->vals[i].datal);
			out += arr->param->vals[i].datal;
		}
	}

	return arrsize;
}

int
pqt_get_array(PGtypeArgs *args)
{
	int i,t;
	int vlen;
	int ntups;
	int nattrs;
	Oid elemoid;
	DECLVALUE(args);
	PGresult *res;
	int first_tup;
	PGarray *arr = va_arg(args->ap, PGarray *);

	CHKGETVALS(args, arr);

	if (args->format == TEXTFMT)
		return args->errorf(args, "array does not support text results");

	/* number of dims */
	arr->ndims = pqt_buf_getint4(value);
	value += 4;

	/* skip NULL flag */
	value += 4;

	/* check the element oid */
	elemoid = (Oid)pqt_buf_getint4(value);
	if (elemoid != args->typhandler->typoid)
		return args->errorf(args,
			"array element type %u is different than what server says %u",
			args->typhandler->typoid, elemoid);
	value += 4;

	/* arr dims and lbounds */
	first_tup = 1;
	for (i=0, ntups=1; i < arr->ndims; i++)
	{
		arr->dims[i] = pqt_buf_getint4(value);
		value += 4;

		arr->lbound[i] = pqt_buf_getint4(value);
		value += 4;

		ntups *= arr->dims[i];
	}

	/* This means ndims is zero because the above loop never iterated. */
	if (i == 0)
		ntups = 0;

	/* numTuples is the number of array items
	 * and numAttributes is 1 for non-composites.
	 */
	nattrs = (args->typhandler->nattrs > 0) ? args->typhandler->nattrs : 1;

	if (!(res = pqt_copyresult(args, nattrs)))
		RERR_MEM(args);

	for (t=0; t < ntups; t++)
	{
		/* get the value len */
		vlen = pqt_buf_getint4(value);
		value += 4;

		/* Regular Array with 1 attr per tuple */
		if (args->typhandler->nattrs == 0)
		{
			/* set the field value */
			if (!PQsetvalue(res, t, 0, value, vlen))
			{
				PQclear(res);
				return -1;
			}

			if (vlen > 0)
				value += vlen;

			continue;
		}

		/* ------------------------
		 * COMPOSITE/RECORD
		 */

		/* NULL compsoite array item, fill in attrs with NULL */
		if (vlen == NULL_LEN)
		{
			int x;

			for (x=0; x < nattrs; x++)
			{
				if (!PQsetvalue(res, t, x, NULL, NULL_LEN))
				{
					PQclear(res);
					return -1;
				}
			}

			/* move on to next tuple, done here. */
			continue;
		}

		/* verify that server's attr count matches ours */
		if (first_tup)
		{
			int attcnt = pqt_buf_getint4(value);

			/* watch for invalidation issues */
			if (attcnt != nattrs)
			{
				PQclear(res);
				return args->errorf(args,
					"type handler attribute count is %d but server says it's %d",
					args->typhandler->nattrs, attcnt);
			}
		}

		/* skip attr count */
		value += 4;

		/* composite attributes (record columns) */
		for (i=0; i < nattrs; i++)
		{
			/* watch for invalidation issues */
			if (first_tup &&
				 (Oid) pqt_buf_getint4(value) != args->typhandler->attDescs[i].attoid)
			{
				Oid server_oid = (Oid) pqt_buf_getint4(value);

				args->errorf(args,
					"type handler attribute OID is %u but server says it's %u",
					args->typhandler->attDescs[i].attoid, server_oid);

				PQclear(res);
				return -1;
			}

			/* skip oid */
			value += 4;

			/* get the value length */
			vlen = pqt_buf_getint4(value);
			value += 4;

			/* set the field value */
			if (!PQsetvalue(res, t, i, value, vlen))
			{
				PQclear(res);
				return -1;
			}

			if (vlen > 0)
				value += vlen;
		}

		first_tup = 0;
	}

	arr->res = res;
	return 0;
}


