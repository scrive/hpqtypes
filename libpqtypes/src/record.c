
/*
 * record.c
 *   Type handler for the record/composite data type.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

int
pqt_put_record(PGtypeArgs *args)
{
	int i;
	int len;
	char *out;
	PGparam *param = va_arg(args->ap, PGparam *);

	PUTNULLCHK(args, param);

	/* watch for invalidation issues */
	if (param->vcnt > args->typhandler->nattrs)
		return args->errorf(args,
			"param value count is %d but server says it's %d",
			param->vcnt, args->typhandler->nattrs);

	/* Auto-fill the remaining fields with SQL NULLs.  This feature was
	 * added because it was needed (by eSilo).  We had a few cases where
	 * we needed to append new fields to existing composites but wanted
	 * to maintain backwards compatibility ... so some_func(mycomposite)
	 * would continue to work even for older versions unaware of the
	 * new composite fields.  I guess for some this is unwanted behavior,
	 * but I think the cases for it are much more common.
	 */
	if (param->vcnt < args->typhandler->nattrs)
	{
		int nattrs = args->typhandler->nattrs - param->vcnt;
		for (i=0; i < nattrs; i++)
			pqt_putparam(param, NULL, 0, 0, BINARYFMT,
				args->typhandler->attDescs[param->vcnt].attoid);
	}

	/* column count, 4-byte integer */
	len = 4;

	/* determine total byte count to ensure args->put.out is large enough */
	for (i=0; i < param->vcnt; i++)
	{
		len += (4 + 4); /* oid + len */

		if (param->vals[i].datal > 0)
			len += param->vals[i].datal;
	}

	/* ensure out buffer is large enough */
	if (args->put.expandBuffer(args, len) == -1)
		return -1;

	out = args->put.out;

	/* write column count */
	pqt_buf_putint4(out, param->vcnt);
	out += 4;

	for (i=0; i < param->vcnt; i++)
	{
		if (param->vals[i].format == 0)
			return args->errorf(args,
				"Cannot put composite attributes in text format");

		if (param->vals[i].datal == NULL_LEN)
			param->vals[i].oid = args->typhandler->attDescs[i].attoid;

		/* watch for invalidation issues */
		if (param->vals[i].oid != args->typhandler->attDescs[i].attoid)
			return args->errorf(args,
				"param value OID is %u but server says it's %u",
				param->vals[i].oid, args->typhandler->attDescs[i].attoid);

		/* write column oid */
		pqt_buf_putint4(out, param->vals[i].oid);
		out += 4;

		/* write column data length */
		pqt_buf_putint4(out, param->vals[i].datal);
		out += 4;

		/* write the column data */
		if (param->vals[i].data && param->vals[i].datal > 0)
		{
			memcpy(out, param->vals[i].data, param->vals[i].datal);
			out += param->vals[i].datal;
		}
	}

	return len;
}

int
pqt_get_record(PGtypeArgs *args)
{
	int i;
	int nattrs;
	int vlen;
	Oid server_oid;
	DECLVALUE(args);
	PGresult *res;
	PGresult **resultp = va_arg(args->ap, PGresult **);

	CHKGETVALS(args, resultp);

	if (args->format == TEXTFMT)
		return args->errorf(args, "record does not support text results");

	/* get record column count, numAttributes */
	nattrs = pqt_buf_getint4(value);
	value += 4;

	/* watch for invalidation issues */
	if (args->typhandler->nattrs != nattrs)
		return args->errorf(args,
			"type handler attribute count is %d but server says it's %d",
			args->typhandler->nattrs, nattrs);

	if (!(res = pqt_copyresult(args, nattrs)))
		RERR_MEM(args);

	for (i=0; i < nattrs; i++)
	{
		/* watch for invalidation issues */
		server_oid = (Oid) pqt_buf_getint4(value);
		if (server_oid != args->typhandler->attDescs[i].attoid)
		{
			args->errorf(args,
				"type handler attribute OID is %u but server says it's %u",
				args->typhandler->attDescs[i].attoid, server_oid);
			PQclear(res);
			return -1;
		}

		/* move past Oid */
		value += 4;

		/* get value length */
		vlen = pqt_buf_getint4(value);
		value += 4;

		if (!PQsetvalue(res, 0, i, value, vlen))
		{
			PQclear(res);
			return -1;
		}

		if (vlen > 0)
			value += vlen;
	}

	*resultp = res;
	return 0;
}

