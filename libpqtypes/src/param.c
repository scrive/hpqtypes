
/*
 * param.c
 *   Management functions for the PGparam object.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

static int argsExpandBuffer(PGtypeArgs *args, int new_len);

PGparam *
PQparamCreate(const PGconn *conn)
{
	PGparam *param;
	PGtypeData *connData;

	PQseterror(NULL);

	if (!conn)
	{
		PQseterror("PGconn cannot be NULL");
		return NULL;
	}

	param = (PGparam *) malloc(sizeof(PGparam));
	if (!param)
	{
		PQseterror(PQT_OUTOFMEMORY);
		return NULL;
	}

	memset(param, 0, sizeof(PGparam));

	connData = (PGtypeData *) PQinstanceData(conn, pqt_eventproc);
	if (!connData)
	{
		PQparamClear(param);
		PQseterror("No type data exists for PGconn at %p", conn);
		return NULL;
	}

	/* copy any handlers from conn object */
	if (connData->typhcnt > 0)
	{
		param->typhandlers = pqt_duphandlers(
			connData->typhandlers, connData->typhcnt);

		if (!param->typhandlers)
		{
			PQparamClear(param);
			PQseterror(PQT_OUTOFMEMORY);
			return NULL;
		}

		param->typhcnt = connData->typhcnt;
	}

	/* copy any typespecs from conn object */
	if (connData->typspeccnt > 0)
	{
		param->typspecs = pqt_dupspecs(
			connData->typspecs, connData->typspeccnt);

		if (!param->typspecs)
		{
			PQparamClear(param);
			PQseterror(PQT_OUTOFMEMORY);
			return NULL;
		}

		param->typspeccnt = connData->typspeccnt;
	}

	pqt_getfmtinfo(conn, &param->fmtinfo);
	return param;
}

PQT_EXPORT PGparam *
PQparamDup(PGparam *param)
{
	PGparam *new_param;

	PQseterror(NULL);

	if (!param)
	{
		PQseterror("PGparam to duplicate cannot be NULL");
		return NULL;
	}

	new_param = (PGparam *) malloc(sizeof(PGparam));
	if (!new_param)
	{
		PQseterror(PQT_OUTOFMEMORY);
		return NULL;
	}

	memset(new_param, 0, sizeof(PGparam));

	/* copy any handlers from conn object */
	if (param->typhcnt > 0)
	{
		new_param->typhandlers = pqt_duphandlers(
			param->typhandlers, param->typhcnt);

		if (!new_param->typhandlers)
		{
			PQparamClear(new_param);
			PQseterror(PQT_OUTOFMEMORY);
			return NULL;
		}

		new_param->typhcnt = param->typhcnt;
	}

	/* copy any typespecs from conn object */
	if (param->typspeccnt > 0)
	{
		new_param->typspecs = pqt_dupspecs(
			param->typspecs, param->typspeccnt);

		if (!new_param->typspecs)
		{
			PQparamClear(new_param);
			PQseterror(PQT_OUTOFMEMORY);
			return NULL;
		}

		new_param->typspeccnt = param->typspeccnt;
	}

	memcpy(&new_param->fmtinfo, &param->fmtinfo, sizeof(PGtypeFormatInfo));

	/* copy any values, don't bother if array is empty. */
	if (param->vcnt > 0)
	{
		int i;

		for (i=0; i < param->vcnt; i++)
		{
			int flags = 0;
			PGvalue *val = &param->vals[i];

			/* Is val->data is direct user pointer?  If so, set pointer flag
			 * so pqt_putparam doesn't deep copy it.
			 */
			if (val->ptr != val->data)
				flags |= TYPFLAG_POINTER;

			if (!pqt_putparam(new_param, val->data, val->datal, flags,
				val->format, val->oid))
			{
				PQparamClear(new_param);
				return NULL;
			}
		}
	}

	return new_param;
}

int
PQparamCount(PGparam *param)
{
	if (param)
		return param->vcnt;
	return 0;
}

void
PQparamReset(PGparam *param)
{
	if (param)
		param->vcnt = 0;
}

void
PQparamClear(PGparam *param)
{
	int i;

	if (!param)
		return;

	/* vmax is the size of the vals array.  We don't use vcnt because
	 * someone could of just called PQparamReset, leaving vcnt 0.  If
	 * we find any value pointers that are not NULL, free'm.
	 */
	for (i=0; i < param->vmax; i++)
		if (param->vals[i].ptr)
			free(param->vals[i].ptr);

	if (param->vals)
		free(param->vals);

	pqt_freehandlers(param->typhandlers, param->typhcnt);
	pqt_freespecs(param->typspecs, param->typspeccnt);

	param->vmax = 0;
	param->vcnt = 0;
	param->vals = NULL;
	param->typhcnt = 0;
	param->typhandlers = NULL;
	param->typspeccnt = 0;
	param->typspecs = NULL;

	free(param);
}

int
PQputf(PGparam *param, const char *format, ...)
{
	int r;
	va_list ap;

	/* This function is just a wrapper to PQputvf */
	va_start(ap, format);
	r = PQputvf(param, NULL, 0, format, ap);
	va_end(ap);

	return r;
}

int
PQputvf(PGparam *param, char *stmtBuf, size_t stmtBufLen,
	const char *format, va_list ap)
{
	int n=0;
	int flags;
	size_t stmtPos = 0;
	int save_vcnt;
	int typpos = 0;
	PGtypeHandler *h;
	PGtypeArgs args;
	char args_outbuf[4096];
	PGtypeSpec *spec = NULL;

	PQseterror(NULL);

	if (!param)
	{
		PQseterror("PGparam cannot be NULL");
		return FALSE;
	}

	if (!format || !*format)
	{
		PQseterror("param 'format' cannot be NULL or an empty string");
		return FALSE;
	}

	if (stmtBuf && stmtBufLen < 1)
	{
		PQseterror("Invalid argument: stmtBufLen must be >= 1");
		return FALSE;
	}

	save_vcnt = param->vcnt;
	va_copy(args.ap, ap);

	/* "@name" format, lookup typeSpec in cache */
	if(format && *format == '@')
	{

		spec = pqt_getspec(param->typspecs, param->typspeccnt, format + 1);

		/* If we didn't find a type spec, this is an error.  A format string
		 * with a '@' as its first character is reserved.
		 */
		if (!spec)
		{
			PQseterror("No such prepared specifier name: '%s'", format + 1);
			return FALSE;
		}
	}

	while (format && *format)
	{
		if (spec)
		{
			/* done, no more handlers in cached spec string. */
			if (typpos == spec->idcnt)
				break;

			h = pqt_gethandlerbyid(param->typhandlers, param->typhcnt,
				spec->idlist[typpos]);

			/* should be an unusual, or a "will never happen", situation */
			if (!h)
			{
				va_end(args.ap);
				PQseterror("Unknown type handler id at position %d", typpos+1);
				param->vcnt = save_vcnt;
				return FALSE;
			}

			flags = (int) spec->flags[typpos];
			typpos++;
		}
		else
		{
			format = pqt_parse(format, param->typhandlers, param->typhcnt,
				stmtBuf, stmtBufLen, &h, &stmtPos, &typpos, &flags);

			if (!format)
			{
				param->vcnt = save_vcnt;
				return FALSE;
			}

			if (!h)
				continue;
		}

		args.is_put           = 1;
		args.put.param        = param;
		args.fmtinfo          = &param->fmtinfo;
		args.put.out          = args_outbuf;
		args.put.__allocated_out = NULL;
		args.put.outl         = (int) sizeof(args_outbuf);
		args.is_ptr           = (flags & TYPFLAG_POINTER) ? 1 : 0;
		args.format           = BINARYFMT;
		args.put.expandBuffer = argsExpandBuffer;
		args.typpos           = typpos;
		args.typhandler       = h;
		args.errorf           = pqt_argserrorf;
		args.super            = pqt_argssuper;
		*args.put.out         = 0;

		if (flags & TYPFLAG_ARRAY)
			n = pqt_put_array(&args);
		else
			n = h->typput(&args);

		if (n == -1)
		{
			if (args.put.__allocated_out && args.put.__allocated_out != args_outbuf)
				free(args.put.__allocated_out);
			param->vcnt = save_vcnt;
			return FALSE;
		}

		if (args.put.out == NULL)
		{
			args.format = BINARYFMT;
			n = -1;
		}

		n = pqt_putparam(param, args.put.out, n, flags, args.format,
			(flags & TYPFLAG_ARRAY) ? h->typoid_array : h->typoid);

		if (args.put.__allocated_out && args.put.__allocated_out != args_outbuf)
			free(args.put.__allocated_out);

		if (!n)
		{
			param->vcnt = save_vcnt;
			return FALSE;
		}
	}

	if (stmtBuf)
		stmtBuf[stmtPos] = 0;

	return TRUE;
}


/* ----------------------------
 * Helper functions
 */

int
pqt_putparam(PGparam *param, const void *data, int datal,
	int flags, int format, Oid typoid)
{
	PGvalue *v;

	if (!param)
		return FALSE;

	if (!data)
		datal = -1;

	/* need to grow param vals array */
	if (param->vcnt == param->vmax)
	{
		PGvalue *vals;
		int vmax = param->vmax ? (param->vmax * 3) / 2 : 16;

		vals = (PGvalue *) pqt_realloc(param->vals, sizeof(PGvalue) * vmax);
		if (!vals)
		{
			PQseterror(PQT_OUTOFMEMORY);
			return FALSE;
		}

		/* zero out the new array elements */
		memset(vals + param->vcnt, 0, (vmax - param->vcnt) * sizeof(PGvalue));
		param->vmax = vmax;
		param->vals = vals;
	}

	/* grab next param value */
	v = &param->vals[param->vcnt];

	if (datal == -1)
	{
		v->data = NULL;
	}
	/* wants to put a direct pointer */
	else if (flags & TYPFLAG_POINTER)
	{
		v->data = (char *) data;
	}
	else
	{
		/* need more mem for param value ptr */
		if (v->ptrl < datal)
		{
			char *ptr = (char *) pqt_realloc(v->ptr, datal);

			if (!ptr)
			{
				PQseterror(PQT_OUTOFMEMORY);
				return FALSE;
			}

			v->ptrl = datal;
			v->ptr = ptr;
		}

		memcpy(v->ptr, data, datal);
		v->data = v->ptr;
	}

	v->datal	= datal;
	v->format = format;
	v->oid		= typoid;
	param->vcnt++;
	return TRUE;
}

/* returns 0 on success and -1 on error */
static int
argsExpandBuffer(PGtypeArgs *args, int new_len)
{
	char *new_out;

	if (new_len <= args->put.outl)
		return 0;

	if (!args->put.__allocated_out)
	{
		new_out = (char *) malloc(new_len);
		if (!new_out)
			return args->errorf(args, PQT_OUTOFMEMORY);

		/* fully copy existing stack buffer, we don't know what data
		 * the user has already written and may want to keep.
		 */
		memcpy(new_out, args->put.out, args->put.outl);
	}
	else
	{
		new_out = (char *) realloc(args->put.__allocated_out, new_len);
		if (!new_out)
			return args->errorf(args, PQT_OUTOFMEMORY);
	}

	args->put.out = args->put.__allocated_out = new_out;
	args->put.outl = new_len;
	return 0;
}





