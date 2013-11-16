
/*
 * exec.c
 *   Query execution and data retrieval functions.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

/*
 * Each param value requires an oid, ptr and 2 ints (oid, value, length,
 * format). This makes the maximum size of a param 20 bytes. A stack size
 * of 4k would allow for 204 query params.	If more params are needed,
 * heap memory is used. Needing more than 204 param values in a query
 * is very rare, although possible.
 */
#define PARAM_STACKSIZE 4096

#define BUILD_ARRAYS(rettype) \
	rettype r; \
	char *buf   = NULL; \
	Oid *oids   = NULL; \
	char **vals = NULL; \
	int *lens   = NULL; \
	int *fmts   = NULL; \
	int vcnt    = 0; \
	char stackbuffer[PARAM_STACKSIZE]; \
\
	PQseterror(NULL); \
	if (!conn) \
	{ \
		PQseterror("PGconn cannot be NULL"); \
		return (rettype)(0); \
	} \
\
	if (param) \
	{ \
		buf = stackbuffer; \
		if (!buildArrays(param, &buf, &oids, &vals, &lens, &fmts)) \
			return (rettype) (0); \
		vcnt = param->vcnt; \
	}

#define RETURN_RESULT \
	if (param) \
	{ \
		if (buf && buf != stackbuffer) \
			free(buf); \
	} \
	return r

static int
buildArrays(PGparam *param, char **buf, Oid **oids,
	char ***vals, int **lens, int **fmts);

static PGresult *
copyExecError(PGconn *conn, PGresult *r);

static const char *
getCommand(PGconn *conn, PGparam *param, const char *command);

static int
_execvf(PGconn *conn, const char *cmdspec, va_list ap, PGresult **resp);

int
PQgetf(const PGresult *res, int tup_num, const char *format, ...)
{
	int n;
	va_list ap;

	va_start(ap, format);
	n = PQgetvf(res, tup_num, format, ap);
	va_end(ap);

	return n;
}

int
PQgetvf(const PGresult *res, int tup_num, const char *format, va_list ap)
{
	int r;
	PGtypeHandler *h;
	PGtypeArgs args;
	int typpos = 0;
	int flags;
	Oid ftype;
	PGtypeData *resData;
	PGtypeSpec *spec = NULL;
	char tmp[200];

	PQseterror(NULL);

	if (!res)
	{
		PQseterror("PGresult cannot be NULL");
		return FALSE;
	}

	if (!(resData = (PGtypeData *) PQresultInstanceData(res, pqt_eventproc)))
	{
		PQseterror("PGresult at %p has no event data", res);
		return FALSE;
	}

	va_copy(args.ap, ap);

	/* "@name" format, lookup typeSpec in cache */
	if(format && *format == '@')
	{
		spec = pqt_getspec(resData->typspecs, resData->typspeccnt, format + 1);

		/* If we didn't find a type spec, this is an error.  A format string
		 * with a '@' as its first character is reserved.
		 */
		if (!spec)
		{
			PQseterror("No such prepared specifier name: '%s'", format + 1);
			va_end(args.ap);
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

			h = pqt_gethandlerbyid(resData->typhandlers, resData->typhcnt,
				spec->idlist[typpos]);

			/* should be an unusual, or a "will never happen", situation */
			if (!h)
			{
				va_end(args.ap);
				PQseterror("Unknown type handler id at position %d", typpos+1);
				return FALSE;
			}

			flags = (int) spec->flags[typpos];
			typpos++;
		}
		else
		{
			format = pqt_parse(format, resData->typhandlers, resData->typhcnt,
				NULL, 0, &h, NULL, &typpos, &flags);

			if (!format)
			{
				va_end(args.ap);
				return FALSE;
			}

			if (!h)
				continue;
		}

		if (flags & TYPFLAG_BYNAME)
			args.get.field_num = PQfnumber(res, va_arg(args.ap, const char *));
		else
			args.get.field_num = va_arg(args.ap, int);

		/* simplify life for handlers by checking getvalue's return here. */
		if (args.get.field_num < 0 ||
			!PQgetvalue(res, tup_num, args.get.field_num))
		{
			PQseterror(
				"Invalid tup_num[%d].field_num[%d] (position %d)",
				tup_num, args.get.field_num, typpos);
			va_end(args.ap);
			return FALSE;
		}

		ftype = PQftype(res, args.get.field_num);
		if (((flags & TYPFLAG_ARRAY) && ftype != h->typoid_array) ||
			 (!(flags & TYPFLAG_ARRAY) && ftype != h->typoid))
		{
			Oid oid = (flags & TYPFLAG_ARRAY) ? h->typoid_array : h->typoid;
			PQseterror(
				"Trying to get type %u '%s' but server returned %u (position %d)",
				oid, pqt_fqtn(tmp, sizeof(tmp), h->typschema, h->typname),
				ftype, typpos);
			va_end(args.ap);
			return FALSE;
		}

		args.is_put       = 0;
		args.get.result   = (PGresult *) res;
		args.format       = PQfformat(res, args.get.field_num);
		args.fmtinfo      = &resData->fmtinfo;
		args.get.tup_num  = tup_num;
		args.is_ptr       = (flags & TYPFLAG_POINTER) ? 1 : 0;
		args.typpos       = typpos;
		args.typhandler   = h;
		args.errorf       = pqt_argserrorf;
		args.super        = pqt_argssuper;

		if (flags & TYPFLAG_ARRAY)
			r = pqt_get_array(&args);
		else
			r = h->typget(&args);

		if (r == -1)
		{
			va_end(args.ap);
			return FALSE;
		}
	}

	va_end(args.ap);

	return TRUE;
}

/* --------------------------------
 * Exec and Send functions
 */

PGresult *
PQexecf(PGconn *conn, const char *cmdspec, ...)
{
	va_list ap;
	PGresult *res;

	va_start(ap, cmdspec);
	res = PQexecvf(conn, cmdspec, ap);
	va_end(ap);

	return res;
}

PGresult *
PQexecvf(PGconn *conn, const char *cmdspec, va_list ap)
{
	PGresult *res;
	(void) _execvf(conn, cmdspec, ap, &res);
	return res;
}

int
PQsendf(PGconn *conn, const char *cmdspec, ...)
{
	int n;
	va_list ap;

	va_start(ap, cmdspec);
	n = PQsendvf(conn, cmdspec, ap);
	va_end(ap);

	return n;
}

int
PQsendvf(PGconn *conn, const char *cmdspec, va_list ap)
{
	return _execvf(conn, cmdspec, ap, NULL);
}

PGresult *
PQparamExec(PGconn *conn, PGparam *param, const char *command,
	int resultFormat)
{
	BUILD_ARRAYS(PGresult *);

	command = getCommand(conn, param, command);
	if (!command)
	{
		r = NULL;
	}
	else
	{
		r = PQexecParams(conn, command, vcnt, oids,
			(const char *const * ) vals, lens, fmts, resultFormat);

		pqt_setresultfields(r);
		r = copyExecError(conn, r);
	}

	RETURN_RESULT;
}

int
PQparamSendQuery(PGconn *conn, PGparam *param, const char *command,
	int resultFormat)
{
	BUILD_ARRAYS(int);

	command = getCommand(conn, param, command);
	if (!command)
	{
		r = FALSE;
	}
	else
	{
		r = PQsendQueryParams(conn, command, vcnt, oids,
			(const char *const * ) vals, lens, fmts, resultFormat);

		if (!r)
			PQseterror("PGconn: %s", PQerrorMessage(conn));
	}

	RETURN_RESULT;
}

PGresult *
PQparamExecPrepared(PGconn *conn, PGparam *param, const char *stmtName,
	int resultFormat)
{
	BUILD_ARRAYS(PGresult *);

	r = PQexecPrepared(conn, stmtName, vcnt, (const char *const * ) vals,
		lens, fmts, resultFormat);

	pqt_setresultfields(r);
	r = copyExecError(conn, r);

	RETURN_RESULT;
}

int
PQparamSendQueryPrepared(PGconn *conn, PGparam *param, const char *stmtName,
	int resultFormat)
{
	BUILD_ARRAYS(int);

	r = PQsendQueryPrepared(conn, stmtName, vcnt,
		(const char *const * ) vals, lens, fmts, resultFormat);

	if (!r)
		PQseterror("PGconn: %s", PQerrorMessage(conn));

	RETURN_RESULT;
}

/* Called by PQexecvf and PQsendvf.  When resp is NULL, PQparamSendQuery
 * is used to execute the command.  Otherwise, PQparamExec is used.  The
 * return value is always zero when resp is not NULL.  When resp is NULL,
 * the return value is zero for error and non-zero for success (identical
 * to PQparamSendQuery).
 */
static int
_execvf(PGconn *conn, const char *cmdspec, va_list ap, PGresult **resp)
{
	int retval = 0;
	size_t stmt_len=0;
	char buffer[8192]; /* could be larger these days but be conservative */
	char *stmt = NULL;
	PGparam *param = NULL;

	if (resp)
		*resp = NULL;

	if(!conn)
	{
		PQseterror("PGconn cannot be NULL");
		return FALSE;
	}

	if(!cmdspec || !*cmdspec)
	{
		PQseterror("cmdspec cannot be NULL or an empty string");
		return FALSE;
	}

	/* No stmt buf required for preapred type specs */
	if (*cmdspec != '@')
	{
		/* The resulting parameterized command is guarenteed to be smaller
		 * than the cmdspec.  When needed, enlarge stmt buf to cmdspec length.
		 */
		stmt_len = strlen(cmdspec) + 1;

		/* stack buffer is too small, use heap */
		if (stmt_len > sizeof(buffer))
		{
			if (!(stmt = (char *) malloc(stmt_len)))
			{
				PQseterror(PQT_OUTOFMEMORY);
				return FALSE;
			}
		}
		else
		{
			stmt = buffer;
			stmt_len = sizeof(buffer);
		}
	}

	if ((param = PQparamCreate(conn)))
	{
		if (PQputvf(param, stmt, stmt_len, cmdspec, ap))
		{
			const char *s = stmt ? stmt : cmdspec;

			if (resp)
				*resp = PQparamExec(conn, param, s, 1);
			else
				retval = PQparamSendQuery(conn, param, s, 1);
		}

		PQparamClear(param);
	}

	if (stmt && stmt != buffer)
		free(stmt);

	return retval;
}

/*
 * Assigns param values to param arrays, for use with postgres
 * parameter API. 'buf' is expected to be PARAM_STACKSIZE bytes.  If more
 * memory is required, memory is allocated and assigned to *buf, which
 * must be freed by caller.  To determine if *buf was allocated, compare
 * its address to the initially provided stack address.
 * Returns 1 on success and 0 on error.
 */
static int
buildArrays(PGparam *param, char **buf, Oid **oids,
	char ***vals, int **lens, int **fmts)
{
	int n;

	/* no params to assign */
	if (param->vcnt == 0)
		return 1;

	/* required memory size for the 4 param arrays */
	n = (int) ((sizeof(void *) * param->vcnt) + /* values */
			((sizeof(int) * 2) * param->vcnt) +	    /* lengths and formats */
			(sizeof(Oid) * param->vcnt));				    /* oids */

	/* required memory is too large for stack buffer, get some heap */
	if (n > PARAM_STACKSIZE)
	{
		char *p;

		if (!(p = (char *) malloc(n)))
		{
			PQseterror(PQT_OUTOFMEMORY);
			return 0;
		}

		*buf = p;
	}

	/* give arrays memory from buffer, which could be stack or heap. */
	*vals = (char **) *buf;
	*lens = (int *) (*buf + (sizeof(void *) * param->vcnt));
	*fmts = (*lens) + param->vcnt;
	*oids = (Oid *) ((*fmts) + param->vcnt);

	/* loop param values and assign value, length, format
	 * and oid to arrays.
	 */
	for (n=0; n < param->vcnt; n++)
	{
		(*oids)[n] = param->vals[n].oid;
		(*vals)[n] = param->vals[n].data;
		(*lens)[n] = param->vals[n].datal;
		(*fmts)[n] = param->vals[n].format;
	}

	return 1;
}

static PGresult *
copyExecError(PGconn *conn, PGresult *r)
{
	if (!r)
	{
		PQseterror("PGconn: %s", PQerrorMessage(conn));
		return NULL;
	}

	switch (PQresultStatus(r))
	{
		case PGRES_COMMAND_OK:
		case PGRES_TUPLES_OK:
		case PGRES_EMPTY_QUERY:
			break;

		default:
		{
			PQseterror("PGresult: %s", PQresultErrorMessage(r));
			PQclear(r);
			r = NULL;
			break;
		}
	}

	return r;
}

/* Using the param is preferred when both conn and param are provided.
 * The conn is there in case the exec has no parameters, NULL param.
 */
static const char *
getCommand(PGconn *conn, PGparam *param, const char *command)
{
	PGtypeSpec *spec;
	int typspeccnt = 0;
	PGtypeSpec *typspecs = NULL;

	if (!command)
	{
		PQseterror("command to execute cannot be NULL");
		return NULL;
	}

	if (*command != '@')
		return command;

	if (param)
	{
		typspecs = param->typspecs;
		typspeccnt = param->typspeccnt;
	}

	/* Try to get instance data from the conn */
	if (!typspecs || typspeccnt == 0)
	{
		PGtypeData *data = PQinstanceData(conn, pqt_eventproc);

		if (!data)
		{
			PQseterror("PGconn at %p has no event data", conn);
			return NULL;
		}

		typspecs = data->typspecs;
		typspeccnt = data->typspeccnt;
	}

	spec = pqt_getspec(typspecs, typspeccnt, command + 1);

	/* If we didn't find a type spec, this is an error.  A format string
	 * with an '@' as its first character is reserved.
	 */
	if (!spec)
	{
		PQseterror("No such prepared specifier name: '%s'", command + 1);
		return NULL;
	}

	/* make sure type spec was prepared with a statement */
	if (!spec->stmt || !*spec->stmt)
	{
		PQseterror("Prepared specifier name '%s' has no statement", command + 1);
		return NULL;
	}

	return (const char *) spec->stmt;
}


