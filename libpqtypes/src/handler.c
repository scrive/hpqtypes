
/*
 * handler.c
 *   Type handler management functions, the core of the handler system.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

/* -------------------------
 * Built-in handler defaults:
 *   base_id, nattrs, freeAttDescs, attDescsBuf, attDescs
 */
#define __HANDLER_DEFAULTS__ -1, 0, 0, {{0}}, NULL

/* id, schema, typname, typlen, oid, arroid, typput, typget, defaults */
static PGtypeHandler pg_handlers[] = {
	/* character types */
	{0, "pg_catalog", "char", 1, CHAROID, 1002, pqt_put_char,
		pqt_get_char, __HANDLER_DEFAULTS__},
	{1, "pg_catalog", "varchar", -1, VARCHAROID, 1015, pqt_put_text,
		pqt_get_text, __HANDLER_DEFAULTS__}, /* supports ptr */
	{2, "pg_catalog", "bpchar", -1, BPCHAROID, 1014, pqt_put_text,
		pqt_get_text, __HANDLER_DEFAULTS__}, /* supports ptr */
	{3, "pg_catalog", "text", -1, TEXTOID, 1009, pqt_put_text,
		pqt_get_text, __HANDLER_DEFAULTS__}, /* supports ptr */

	/* boolean types */
	{4, "pg_catalog", "bool", 1, BOOLOID, 1000, pqt_put_bool,
		pqt_get_bool, __HANDLER_DEFAULTS__},

	/* numeric types */
	{5, "pg_catalog", "int2", 2, INT2OID, 1005, pqt_put_int2,
		pqt_get_int2, __HANDLER_DEFAULTS__},
	{6, "pg_catalog", "int4", 4, INT4OID, 1007, pqt_put_int4,
		pqt_get_int4, __HANDLER_DEFAULTS__},
	{7, "pg_catalog", "int8", 8, INT8OID, 1016, pqt_put_int8,
		pqt_get_int8, __HANDLER_DEFAULTS__},
	{8, "pg_catalog", "float4", 4, FLOAT4OID, 1021, pqt_put_float4,
		pqt_get_float4, __HANDLER_DEFAULTS__},
	{9, "pg_catalog", "float8", 8, FLOAT8OID, 1022, pqt_put_float8,
		pqt_get_float8, __HANDLER_DEFAULTS__},
	{10, "pg_catalog", "numeric", -1, NUMERICOID, 1231, pqt_put_numeric,
		pqt_get_numeric, __HANDLER_DEFAULTS__},

	/* bytea types */
	{11, "pg_catalog", "bytea", -1, BYTEAOID, 1001, pqt_put_bytea,
		pqt_get_bytea, __HANDLER_DEFAULTS__}, /* supports ptr */

	/* geometric types */
	{12, "pg_catalog", "point", 16, POINTOID, 1017, pqt_put_point,
		pqt_get_point, __HANDLER_DEFAULTS__},
	{13, "pg_catalog", "lseg", 32, LSEGOID, 1018, pqt_put_lseg,
		pqt_get_lseg, __HANDLER_DEFAULTS__},
	{14, "pg_catalog", "box", 32, BOXOID, 1020, pqt_put_box,
		pqt_get_box, __HANDLER_DEFAULTS__},
	{15, "pg_catalog", "circle", 24, CIRCLEOID, 719, pqt_put_circle,
		pqt_get_circle, __HANDLER_DEFAULTS__},
	{16, "pg_catalog", "path", -1, PATHOID, 1019, pqt_put_path,
		pqt_get_path, __HANDLER_DEFAULTS__},
	{17, "pg_catalog", "polygon", -1, POLYGONOID, 1027, pqt_put_polygon,
		pqt_get_polygon, __HANDLER_DEFAULTS__},

	/* monetary types */
	{18, "pg_catalog", "money", 8, CASHOID, 791, pqt_put_money,
		pqt_get_money, __HANDLER_DEFAULTS__},

	/* network address typess */
	{19, "pg_catalog", "inet", -1, INETOID, 1041, pqt_put_inet,
		pqt_get_inet, __HANDLER_DEFAULTS__},
	{20, "pg_catalog", "cidr", -1, CIDROID, 651, pqt_put_inet,
		pqt_get_cidr, __HANDLER_DEFAULTS__},
	{21, "pg_catalog", "macaddr", 6, MACADDROID, 1040, pqt_put_macaddr,
		pqt_get_macaddr, __HANDLER_DEFAULTS__},

	/* date & time types */
	{22, "pg_catalog", "time", 8, TIMEOID, 1183, pqt_put_time,
		pqt_get_time, __HANDLER_DEFAULTS__},
	{23, "pg_catalog", "timetz", 12, TIMETZOID, 1270, pqt_put_timetz,
		pqt_get_timetz, __HANDLER_DEFAULTS__},
	{24, "pg_catalog", "date", 4, DATEOID, 1182, pqt_put_date,
		pqt_get_date, __HANDLER_DEFAULTS__},
	{25, "pg_catalog", "timestamp", 8, TIMESTAMPOID, 1115, pqt_put_timestamp,
		pqt_get_timestamp, __HANDLER_DEFAULTS__},
	{26, "pg_catalog", "timestamptz", 8, TIMESTAMPTZOID, 1185,
		pqt_put_timestamptz, pqt_get_timestamptz, __HANDLER_DEFAULTS__},
	{27, "pg_catalog", "interval", 16, INTERVALOID, 1187, pqt_put_interval,
		pqt_get_interval, __HANDLER_DEFAULTS__},

	/* object identifier types */
	{28, "pg_catalog", "oid", 4, OIDOID, 1028, pqt_put_int4,
		pqt_get_int4, __HANDLER_DEFAULTS__},

	/* UUID types */
	{29, "pg_catalog", "uuid", 16, UUIDOID, 2951, pqt_put_uuid,
		pqt_get_uuid, __HANDLER_DEFAULTS__},

	/* Record types (composites) */
	{30, "pg_catalog", "record", -1, RECORDOID, 0, pqt_put_record,
		pqt_get_record, __HANDLER_DEFAULTS__},

	/* pqt types */
	{31, "pqt", "str", -1, InvalidOid, 0, pqt_put_str,
		NULL, __HANDLER_DEFAULTS__}, /* supports ptr */
	{32, "pqt", "null", -1, InvalidOid, 0, pqt_put_null,
		NULL, __HANDLER_DEFAULTS__},

	/* more character types */
	{33, "pg_catalog", "name", -1, NAMEOID, 1003, pqt_put_text,
		pqt_get_text, __HANDLER_DEFAULTS__}, /* supports ptr */

	/* text passed as bytea to avoid double copy with ByteStringS
	 * and silent truncation of strings containting NULL characters */
	{34, "pg_catalog", "btext", -1, TEXTOID, 1009, pqt_put_bytea,
		pqt_get_bytea, __HANDLER_DEFAULTS__}
};

static int
expandHandlers(PGtypeData *typeData, PGerror *err);

static PGrecordAttDesc *
initAttDescs(PGerror *err, PGtypeHandler *h, char *attrs);

static char *
parseType(PGerror *err, const char *spec, char *schema, char *typname, int typpos);

static int
getTypeParams(PGconn *conn, PGerror *err, PGregisterType *types, int count,
	PGarray *names, PGarray *schemas);

static int
checkTypeLookups(PGresult *res, PGerror *err, PGregisterType *types, int count);

/* If res is NULL, non-blocking send is used, otherwise a blocking
 * exec is issued and *res contains the result.  Returns zero on
 * error and non-zero on success.
 */
static int
performRegisterQuery(PGconn *conn, PGerror *err, int which, PGregisterType *types,
	int count, PGresult **res);

/* Called by PQregisterTypes() for each type provided. */
static int
registerSubClass(PGtypeData *connData, PGerror *err, const char *type_name,
	PGtypeProc typput, PGtypeProc typget);

int
PQinitTypes(PGconn *conn)
{
	return PQregisterEventProc(conn, pqt_eventproc, "pqtypes", NULL);
}

/* Deprecated */
int
PQregisterSubClasses(PGconn *conn, PGerror *err, PGregisterType *types, int count)
{
	return PQregisterTypes(conn, err, PQT_SUBCLASS, types, count, 0);
}

/* Deprecated */
int
PQregisterUserDefinedTypes(PGconn *conn, PGerror *err, PGregisterType *types, int count)
{
	return PQregisterTypes(conn, err, PQT_USERDEFINED, types, count, 0);
}

/* Deprecated */
int
PQregisterComposites(PGconn *conn, PGerror *err, PGregisterType *types, int count)
{
	return PQregisterTypes(conn, err, PQT_COMPOSITE, types, count, 0);
}

int
PQregisterTypes(PGconn *conn, PGerror *err, int which, PGregisterType *types,
	int count, int async)
{
	int n = FALSE;

	if (!conn)
	{
		PQseterror(err, "PGconn cannot be NULL");
		return FALSE;
	}

	if (!types)
	{
		PQseterror(err, "PGregisterType[] cannot be NULL");
		return FALSE;
	}

	if (count < 0)
	{
		PQseterror(err, "PGregisterType[] count cannot be less than zero");
		return FALSE;
	}

	/* nothing to do, silently ignore it */
	if (count == 0)
		return TRUE;

	if (which == PQT_SUBCLASS)
	{
		int i;
		PGtypeData *connData;

		if (!(connData = (PGtypeData *) PQinstanceData(conn, pqt_eventproc)))
		{
			PQseterror(err, "PGconn is missing event data");
			return FALSE;
		}

		for (i=0; i < count; i++)
		{
			n = registerSubClass(connData, err, types[i].typname,
				types[i].typput, types[i].typget);

			if (!n)
				return FALSE;
		}
	}
	else
	{
		PGresult *res = NULL;

		n = performRegisterQuery(conn, err, which, types, count, async ? NULL : &res);

		/* If not async, register the result and clear it. */
		if (n && !async)
		{
			n = PQregisterResult(conn, err, which, types, count, res);
			PQclear(res);
		}
	}

	return n;
}

int
PQregisterResult(PGconn *conn, PGerror *err, int which, PGregisterType *types,
	int count, PGresult *res)
{
	int i;
	PGtypeData *connData;
	char typname[PQT_MAXIDLEN + 1];
	char typschema[PQT_MAXIDLEN + 1];
	/* inherit typput and typget from record type */
	PGtypeHandler *h_rec = pqt_gethandler(NULL, 0, "pg_catalog", "record");

	if (!conn)
	{
		PQseterror(err, "PGconn cannot be NULL");
		return FALSE;
	}

	if (!res)
	{
		PQseterror(err, "PGresult cannot be NULL");
		return FALSE;
	}

	if (which == PQT_SUBCLASS)
	{
		PQseterror(err, "Cannot call PQregisterResult for a subclass registration.");
		return FALSE;
	}

	if (!(connData = (PGtypeData *) PQinstanceData(conn, pqt_eventproc)))
	{
		PQseterror(err, "PGconn is missing event data");
		return FALSE;
	}

	if (!types)
	{
		PQseterror(err, "PGregisterType[] cannot be NULL");
		return FALSE;
	}

	if (count < 0)
	{
		PQseterror(err, "PGregisterType[] count cannot be less than zero");
		return FALSE;
	}

	if(!checkTypeLookups(res, err, types, count))
		return FALSE;

	for (i=0; i < PQntuples(res); i++)
	{
		int flags;
		PGint2 typlen;
		PGtypeHandler *h;

		if (which == PQT_USERDEFINED && !types[i].typput && !types[i].typget)
		{
			PQseterror(err, "Must provide a put and/or a get routine: '%s'",
				types[i].typname);
			return FALSE;
		}

		/* make sure conn's type handlers array is large enough */
		if (!expandHandlers(connData, err))
			return FALSE;

		/* create the handler */
		h = &connData->typhandlers[connData->typhcnt];
		memset(h, 0, sizeof(PGtypeHandler));

		if (!PQgetf(res, err, i, "%oid %oid %int2", 1, &h->typoid,
			2, &h->typoid_array, 3, &typlen))
		{
			return FALSE;
		}

		h->id = connData->typhcnt + countof(pg_handlers);
		h->typlen = (int) typlen;
		h->base_id = -1;

		if (which == PQT_USERDEFINED)
		{
			h->typput = types[i].typput;
			h->typget = types[i].typget;
		}
		else
		{
			h->typput = h_rec->typput;
			h->typget = h_rec->typget;
		}

		/* parse out type and schema names again */
		(void ) pqt_parsetype(err, types[i].typname, typschema, typname, &flags, 1);
		pqt_strcpy(h->typschema, sizeof(h->typschema), typschema);
		pqt_strcpy(h->typname, sizeof(h->typname), typname);

		/* Process composite attributes */
		if(which == PQT_COMPOSITE)
		{
			PGtext attrs;
			int nattrs;
			PGrecordAttDesc *attDescs;

			if (!PQgetf(res, err, i, "%text", 4, &attrs))
			{
				return FALSE;
			}

			if (!(attDescs = initAttDescs(err, h, attrs)))
				return FALSE;

			for (nattrs=0; *attrs; nattrs++)
			{
				char *p;
				char *name;
				int len;

				/* Attribute Text Encoding:
				 *   "attoid,attlen,atttypmod,name_hex attoid,etc..."
				 */

				attDescs[nattrs].attoid    = (int) strtol(attrs, &attrs, 10);
				attDescs[nattrs].attlen    = (int) strtol(attrs + 1, &attrs, 10);
				attDescs[nattrs].atttypmod = (int) strtol(attrs + 1, &attrs, 10);

				/* skip comma before name */
				attrs++;

				/* attribute name in hex */
				if (!(p = strchr(attrs, ' ')))
					p = attrs + strlen(attrs); /* last attr, point at NUL */

				/* truncate name if it exceeds buffer */
				len = (int) (p - attrs);
				if (len >= (int) sizeof(attDescs[nattrs].attname))
					len = (int) (sizeof(attDescs[nattrs].attname) - 1);

				/* hex decode and copy */
				for (name = attDescs[nattrs].attname; attrs < p; attrs += 2)
					*name++ = (char) (pqt_hex_to_dec(attrs[0]) << 4)
						| pqt_hex_to_dec(attrs[1]);
				*name = 0;
			}

			h->nattrs = nattrs;
			h->attDescs = attDescs;
		}

		connData->typhcnt++;
	}

	return TRUE;
}

int
PQclearTypes(PGconn *conn, PGerror *err)
{
	PGtypeData *connData;

	if (!conn)
	{
		PQseterror(err, "PGconn cannot be NULL");
		return FALSE;
	}

	if (!(connData = (PGtypeData *) PQinstanceData(conn, pqt_eventproc)))
	{
		PQseterror(err, "PGconn is missing event data");
		return FALSE;
	}

	pqt_cleartypes(connData);

	return TRUE;
}

/* Do not call when hcnt is 0.  This returns NULL when malloc fails.
 * Passing in 0 could only do the same thing (ambiguos), so it is simply
 * not handled and may dump core.
 */
PGtypeHandler *
pqt_duphandlers(PGtypeHandler *handlers, int hcnt)
{
	int i;
	PGtypeHandler *h = (PGtypeHandler *) malloc(hcnt * sizeof(PGtypeHandler));

	if (!h)
		return NULL;

	/* In the most common cases, this is the total cost of the dup.
	 * Previously, the handler had 4 inner strings that required a deep
	 * copy.  Surprisingly, this had a noticeable overhead.  This was
	 * solved by using fixed length buffers in the type handler struct.
	 * Also, a fixed length attDescs buffer was added to avoid having
	 * to perform a deep copy for the common cases, 16 or less attrs.
	 */
	memcpy(h, handlers, hcnt * sizeof(PGtypeHandler));

	/* Possibly deep copy PGrecordAttDesc array.  Otherwise, assign it
	 * to the fixed length buffer.
	 */
	for (i=0; i < hcnt; i++)
	{
		if (h[i].nattrs == 0)
			continue;

		/* There are attributes but the attDescs buffer can be used rather
		 * than allocating and copying.  The data was copied during the
		 * handlers memcpy prior to this loop.
		 */
		if (!h[i].freeAttDescs)
		{
			h[i].attDescs = h[i].attDescsBuf;
			continue;
		}

		/* ------------------------------
		 * Must allocate and copy.
		 */

		h[i].attDescs = (PGrecordAttDesc *) malloc(
			h[i].nattrs * sizeof(PGrecordAttDesc));

		if (!h[i].attDescs)
		{
			pqt_freehandlers(h, i+1);
			return NULL;
		}

		memcpy(h[i].attDescs, handlers[i].attDescs,
			h[i].nattrs * sizeof(PGrecordAttDesc));
	}

	return h;
}

void
pqt_cleartypes(PGtypeData *typeData)
{
	if (typeData)
	{
		pqt_freehandlers(typeData->typhandlers, typeData->typhcnt);

		typeData->typhandlers = NULL;
		typeData->typhcnt = 0;
		typeData->typhmax = 0;
	}
}

void
pqt_freehandlers(PGtypeHandler *handlers, int hcnt)
{
	int i;

	/* Free attDescs */
	for (i=0; i < hcnt; i++)
		if (handlers[i].freeAttDescs && handlers[i].attDescs)
			free(handlers[i].attDescs);

	if (handlers)
		free(handlers);
}

PGtypeHandler *
pqt_gethandler(PGtypeHandler *handlers, int hcnt,
	const char *schema, const char *typname)
{
	int i;
	int noschema = !schema || !*schema;

	if (!typname || !*typname)
		return NULL;

	/* user registered types are searched first */
	for (i=0; i < hcnt; i++)
	{
		if ((noschema || strcmp(handlers[i].typschema, schema)==0) &&
			 strcmp(handlers[i].typname, typname)==0)
		{
			return &handlers[i];
		}
	}

	/* builtin types searched last */
	for (i=0; i < countof(pg_handlers); i++)
	{
		if ((noschema || strcmp(pg_handlers[i].typschema, schema)==0) &&
			 strcmp(pg_handlers[i].typname, typname)==0)
		{
			return &pg_handlers[i];
		}
	}

	return NULL;
}

PGtypeHandler *
pqt_gethandlerbyid(PGtypeHandler *handlers, int hcnt, int id)
{
	if (id <= -1)
		return NULL;

	if (id < countof(pg_handlers))
		return &pg_handlers[id];

	id -= countof(pg_handlers);
	if (id >= hcnt)
		return NULL;

	return &handlers[id];
}

int
pqt_argssuper(PGtypeArgs *args, ...)
{
	int r;
	va_list ap;
	PGtypeHandler *baseclass;
	PGtypeHandler *subclass = args->typhandler;
	PGtypeData *resData = NULL;

	if (!args->is_put)
	{
		resData = (PGtypeData *) PQresultInstanceData(
			args->get.result, pqt_eventproc);

		if (!resData)
			return args->errorf(args, "PGresult is missing event data");
	}

	/* should always work, but play it safe */
	baseclass = pqt_gethandlerbyid(
		args->is_put ? args->put.param->typhandlers : resData->typhandlers,
		args->is_put ? args->put.param->typhcnt : resData->typhcnt,
		subclass->base_id);

	if (!baseclass)
		return args->errorf(args, "type handler has no base type");

	args->typhandler = baseclass;

	va_copy(ap, args->ap);
	va_start(args->ap, args);
	r = args->is_put ? baseclass->typput(args) : baseclass->typget(args);
	va_copy(args->ap, ap);

	args->typhandler = subclass;
	return r;
}

/* FQTN standards for Fully Qualified Type Name.  Returns a pointer to out.
 * Only returns NULL if out is NULL or outl <= 0.
 */
char *
pqt_fqtn(char *out, size_t outl, const char *schema, const char *typname)
{
	int r;
	int have_schema = schema && *schema;

	if (!out || outl<=0)
		return NULL;

	*out = 0;
	if (!typname || !*typname)
		return out;

	r = pqt_snprintf(out, outl, "%s%s%s", have_schema ? schema : "",
		have_schema ? "." : "", typname);

	if (r < 0)
	{
		*out = 0;
		return out;
	}

	return out;
}

/* only checks builin types or pqt types.  User registered types must
 * be checked by the user's handler functions.
 */
int
pqt_allowsptr(PGtypeHandler *h)
{
	/* pg_catalog.[bpchar, varchar, name, text, bytea] */
	if (strcmp(h->typschema, "pg_catalog")==0)
	{
		if (strcmp(h->typname, "bpchar") &&
			 strcmp(h->typname, "varchar") &&
			 strcmp(h->typname, "text") &&
			 strcmp(h->typname, "bytea") &&
			 strcmp(h->typname, "name"))
		{
			return FALSE;
		}
	}
	/* pqt.str */
	else if (strcmp(h->typschema, "pqt")==0 && strcmp(h->typname, "str"))
	{
		return FALSE;
	}

	return TRUE;
}

void
pqt_getfmtinfo(const PGconn *conn, PGtypeFormatInfo *info)
{
	const char *value;

	memset(info, 0, sizeof(PGtypeFormatInfo));

	if ((value = PQparameterStatus(conn, "DateStyle")))
		pqt_strcpy(info->datestyle, sizeof(info->datestyle), value);

	if ((value = PQparameterStatus(conn, "integer_datetimes")))
		info->integer_datetimes = strcmp(value, "on")==0 ? TRUE : FALSE;

	info->sversion = PQserverVersion(conn);
	info->pversion = PQprotocolVersion(conn);
}

static int registerSubClass(PGtypeData *connData, PGerror *err, const char *type_name,
	PGtypeProc typput, PGtypeProc typget)
{
	char *s;
	PGtypeHandler *h_sub;
	PGtypeHandler *h_base;
	char sub_typschema[PQT_MAXIDLEN + 1];
	char sub_typname[PQT_MAXIDLEN + 1];
	char base_typschema[PQT_MAXIDLEN + 1];
	char base_typname[PQT_MAXIDLEN + 1];
	char sub_fqtn[200];
	char base_fqtn[200];

	if (!(s = parseType(err, type_name, sub_typschema, sub_typname, 1)))
		return FALSE;

	if (*s != '=')
	{
		PQseterror(err, "Missing inheritence operator '=': %s", type_name);
		return FALSE;
	}

	if (!parseType(err, s + 1, base_typschema, base_typname, 1))
		return FALSE;

	/* lookup the base handler */
	h_base = pqt_gethandler(connData->typhandlers, connData->typhcnt,
		base_typschema, base_typname);

	if (!h_base)
	{
		PQseterror(err, "typname '%s' does not exist, '%s' cannot sub-class it",
			pqt_fqtn(base_fqtn, sizeof(base_fqtn), base_typschema, base_typname),
			pqt_fqtn(sub_fqtn, sizeof(sub_fqtn), sub_typschema, sub_typname));
		return FALSE;
	}

	/* cannot sub-class a record type */
	if (h_base->typoid == RECORDOID)
	{
		PQseterror(err, "Cannot sub-class pg_catalog.record '%s'",
			pqt_fqtn(sub_fqtn, sizeof(sub_fqtn), sub_typschema, sub_typname));
		return FALSE;
	}

	if (!expandHandlers(connData, err))
		return FALSE;

	h_sub = &connData->typhandlers[connData->typhcnt];
	memset(h_sub, 0, sizeof(PGtypeHandler));

	h_sub->id = connData->typhcnt + countof(pg_handlers);
	h_sub->typlen = h_base->typlen;
	h_sub->typoid = h_base->typoid;
	h_sub->typoid_array = h_base->typoid_array;
	h_sub->typput = typput;
	h_sub->typget = typget;
	h_sub->base_id = h_base->id;

	pqt_strcpy(h_sub->typschema,
		sizeof(h_sub->typschema), sub_typschema);

	pqt_strcpy(h_sub->typname,
		sizeof(h_sub->typname), sub_typname);

	connData->typhcnt++;
	return TRUE;
}

static int
expandHandlers(PGtypeData *typeData, PGerror *err)
{
	int hmax;
	PGtypeHandler *h;

	if (typeData->typhcnt < typeData->typhmax)
		return TRUE;

	hmax = typeData->typhmax ? (typeData->typhmax * 3) / 2 : 8;
	h = (PGtypeHandler *) pqt_realloc(
		typeData->typhandlers, sizeof(PGtypeHandler) * hmax);

	if (!h)
	{
		PQseterror(err, PQT_OUTOFMEMORY);
		return FALSE;
	}

	typeData->typhandlers = h;
	typeData->typhmax = hmax;
	return TRUE;
}

static int
checkTypeLookups(PGresult *res, PGerror *err, PGregisterType *types, int count)
{
	int i;
	int ntups = PQntuples(res);

  /* The tuple count must match the requested count.  The server omits
   * tuples for types it did not find.  For those it did find, it returns
   * a sequenctial index.  The first gap found is our missing type.  This
   * only reports about the first missing type.
   */
  if (ntups == count)
		return TRUE;

 	for (i=0; i < ntups; i++)
	{
		int idx;

		if (!PQgetf(res, err, i, "%int4", 0, &idx))
			return FALSE;

		/* 'i' should always match idx-1, postgresql arrays are 1-based.
		 * This is a missing type, first gap in the sequence.
		 */
		if (i != idx-1)
			break;
	}

	PQseterror(err, "server type lookup failed: could not find '%s'",
		types[i].typname);

	return FALSE;
}

/* This is part of a performance enhancement for getting arrays
 * and/or composites.  They require generating PGresults which
 * causes pqt_duphandlers() to run.  Its amazing how much a simple
 * malloc+memcpy costs after around 10000 or so.  The common case
 * avoids this by using a fixed length PGrecordAttDesc buffer.  If
 * there are a large number of attributes, the slower path is used.
 * Again this is small, maybe 10% of the overall 63% win.
 */
static PGrecordAttDesc *initAttDescs(PGerror *err, PGtypeHandler *h, char *attrs)
{
	char *p;
	int nattrs = 1;
	PGrecordAttDesc *attDescs;

	for(p = attrs; *p; nattrs++, ++p)
		if(!(p = strchr(p, ' ')))
			break;

	if (nattrs < (int) (sizeof(h->attDescsBuf) / sizeof(h->attDescsBuf[0])))
	{
		h->freeAttDescs =  0;
		attDescs = h->attDescsBuf;
	}
	else
	{
		attDescs = (PGrecordAttDesc *) malloc(nattrs * sizeof(PGrecordAttDesc));
		if (!attDescs)
		{
			PQseterror(err, PQT_OUTOFMEMORY);
			return NULL;
		}

		h->freeAttDescs = 1;
	}

	return attDescs;
}

/* wraps pqt_parsetype to toggle out illegal flags during a register */
static char *parseType(PGerror *err, const char *spec, char *typschema, char *typname,
	int typpos)
{
	char *s;
	int flags;

	if (!(s = pqt_parsetype(err, spec, typschema, typname, &flags, typpos)))
		return NULL;

	if (flags & TYPFLAG_INVALID)
		return NULL;

	if (flags & TYPFLAG_ARRAY)
	{
		PQseterror(err, "Cannot use an array[] during a type handler registration.");
		return NULL;
	}

	if (flags & TYPFLAG_POINTER)
	{
		PQseterror(err, "Cannot use a type* during a type handler registration.");
		return NULL;
	}

	return s;
}

static int getTypeParams(PGconn *conn, PGerror *err, PGregisterType *types, int count,
	PGarray *names, PGarray *schemas)
{
	int i;

	names->ndims = 0;
	schemas->ndims = 0;

	if (!(names->param = PQparamCreate(conn, err)))
		return FALSE;

	if (!(schemas->param = PQparamCreate(conn, err)))
	{
		PQparamClear(names->param);
		return FALSE;
	}

	for (i=0; i < count; i++)
	{
		char typname[PQT_MAXIDLEN + 1];
		char typschema[PQT_MAXIDLEN + 1];
		char *s = parseType(err, types[i].typname, typschema, typname, 1);

		if (!s)
		{
			PQparamClear(names->param);
			PQparamClear(schemas->param);
			return FALSE;
		}

		s = *typschema ? typschema : NULL;
		if (!PQputf(names->param, err, "%text", typname) ||
			!PQputf(schemas->param, err, "%text", s))
		{
			PQparamClear(names->param);
			PQparamClear(schemas->param);
			return FALSE;
		}
	}

	return TRUE;
}



/* Lookup types, including composites.  Arguments are:
 *   schemas text[], type_names text[], want_attrs bool
 */
#define LOOKUP_TYPES \
"WITH nspnames AS" \
"(" \
"  SELECT * FROM information_schema._pg_expandarray(%text[])" \
")," \
"typnames AS" \
"(" \
"  SELECT * FROM information_schema._pg_expandarray(%text[])" \
")," \
"curpath AS" \
"(" \
"  SELECT * FROM information_schema._pg_expandarray(current_schemas(true))" \
")," \
"composites AS" \
"(" \
"  SELECT n.n AS idx, n.x AS nspname, t.x AS typname" \
"    FROM nspnames n LEFT JOIN typnames t ON n.n = t.n" \
"      AND n.x IS NOT NULL" \
"      WHERE t.x IS NOT NULL" \
"  UNION ALL" \
"  SELECT n.n AS idx," \
"  (" \
"    SELECT n.nspname from pg_type nt JOIN pg_namespace n ON " \
         "nt.typnamespace = n.oid" \
"      JOIN curpath c ON c.x = n.nspname" \
"      WHERE nt.typname = t.x" \
"      ORDER BY c.n LIMIT 1" \
"  ) AS nspname, t.x AS typname" \
"    FROM nspnames n LEFT JOIN typnames t ON n.n = t.n" \
"      AND n.x IS NULL" \
"      WHERE t.x IS NOT NULL" \
")" \
"SELECT idx, t.oid AS typoid, a.oid AS arroid, t.typlen," \
" (" \
"   CASE WHEN %bool THEN (" \
"   SELECT array_to_string" \
"   (" \
"     ARRAY" \
"     (" \
"       SELECT CASE WHEN tt.typtype = 'd' THEN tt.typbasetype " \
           "ELSE a.atttypid END ||" \
"         ',' || attlen || ',' || atttypmod || ',' || " \
            "encode(attname::bytea, 'hex')" \
"         FROM pg_type b" \
"           JOIN pg_attribute a ON b.typrelid = a.attrelid" \
"           JOIN pg_type tt ON a.atttypid = tt.oid" \
"           WHERE b.oid = t.oid" \
"             AND a.attnum > 0" \
"             AND NOT a.attisdropped" \
"           ORDER BY a.attnum" \
"    ), ' ')" \
"  ) ELSE NULL END) AS arr_props" \
"  FROM composites c" \
"  JOIN pg_type t ON t.typname = c.typname" \
"  JOIN pg_namespace n ON t.typnamespace = n.oid AND n.nspname = c.nspname" \
"  JOIN pg_type a ON a.oid = t.typarray" \
"  ORDER BY idx;"

/* Lookup types for pre 8.4 servers, including composites.  Arguments are:
 *  want_attrs bool, schemas text[], type_names text[]
 */
#define LOOKUP_TYPES_PRE_8_4 \
"SELECT idx, t.oid AS typoid, a.oid AS arroid, t.typlen," \
" (" \
"   CASE WHEN %bool THEN (" \
"   SELECT array_to_string" \
"   (" \
"     ARRAY" \
"     (" \
"       SELECT CASE WHEN tt.typtype = 'd' THEN tt.typbasetype " \
            "ELSE a.atttypid END ||" \
"         ',' || attlen || ',' || atttypmod || ',' || " \
            "encode(attname::bytea, 'hex')" \
"         FROM pg_type b" \
"           JOIN pg_attribute a ON b.typrelid = a.attrelid" \
"           JOIN pg_type tt ON a.atttypid = tt.oid" \
"           WHERE b.oid = t.oid" \
"             AND a.attnum > 0" \
"             AND NOT a.attisdropped" \
"           ORDER BY a.attnum" \
"    ), ' ')" \
"  ) ELSE NULL END) AS arr_props" \
"  FROM" \
"  (" \
"    SELECT n.n AS idx, n.x AS nspname, t.x AS typname" \
"      FROM" \
"      (" \
"        SELECT * FROM information_schema._pg_expandarray(%text[])" \
"      ) n LEFT JOIN" \
"      (" \
"        SELECT * FROM information_schema._pg_expandarray(%text[])" \
"      ) t ON n.n = t.n" \
"        AND n.x IS NOT NULL" \
"        WHERE t.x IS NOT NULL" \
"    UNION ALL" \
"    SELECT n.n AS idx," \
"    (" \
"      SELECT n.nspname from pg_type nt JOIN pg_namespace n ON " \
           "nt.typnamespace = n.oid" \
"        JOIN" \
"        (" \
"          SELECT * FROM information_schema._pg_expandarray(" \
             "current_schemas(true))" \
"        ) c ON c.x = n.nspname" \
"        WHERE nt.typname = t.x" \
"        ORDER BY c.n LIMIT 1" \
"    ) AS nspname, t.x AS typname" \
"      FROM" \
"      (" \
"        SELECT * FROM information_schema._pg_expandarray($2)" \
"      ) n LEFT JOIN" \
"      (" \
"        SELECT * FROM information_schema._pg_expandarray($3)" \
"      ) t ON n.n = t.n" \
"        AND n.x IS NULL" \
"        WHERE t.x IS NOT NULL" \
"  ) c" \
"  JOIN pg_type t ON t.typname = c.typname" \
"  JOIN pg_namespace n ON t.typnamespace = n.oid AND n.nspname = c.nspname" \
"  JOIN pg_type a ON a.oid = t.typarray" \
"  ORDER BY idx"

static PGresult *
execLookupTypes(PGconn *conn, PGerror *err, PGtypeData *data, PGarray *schemas,
	PGarray *names, int want_attrs)
{
	if(data->fmtinfo.sversion >= 80400)
		return PQexecf(conn, err, LOOKUP_TYPES, schemas, names, &want_attrs);
	return PQexecf(conn, err, LOOKUP_TYPES_PRE_8_4, &want_attrs, schemas, names);
}

static int
sendLookupTypes(PGconn *conn, PGerror *err, PGtypeData *data, PGarray *schemas,
	PGarray *names, int want_attrs)
{
	if(data->fmtinfo.sversion >= 80400)
		return PQsendf(conn, err, LOOKUP_TYPES, schemas, names, &want_attrs);
	return PQsendf(conn, err, LOOKUP_TYPES_PRE_8_4, &want_attrs, schemas, names);
}

static int
performRegisterQuery(PGconn *conn, PGerror *err, int which, PGregisterType *types,
	int count, PGresult **res)
{
	int n = FALSE;
	PGtypeData *connData;
	PGarray names;
	PGarray schemas;
	int want_attrs = which == PQT_COMPOSITE;

	if (!(connData = (PGtypeData *) PQinstanceData(conn, pqt_eventproc)))
	{
		PQseterror(err, "PGconn is missing event data");
		return FALSE;
	}

	if (!getTypeParams(conn, err, types, count, &names, &schemas))
		return FALSE;

	if (res)
		*res = execLookupTypes(conn, err, connData, &schemas, &names, want_attrs);
	else
		n = sendLookupTypes(conn, err, connData, &schemas, &names, want_attrs);

	PQparamClear(names.param);
	PQparamClear(schemas.param);

	if (res)
		return *res ? TRUE : FALSE;

	return n;
}
