
/*
 * spec.c
 *   Type Specifier parser and compiler.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

/* For use with pqt_parse */
#define CHKSTMTBUF(nbytes_add) do{ \
	if ((*stmtPos + (nbytes_add)) >= stmtBufLen) \
	{ \
		PQseterror(err, "statement buffer is too small"); \
		return FALSE; \
	} \
}while (0)

/* For use with PQspecPrepare */
#define FREESTMTBUF do{ \
	if (stmtBuf && stmtBuf != buffer) \
		free(stmtBuf); \
}while(0)

static char *
skipQuotes(char *s, PGerror *err);

static char *
parseId(PGerror *err, char *id, char **start, int *len, int *flags, int typpos);

static int
expandSpecs(PGtypeData *typeData, PGerror *err);

int
PQspecPrepare(PGconn *conn, PGerror *err, const char *name,
	const char *format, int is_stmt)
{
	int flags;
	int typpos = 0;
	int idmax = 0;
	size_t stmtPos = 0;
	PGtypeHandler *h;
	PGtypeData *typeData;
	PGtypeSpec *spec;
	size_t stmtBufLen = 0;
	char *stmtBuf = NULL;
	char buffer[8192];

	if (!conn)
	{
		PQseterror(err, "PGConn cannot be NULL");
		return FALSE;
	}

	if (!name || !*name)
	{
		PQseterror(err, "Prepared specifier name cannot be NULL or an empty string");
		return FALSE;
	}

	if (format && !*format)
	{
		PQseterror(err, "Specifier format string cannot be empty");
		return FALSE;
	}

	if (!isalnum(*name) && *name != '_')
	{
		PQseterror(err, "Prepared specifier name must begin with an alpha, "
			"number or underscore.");
		return FALSE;
	}

	typeData = PQinstanceData(conn, pqt_eventproc);
	if (!typeData)
	{
		PQseterror(err, "No type data exists for PGconn at %p", conn);
		return FALSE;
	}

	/* This is a removal request */
	if (!format)
	{
		int i;

		for (i=0; i < typeData->typspeccnt; i++)
		{
			if (strcmp(typeData->typspecs[i].name, name) == 0)
			{
				/* clear it */
				pqt_clearspec(&typeData->typspecs[i]);

				/* remove from list, not needed if its the last element */
				if (i != typeData->typspeccnt - 1)
					memmove(typeData->typspecs + i, typeData->typspecs + i + 1,
						(typeData->typspeccnt - i - 1) * sizeof(PGtypeSpec));

				typeData->typspeccnt--;
				break;
			}
		}

		/* always return TRUE, an error is not useful here */
		return TRUE;
	}

	/* Already exists case */
	spec = pqt_getspec(typeData->typspecs, typeData->typspeccnt, name);
	if (spec)
	{
		PQseterror(err, "Prepared spec already exists '%s'", name);
		return FALSE;
	}

	/* Make sure specs array is large enough */
	if (!expandSpecs(typeData, err))
		return FALSE;

	spec = &typeData->typspecs[typeData->typspeccnt];

	/* cache statement along with prepared type spec */
	if (is_stmt)
	{
		stmtBufLen = strlen(format) + 1;

		/* no room in stack, use heap */
		if (stmtBufLen > sizeof(buffer))
		{
			stmtBuf = (char *) malloc(stmtBufLen);
			if (!stmtBuf)
			{
				PQseterror(err, PQT_OUTOFMEMORY);
				return FALSE;
			}
		}
		else
		{
			stmtBuf = buffer;
			stmtBufLen = sizeof(buffer);
		}
	}

	while (format && *format)
	{
		format = pqt_parse(err, format, typeData->typhandlers, typeData->typhcnt,
			stmtBuf, stmtBufLen, &h, &stmtPos, &typpos, &flags);

		if (!format)
		{
			pqt_clearspec(spec);
			FREESTMTBUF;
			return FALSE;
		}

		/* skipped harmless chars in format, like quoted sections. */
		if(!h)
			continue;

		if (!spec->idlist || spec->idcnt == idmax)
		{
			int c = idmax ? idmax * 2 : 8;
			void *p = pqt_realloc(spec->idlist, c * sizeof(int));

			if (!p)
			{
				PQseterror(err, PQT_OUTOFMEMORY);
				pqt_clearspec(spec);
				FREESTMTBUF;
				return FALSE;
			}

			spec->idlist = (int *) p;

			p = pqt_realloc(spec->flags, c * sizeof(char));
			if (!p)
			{
				PQseterror(err, PQT_OUTOFMEMORY);
				pqt_clearspec(spec);
				FREESTMTBUF;
				return FALSE;
			}

			spec->flags = (unsigned char *) p;
			idmax = c;
		}

		/* Parallel arrays, every handler needs type flags */
		spec->idlist[spec->idcnt] = h->id;
		spec->flags[spec->idcnt++] = (unsigned char) flags;
	}

	/* terminate stmtBuf, guarenteed to have room for NUL */
	if (stmtBuf)
		stmtBuf[stmtPos] = 0;

	/* copy name string */
	spec->name = strdup(name);
	if (!spec->name)
	{
		pqt_clearspec(spec);
		PQseterror(err, PQT_OUTOFMEMORY);
		FREESTMTBUF;
		return FALSE;
	}

	/* copy the parameterized stmt string */
	if (stmtBuf)
	{
		spec->stmt = strdup(stmtBuf);
		if (!spec->stmt)
		{
			pqt_clearspec(spec);
			PQseterror(err, PQT_OUTOFMEMORY);
			FREESTMTBUF;
			return FALSE;
		}
	}

	FREESTMTBUF;

	/* Success, increment type spec count */
	typeData->typspeccnt++;
	return TRUE;
}

int
PQclearSpecs(PGconn *conn, PGerror *err)
{
	PGtypeData *typeData;

	if (!conn)
	{
		PQseterror(err, "PGConn cannot be NULL");
		return FALSE;
	}

	typeData = PQinstanceData(conn, pqt_eventproc);
	if (!typeData)
	{
		PQseterror(err, "No type data exists for PGconn at %p", conn);
		return FALSE;
	}

	pqt_freespecs(typeData->typspecs, typeData->typspeccnt);
	typeData->typspecs = NULL;
	typeData->typspeccnt = 0;
	typeData->typspecmax = 0;

	return TRUE;
}

char *pqt_parse(PGerror *err, const char *format, PGtypeHandler *h, int hcnt,
	char *stmtBuf, size_t stmtBufLen, PGtypeHandler **out, size_t *stmtPos,
	int *typpos, int *flags)
{
	int specMark;
	char *s = skipQuotes((char *) format, err);
	char typname[PQT_MAXIDLEN + 1];
	char schema[PQT_MAXIDLEN + 1];
	char tmp[200];

	*out = NULL;

	if (!s)
		return NULL;

	/* found quotes to skip */
	if (s != format)
	{
		if (stmtBuf)
		{
			size_t n = s - format;
			CHKSTMTBUF(n);
			memcpy(stmtBuf + *stmtPos, format, n);
			(*stmtPos) += n;
		}

		return s;
	}

	specMark = *format;
	if (specMark != '%' && specMark != '#')
	{
		if (stmtBuf)
		{
			CHKSTMTBUF(1);
			stmtBuf[*stmtPos] = *format;
			(*stmtPos)++;
		}

		format++;
		return (char *) format;
	}

	/* spec skips % or # */
	if (!(s = pqt_parsetype(err, format + 1, schema, typname, flags, *typpos + 1)))
		return NULL;

	if (*flags & TYPFLAG_INVALID)
	{
		if (stmtBuf)
		{
			CHKSTMTBUF(1);
			stmtBuf[*stmtPos] = *format++;
			(*stmtPos)++;
			return (char *) format;
		}

		return NULL;
	}

	(*typpos)++;

	if (!(*out = pqt_gethandler(h, hcnt, schema, typname)))
	{
		PQseterror(err, "Unknown type '%s' (position %d)",
			pqt_fqtn(tmp, sizeof(tmp), schema, typname), *typpos);
		return NULL;
	}

	if (stmtBuf)
	{
		int n = pqt_snprintf(tmp, sizeof(tmp), "$%d", *typpos);
		CHKSTMTBUF(n);
		memcpy(stmtBuf + *stmtPos, tmp, n);
		(*stmtPos) += n;
	}

	if (!(*out)->typput)
	{
		PGtypeHandler *o = pqt_gethandlerbyid(h, hcnt, h->base_id);
		if (!o || !o->typput)
		{
			PQseterror(err,
				"Type '%s' doesn't support put operations (position %d)",
				pqt_fqtn(tmp, sizeof(tmp), (*out)->typschema,
					(*out)->typname), *typpos);

			*out = NULL;
			return NULL;
		}

		*out = o;
	}

	if ((*flags & TYPFLAG_POINTER) && !pqt_allowsptr(*out))
	{
		PQseterror(err,
			"Type '%s' doesn't support putting pointers (position %d)",
			pqt_fqtn(tmp, sizeof(tmp), (*out)->typschema,
				(*out)->typname), *typpos);

		*out = NULL;
		return NULL;
	}

	if (specMark == '#')
		(*flags) |= TYPFLAG_BYNAME;

	return s;
}

void
pqt_clearspec(PGtypeSpec *spec)
{
	if (spec->name)
		free(spec->name);

	if (spec->stmt)
		free(spec->stmt);

	if (spec->idlist)
		free(spec->idlist);

	if (spec->flags)
		free(spec->flags);

	memset(spec, 0, sizeof(PGtypeSpec));
}

PGtypeSpec *pqt_dupspecs(PGtypeSpec *specs, int count)
{
	int i;
	PGtypeSpec *new_specs = (PGtypeSpec *) malloc(count * sizeof(PGtypeSpec));

	if (!new_specs)
		return NULL;

	memset(new_specs, 0, count * sizeof(PGtypeSpec));

	for (i=0; i < count; i++)
	{
		PGtypeSpec *s = &specs[i];
		PGtypeSpec *news = &new_specs[i];

		news->idcnt = s->idcnt;

		news->name = strdup(s->name);
		if (!news->name)
		{
			pqt_freespecs(new_specs, i+1);
			return NULL;
		}

		if(s->stmt)
		{
			news->stmt = strdup(s->stmt);
			if (!news->stmt)
			{
				pqt_freespecs(new_specs, i+1);
				return NULL;
			}
		}

		news->idlist = (int *) malloc(s->idcnt * sizeof(int));
		if (!news->idlist)
		{
			pqt_freespecs(new_specs, i+1);
			return NULL;
		}

		memcpy(news->idlist, s->idlist, s->idcnt * sizeof(int));

		news->flags = (unsigned char *) malloc(s->idcnt * sizeof(char));
		if (!news->flags)
		{
			pqt_freespecs(new_specs, i+1);
			return NULL;
		}

		memcpy(news->flags, s->flags, s->idcnt * sizeof(char));
	}

	return new_specs;
}

void pqt_freespecs(PGtypeSpec *specs, int count)
{
	int i;

	for (i=0; i < count; i++)
		pqt_clearspec(&specs[i]);

	if (specs)
		free(specs);
}

PGtypeSpec *pqt_getspec(PGtypeSpec *specs, int count, const char *name)
{
	int i;

	for (i=0; i < count; i++)
		if (strcmp(specs[i].name, name) == 0)
			return &specs[i];

	return NULL;
}

/* Parse a type identifer name (schema qualified or not) from spec. spec
 * must point to the first char after the % sign, which maybe a
 * double quote.
 *
 * spec - pointer to typname, just after the '%' or '#'
 * schema - buffer to receive schema (PQT_MAXIDLEN bytes)
 * typname - buffer to receive typname (PQT_MAXIDLEN bytes)
 * flags - a pointer to an int that is set one or more TYPFLAG_xxx
 * typpos - 1-based position of spec in specifier string (0 for unknown)
 */
char *
pqt_parsetype(PGerror *err, const char *spec, char *schema, char *typname,
	int *flags, int typpos)
{
	int i;
	char *start;
	int len=0;
	char *s = (char *)spec;

	if (!(s = parseId(err, s, &start, &len, flags, typpos)))
		return NULL;

	/* not a valid specifer, false positive like "(x % y) = 0" */
	if (*flags & TYPFLAG_INVALID)
		return s;

	*schema = 0;
	if (*s == '.')
	{
		memcpy(schema, start, len);
		schema[len] = 0;
		if (*flags & TYPFLAG_CASEFOLD)
			for (i=0; i < len; i++)
				schema[i] = pqt_tolower(schema[i]);

		/* now get typname */
		if (!(s = parseId(err, ++s, &start, &len, flags, typpos)))
			return NULL;

		if (*flags & TYPFLAG_INVALID)
			return s;
	}

	memcpy(typname, start, len);
	typname[len] = 0;
	if (*flags & TYPFLAG_CASEFOLD)
		for (i=0; i < len; i++)
			typname[i] = pqt_tolower(typname[i]);

	return s;
}

static char *
parseId(PGerror *err, char *id, char **start, int *len, int *flags, int typpos)
{
	char *p = id;

	*flags = 0;
	*start = NULL;
	*len = 0;

	if (*p == '"')
		p++;

	/* check first character */
	if (!isalpha(*p) && *p != '_')
	{
		*flags |= TYPFLAG_INVALID;
		PQseterror(err,
			"Invalid first character for identifier '%c' (pos:%d)", *p, typpos);
		return p;
	}

	if (*id == '"')
	{
		id++;
		if (!(p = strchr(id, '"')))
		{
			*flags |= TYPFLAG_INVALID;
			PQseterror(err, "Unterminated double quote '%s' (pos:%d)",
				id-1, typpos);
			return p;
		}

		*len = (int) (p - id);
		*start = id;
		p++;
	}
	else
	{
		for (p=id+1; isalnum(*p) || *p=='_'; p++) ;

		*len = (int) (p - id);
		*start = id;
		*flags |= TYPFLAG_CASEFOLD;
	}

	/* range check */
	if (*len == 0 || *len > PQT_MAXIDLEN)
	{
		*flags |= TYPFLAG_INVALID;
		PQseterror(err, "Identifier out of range %d (pos:%d), range is 1 to %d",
			*len, typpos, PQT_MAXIDLEN);
		return p;
	}

	/* direct pointer request */
	if (*p == '*')
	{
		p++;
		*flags |= TYPFLAG_POINTER;
	}

	/* Is this an array?  Ex. %int4[] or %"a b"[] */
	if (p[0] == '[' && p[1] == ']')
	{
		if (*flags & TYPFLAG_POINTER)
		{
			PQseterror(err,
				"'*' specifer flag cannot be used with arrays[] '%s' (pos:%d)",
				id, typpos);
			return NULL;
		}

		*flags |= TYPFLAG_ARRAY;
		p += 2;
	}

	return p;
}

static int
expandSpecs(PGtypeData *typeData, PGerror *err)
{
	int n;
	PGtypeSpec *specs;

	if (typeData->typspeccnt < typeData->typspecmax)
		return TRUE;

	n = typeData->typspecmax ? (typeData->typspecmax * 3) / 2 : 8;

	specs = (PGtypeSpec *) pqt_realloc(
		typeData->typspecs, sizeof(PGtypeSpec) * n);

	if (!specs)
	{
		PQseterror(err, PQT_OUTOFMEMORY);
		return FALSE;
	}

	memset(specs + typeData->typspeccnt, 0,
		(n - typeData->typspeccnt) * sizeof(PGtypeSpec));

	typeData->typspecs = specs;
	typeData->typspecmax = n;
	return TRUE;
}

/* skip quoted strings.  Doesn't need to account for E'' syntax. The
 * E is copied over prior to the quoted string.
 *
 * Returns a pointer to the next character after the closing quote or
 * NULL if there was an error.
 */
static char *
skipQuotes(char *s, PGerror *err)
{
	char *end;

	if (*s != '\'')
		return s;

	end = s;
	while (*++end)
	{
		/* If we see a backslash, skip an extra char.  No need to dig any
		 * further since this method works with \digits and \hex.
		 */
		if (*end == '\\')
			end++;
		else if (*end == '\'')
			break;
	}

	/* unterminated quote */
	if (!*end)
	{
		PQseterror(err, "unterminated single quoted string");
		return NULL;
	}

	return ++end; /* skip ending quote */
}

