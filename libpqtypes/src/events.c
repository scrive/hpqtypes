
/*
 * events.c
 *   The libpq PGEventProc implementation.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

static PGtypeData *
allocTypeData(PGconn *conn);

static void
freeTypeData(PGtypeData *typeData);

/* Deprecated, use PQinitTypes instead */
int
PQtypesRegister(PGconn *conn)
{
	return PQinitTypes(conn);
}

int
pqt_eventproc(PGEventId id, void *info, void *passThrough)
{
	switch (id)
	{
		case PGEVT_REGISTER:
		{
			PGEventRegister *e = (PGEventRegister *) info;
			void *data = allocTypeData(e->conn);
			if (!data)
				return FALSE;
			PQsetInstanceData((PGconn *) e->conn, pqt_eventproc, data);
			break;
		}

		case PGEVT_CONNRESET:
		{
			/* No special handling for PGEVT_CONNRESET.  Previously, types were
			 * automatically re-registered but this idea fails miserably during
			 * asynchronous resets.  Yanked in favor of using the following
			 * call sequence: PQresetXXX, PQclearTypes, PQregisterTypes.
			 */
			break;
		}

		case PGEVT_CONNDESTROY:
		{
			PGEventConnDestroy *e = (PGEventConnDestroy *) info;
			freeTypeData((PGtypeData *) PQinstanceData(e->conn, pqt_eventproc));
			break;
		}

		case PGEVT_RESULTCREATE:
		{
			PGtypeData *resData;
			PGEventResultCreate *e = (PGEventResultCreate *) info;
			PGtypeData *connData = (PGtypeData *) PQinstanceData(
				e->conn, pqt_eventproc);

			if (!connData || !(resData = allocTypeData(e->conn)))
				return FALSE;

			/* copy type handlers from PGconn's typeData */
			if (connData->typhcnt > 0)
			{
				resData->typhandlers = pqt_duphandlers(
					connData->typhandlers, connData->typhcnt);

				if (resData->typhandlers)
					resData->typhcnt = connData->typhcnt;
			}

			/* copy type specs from PGconn's typeData */
			if (connData->typspeccnt > 0)
			{
				resData->typspecs = pqt_dupspecs(
					connData->typspecs, connData->typspeccnt);

				if (resData->typspecs)
					resData->typspeccnt = connData->typspeccnt;
			}

			PQresultSetInstanceData((PGresult *) e->result, pqt_eventproc, resData);
			break;
		}

		case PGEVT_RESULTCOPY:
		{
			PGtypeData *destData;
			PGEventResultCopy *e = (PGEventResultCopy *) info;
			PGtypeData *srcData = (PGtypeData *) PQresultInstanceData(
				e->src, pqt_eventproc);

			if (!srcData || !(destData = allocTypeData(NULL)))
				return FALSE;

			memcpy(&destData->fmtinfo, &srcData->fmtinfo, sizeof(PGtypeFormatInfo));

			/* copy type handlers from PGresult's typeData */
			if (srcData->typhcnt > 0)
			{
				destData->typhandlers = pqt_duphandlers(
					srcData->typhandlers, srcData->typhcnt);

				if (destData->typhandlers)
					destData->typhcnt = srcData->typhcnt;
			}

			/* copy type specs from PGresult's typeData */
			if (srcData->typspeccnt > 0)
			{
				destData->typspecs = pqt_dupspecs(
					srcData->typspecs, srcData->typspeccnt);

				if (destData->typspecs)
					destData->typspeccnt = srcData->typspeccnt;
			}

			PQresultSetInstanceData(e->dest, pqt_eventproc, destData);
			break;
		}

		case PGEVT_RESULTDESTROY:
		{
			PGEventResultDestroy *e = (PGEventResultDestroy *) info;
			freeTypeData((PGtypeData *) PQresultInstanceData(
				e->result, pqt_eventproc));
			break;
		}
	}

	return TRUE;
}

static PGtypeData *
allocTypeData(PGconn *conn)
{
	PGtypeData *typeData = (PGtypeData *) malloc(sizeof(PGtypeData));

	if (typeData)
	{
		memset(typeData, 0, sizeof(PGtypeData));

		/* get type formatting info from conn */
		if (conn)
			pqt_getfmtinfo(conn, &typeData->fmtinfo);
	}

	return typeData;
}

static void
freeTypeData(PGtypeData *typeData)
{
	if (typeData)
	{
		pqt_cleartypes(typeData);

		pqt_freespecs(typeData->typspecs, typeData->typspeccnt);
		typeData->typspecs = NULL;
		typeData->typspeccnt = 0;
		typeData->typspecmax = 0;

		free(typeData);
	}
}

