
#ifdef HAVE_CONFIG_H
#	include "pqt_config.h"
#endif

#define PLN do{ \
	printf("%s:%d\n", __FUNCTION__, __LINE__); \
	fflush(stdout); \
}while(0)

#if defined(_WIN32) || defined(_WIN64)
#	define _CRT_SECURE_NO_WARNINGS /* shut up msvc8! */
#	ifdef _MSC_VER
# 	pragma warning (disable : 4127 4706)
#	endif
#	include <winsock2.h>
#	include <ws2tcpip.h> /* getnameinfo */
#	ifdef _MSC_VER
#		define I64FMT "%I64d"
#	else
#		define I64FMT "%lld"
#	endif
#else
#	define SIGNAL_SUPPORT
#	include <netdb.h>
#	include <unistd.h>
#	include <sys/socket.h>
#	include <netinet/in.h>
#	define I64FMT "%lld"
#endif

#ifdef SIGNAL_SUPPORT
#	include <signal.h>
#endif

#include <libpqtypes.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <time.h>
#include <stdarg.h>
#include <errno.h>

#ifndef LLONG_MAX
# ifdef LONG_LONG_MAX
#   define LLONG_MAX LONG_LONG_MAX
# else
#   define LLONG_MAX 9223372036854775807LL
# endif
#endif

#ifndef LLONG_MIN
# ifdef LONG_LONG_MIN
#   define LLONG_MIN LONG_LONG_MIN
# else
#   define LLONG_MIN -9223372036854775808LL
# endif
#endif

#define RESERR PQresultErrorMessage(result)
#define CLEAR_TABLE(tname) PQclear(PQexec(conn, "TRUNCATE " tname))
#define DROP_TABLE(tname) \
	PQclear(PQexec(conn, "DROP TABLE IF EXISTS " tname))

#define EXECOKAY(msg, status) do{ \
	if (PQresultStatus(result) != (status)) \
	{ \
		fprintf(stderr, "  **ERROR: %s - %s\n", msg, RESERR); \
		PQclear(result); \
		failcnt++; \
		return; \
	} \
}while (0)

#define CMDOKAY(msg) EXECOKAY(msg, PGRES_COMMAND_OK)
#define TUPSOKAY(msg) EXECOKAY(msg, PGRES_TUPLES_OK)

#define PUTOKAY(paramobj, retval, msg) do{ \
	if ((retval) == 0){ \
		failcnt++; \
		fprintf(stderr, "  **ERROR: %s - %s\n", msg, PQgeterror()); \
		return;	\
	} \
}while (0)

#define GETOKAY(retval, msg) GETOKAY2(result, retval, msg)
#define GETOKAY2(_res, retval, msg) do{ \
	if ((retval) == 0){ \
		failcnt++; \
		fprintf(stderr, "  **ERROR: %s - %s\n", msg, PQgeterror()); \
		PQclear(result); \
		return;	\
	} \
}while (0)

#define CHKNUM(typspec, getval, putval) do{ \
	if ((getval) != (putval)) { \
		failcnt++; \
		fprintf(stderr, "  %s - FAILED\n", typspec); \
	} \
	else \
		printf("  %s - passed\n", typspec); \
	testcnt++; \
} while (0)

#define CHKGEO(type) do{ \
	const char *typspec = "%" # type; \
	if (memcmp(& type, & type ## val, sizeof(type))) { \
		failcnt++; \
		fprintf(stderr, "  %s - FAILED\n", typspec); \
	} \
	else \
		printf("  %s - passed\n", typspec); \
	testcnt++; \
} while (0)

#define CHKVLEN(typ, resval, inval) do{ \
	testcnt++; \
	if (strcmp(resval, inval)) { \
		failcnt++; \
		fprintf(stderr, "  %%" typ " - FAILED\n"); \
	} \
	else \
		printf("  %%" typ " - passed\n"); \
}while (0)

static void cleanup(void);
static void test_natives(int format);
static void test_composite(void);
static void test_geometrics(int format);
static void test_varlen(int format);
static void test_datetime(int format);
static void test_network(int format);
static int epoch_put(PGtypeArgs *args);
static int epoch_get(PGtypeArgs *args);
static void test_subclass(int format);
static PGresult *execf(const char *cmdSpec, ...);
static int bpcharcmp(const char *put, const char *get, int charlen);

#ifdef SIGNAL_SUPPORT
static void sighandler(int s);
#endif

static void _notice_processor(void *arg, const char *message)
{/* do nothing */
	arg = NULL;
	message = NULL;
}


static int testcnt = 0;
static int failcnt = 0;
static const char *datestyle = "";
static PGconn *conn = NULL;
static PGparam *param = NULL;
static PGresult *result = NULL;

#define ALIGN_NUM(val, mult) (((val) + ((mult) - 1)) & ~(((mult) - 1)))

int main(int argc, char **argv)
{
	PGint4 n;
	PGregisterType regtype;
	char *conninfo = argc > 1 ? argv[1] : "hostaddr=127.0.0.1 user=postgres";

	conn = PQconnectdb(conninfo);
	if (PQstatus(conn) != CONNECTION_OK)
	{
		fprintf(stderr, "connection failure: %s\n", PQerrorMessage(conn));
		return 1;
	}

	PQsetNoticeProcessor(conn, _notice_processor, NULL);

	if (!PQinitTypes(conn))
	{
		fprintf(stderr, "Failed to initialize libpqtypes as "
			"a libpq event proc\n");
		PQfinish(conn);
		return 1;
	}

	testcnt++;
	regtype.typname = "bad_type";
	if(PQregisterTypes(conn, PQT_COMPOSITE, &regtype, 1, 0))
	{
		failcnt++;
		fprintf(stderr, "Test bad type register failure case (FAILED)\n");
	}

	regtype.typname = "mytype_t";
  if(!PQregisterTypes(conn, PQT_COMPOSITE, &regtype, 1, 0))
    fprintf(stderr, "%s\n", PQgeterror());

	test_composite();

	PQclear(PQexec(conn, "SET DateStyle TO 'ISO'"));
	param = PQparamCreate(conn);

	/* test PQputvf - result should be 10 */
	testcnt++;
	printf("\nPQputvf\n");
	result = execf("SELECT %int4 + (%int4 % %int4) AS answer", 9, 67, 3);
	if (PQresultStatus(result) != PGRES_TUPLES_OK)
	{
		fprintf(stderr, "  **ERROR: PQputvf - %s\n", PQgeterror());
		PQclear(result);
		failcnt++;
		return 1;
	}

	PQgetf(result, 0, "#int4", "answer", &n);
	PQclear(result);
	if (n != 10)
	{
		failcnt++;
		fprintf(stderr, "  PQputvf - FAILED\n");
	}
	else
	{
		printf("  PQputvf - passed\n");
	}

	testcnt++;
	printf("\nPQspecPrepare\n");
	if(!PQspecPrepare(conn, "test", "SELECT %int8 + %int4", 1))
	{
		failcnt++;
		fprintf(stderr, "  PQspecPrepare - FAILED (%s)\n", PQgeterror());
	}
	else
	{
		char data[4];
		PGbytea ba;
		PGint8 i8 = 45;
		ba.len = 4;
		ba.data = data;

		result = PQexecf(conn, "@test", i8, 4);
		if(!result)
		{
			failcnt++;
			fprintf(stderr, "  PQspecPrepare - FAILED (%s)\n", PQgeterror());
		}
		else
		{
			PGint8 i8out=0;

			PQgetf(result, 0, "%int8", 0, &i8out);

			if(i8 + 4 != i8out)
			{
				failcnt++;
				fprintf(stderr, "  PQspecPrepare - FAILED (expected "
					"%lld but got %lld)\n", i8+4, i8out);
			}
			else
			{
				printf("  PQspecPrepare - passed\n");
			}

			PQclear(result);
		}
	}

	datestyle = PQparameterStatus(conn, "DateStyle");
	if (!datestyle)
		datestyle = "";

#ifdef SIGNAL_SUPPORT
	signal(SIGINT, sighandler);
	signal(SIGILL, sighandler);
	signal(SIGSEGV, sighandler);
#	ifdef SIGFPE
	signal(SIGFPE, sighandler);
#	endif
#	ifdef SIGBUS
	signal(SIGBUS, sighandler);
#	endif
#endif


	test_natives(0);
	test_natives(1);
	test_varlen(0);
	test_varlen(1);
	test_geometrics(0);
	test_geometrics(1);
	test_network(0);
	test_network(1);

	regtype.typname = "libpq.epoch=pg_catalog.timestamptz";
	regtype.typput = epoch_put;
	regtype.typget = epoch_get;

	/* Test type sub-classing, use the epoch example from docs */
	if (!PQregisterTypes(conn, PQT_SUBCLASS, &regtype, 1, 0))
	{
		fprintf(stderr, "PQregisterTypes(epoch, subclass): %s\n",
			PQerrorMessage(conn));
	}
	else
	{
		test_subclass(0);
		test_subclass(1);
	}

	test_datetime(0);
	test_datetime(1);

	/* datestyle is cached in PGparam.  Since we need to change the datestyle,
	 * we destroy the current param and recreate it later.
	 */
	PQparamClear(param);

	/* Just tested ISO, test a non-ISO */
	PQclear(PQexec(conn, "SET DateStyle TO 'POSTGRES'"));
	datestyle = PQparameterStatus(conn, "DateStyle");
	if (!datestyle)
		datestyle = "";

	param = PQparamCreate(conn);
	test_datetime(0);
	test_datetime(1);
	cleanup();

	printf("\nRan %d tests - %d failed\n", testcnt, failcnt);
	return 1;
}

/* Calls putf and executes cmdSpec. Returns a result object */
static PGresult *execf(const char *cmdSpec, ...)
{
  int n;
  PGresult *res;
  va_list ap;
  char stmt[4096];
  PGparam *prm = PQparamCreate(conn);

  va_start(ap, cmdSpec);
  n = PQputvf(prm, stmt, sizeof(stmt), cmdSpec, ap);
  va_end(ap);

  /* error message in PQparamErrorMessage */
  if (!n)
  {
    PQparamClear(prm);
    return NULL;
  }

	printf("  %s\n", stmt);
  res = PQparamExec(conn, prm, stmt, 1);
  PQparamClear(prm);
  return res;
}

/* Handle BPCHAR (Blank-Padded) SQL-type "character(n)" */
static int bpcharcmp(const char *put, const char *get, int charlen)
{
	int i;
	int len = (int)strlen(put);

	for (i=len; i < charlen; i++)
		if (!isspace(get[i]))
			return 1;

	return memcmp(put, get, len);
}

static void cleanup(void)
{
	DROP_TABLE("libpq_natives");
	DROP_TABLE("libpq_varlen");
	DROP_TABLE("libpq_geos");
	DROP_TABLE("libpq_network");
	DROP_TABLE("libpq_datetime");
	DROP_TABLE("libpq_subclass");
	DROP_TABLE("libpq_composite");
	PQclear(PQexec(conn, "DROP TYPE complex"));
	PQclear(PQexec(conn, "DROP TYPE simple"));
	PQparamClear(param);
	PQfinish(conn);
}

/* make an effort to clean up */
#ifdef SIGNAL_SUPPORT
static void sighandler(int s)
{
	cleanup();
	fprintf(stderr, "\n**Caught signal: %d ... aborting\n", s);
	exit(1);
}
#endif

static void test_composite(void)
{
	int i,x,r;
	int ntups;
	int inttups;
	PGtext text;
	PGparam *simple;
	PGparam *complex;
	PGparam *prm;
	PGresult *simple_res;
	PGarray intarr;
	PGarray comparr;
	PGchar buf[64];
	PGint4 a;
	PGint8 b;
	PGregisterType types[] = {
		{"public.simple", NULL, NULL},
		{"public.complex", NULL, NULL}
	};

	printf("\nComposites & Arrays: (always binary)\n");

	printf("  Testing empty array handling ");
	PQclear(PQexec(conn, "DROP TABLE libpq_array"));
  result = PQexec(conn, "CREATE TABLE libpq_array (arr int[])");
  CMDOKAY("creating libpq_array table:");
  PQclear(result);

	result = PQexec(conn, "INSERT INTO libpq_array VALUES ('{}')");
	CMDOKAY("inserting into libpq_array:");
  PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_array", 1);
	TUPSOKAY("executing select on libpq_array:");

	r = PQgetf(result, 0, "%int4[]", 0, &intarr);

	GETOKAY(r, "Failed to get an empty array");
	PQclear(result);

	/* both values should be 0 */
	testcnt++;
	if(PQntuples(intarr.res) != 0 || intarr.ndims != 0)
	{
		fprintf(stderr, "- FAILED\n    ***ERROR: tuple and dimension "
			"count should both be zero - ntups=%d, ndims=%d\n",
			PQntuples(intarr.res), intarr.ndims);
		failcnt++;
	}
	else
	{
		printf("- passed\n");
	}
	PQclear(intarr.res);

	DROP_TABLE("libpq_composite");
	PQclear(PQexec(conn, "DROP TYPE public.complex"));

	PQclear(PQexec(conn, "DROP TYPE public.simple"));
	result = PQexec(conn, "CREATE TYPE simple AS (a int4, b int8)");
	CMDOKAY("creating simple type");
	PQclear(result);

	result = PQexec(conn, "CREATE TYPE complex AS (t text, s simple, "
		"arr int4[][])");
	CMDOKAY("creating complex type");
	PQclear(result);

	if (!PQregisterTypes(conn, PQT_COMPOSITE, types, 2, 0))
	{
		fprintf(stderr, "  **ERROR: PQregisterTypes(%s %s): %s\n",
			types[0].typname, types[1].typname, PQgeterror());
		failcnt++;
		return;
	}

	result = PQexec(conn, "CREATE TABLE libpq_composite (comparr complex[])");
	CMDOKAY("creating libpq_composite table:");
	PQclear(result);

	simple = PQparamCreate(conn);
	complex = PQparamCreate(conn);
	prm = PQparamCreate(conn);

	/* array[10][3] */
	intarr.ndims     = 2;
	intarr.dims[0]   = 10;
	intarr.dims[1]   = 3;
	intarr.lbound[0] = 1;
	intarr.lbound[1] = 1;
	intarr.param = PQparamCreate(conn);

	/* If a 1D array is being used, you can avoid setting the dimension
	 * members of the PGarray structure by zeroing it.
	 */
	memset(&comparr, 0, sizeof(PGarray));
	comparr.param = PQparamCreate(conn);

	/* generate a complex[] containing 100 elements */
	for (i=0; i < 100; i++)
	{
		PGint8 val = ((PGint8)1<<48) + (PGint8)i;

		/* pack the fields of the 'simple' composite */
		r = PQputf(simple, "%int4 %int8", i+1000000, val);
		PUTOKAY(simple, r, "putting simple attributes");

		/* populate the complex.arr[10][3] */
		PQputf(intarr.param, "%null");
		for (x=1; x < 30; x++)
		{
			r= PQputf(intarr.param, "%int4", x+1);
			PUTOKAY(intarr.param, r, "putting complex.arr element");
		}

		/* pack the fields of the 'complex' composite */
		sprintf((char *) buf, "text_%03d", i+1);
		r = PQputf(complex, "%text %simple %int4[]", buf, simple, &intarr);
		PUTOKAY(complex, r, "putting complex attributes");

		/* now put element 'i' into the composite array */
		r = PQputf(comparr.param, "%complex", complex);
		PUTOKAY(comparr.param, r, "putting complex[] element");

		/* make sure to reset the params, otherwise you will continue to
		 * append parameters to below params.  DO NOT reset the comparr.param,
		 * as this is the array we want to insert.
		 */
		PQparamReset(simple);
		PQparamReset(intarr.param);
		PQparamReset(complex);

		/* sneak in a NULL element.  NULL composite array items
     * used to be a bug.
		 */
		if(i == 49)
		{
			PQputf(comparr.param, "%complex", NULL);
			i++;
		}
	}

	/* insert the complex[] */
	PQputf(prm, "%complex[]", &comparr);
	result = PQparamExec(conn, prm,
		"INSERT INTO libpq_composite VALUES ($1)", 1);
	CMDOKAY("inserting into libpq_composite");
	PQclear(result);

	/* clear all params (order doesn't matter, they are
	 * isolated structures)
	 */
	PQparamClear(simple);
	PQparamClear(intarr.param);
	PQparamClear(complex);
	PQparamClear(comparr.param);
	PQparamClear(prm);

	/* select complex[] back out */
	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_composite", 1);
	TUPSOKAY("PQparamExec(SELECT:composite[])");

	/* getf it into a PGarray */
	r = PQgetf(result, 0, "%complex[]", 0, &comparr);
	GETOKAY(r, "PQgetf(complex[])");
	PQclear(result);

	/* composite arrays come back 1 tuple per element and 1
	 * field per attribute
	 */
	ntups = PQntuples(comparr.res);
	if (ntups != 100)
	{
		failcnt++;
		fprintf(stderr, "put 100 elements into complex[] but "
			"got back %d\n", ntups);
		PQclear(comparr.res);
		return;
	}

	testcnt++;
	for (i=0; i < ntups; i++)
	{
		/* Verify element 50 is NULL */
		if(i==50)
		{
			/* All fields should be NULL */
	 		if(!PQgetisnull(comparr.res, i, 0) ||
				 !PQgetisnull(comparr.res, i, 1) ||
				 !PQgetisnull(comparr.res, i, 2))
			{
				failcnt++;
				fprintf(stderr, "  Element 50 should of been NULL\n");
				PQclear(comparr.res);
				return;
			}

			continue;
		}

		/* get the complex composite at tuple 'i' */
		r = PQgetf(comparr.res, i, "%text %simple #int4[]",
			0, &text, 1, &simple_res, "arr", &intarr);
		GETOKAY2(comparr.res, r, "PQgetf(comparr.res element)");

		/* check the text field */
		sprintf((char *) buf, "text_%03d", i+1);
		if (strcmp(text, (char *) buf))
		{
			failcnt++;
			fprintf(stderr, "complex[%d].t failed: sent=%s, recv=%s\n",
				i, buf, text);
			PQclear(simple_res);
			PQclear(intarr.res);
			PQclear(comparr.res);
			return;
		}

		/* Check the simple field: reference by fname, use '#'
		 * rather than '%'
		 */
		r = PQgetf(simple_res, 0, "#int4 #int8", "a", &a, "b", &b);
		GETOKAY2(simple_res, r, "PQgetf(complex[].simple.*)");
		PQclear(simple_res);

		/* verify simple.* values */
		if (a != (i+1000000) || b != ((PGint8) 1<<48)+i)
		{
			failcnt++;
			fprintf(stderr, "complex[%d].simple.* failed: "
				"sent(a=%d, b=%lld), recv(a=%d, b=%lld)\n",
				i, i+1000000, ((PGint8) 1<<48)+i, a, b);
			PQclear(intarr.res);
			PQclear(comparr.res);
			return;
		}

		/* check the int[10][3] field */
		if (intarr.dims[0] != 10 || intarr.dims[1] != 3 ||
			 intarr.lbound[0] != 1 || intarr.lbound[1] != 1 || intarr.ndims != 2)
		{
			failcnt++;
			fprintf(stderr, "complex[%d].int[][] dimension info failed: "
				"sent(ndims=2, arr[1:10][1:3]), recv(ndims=%d, arr[%d:%d][%d:%d])\n",
				i, intarr.ndims, intarr.lbound[0], intarr.dims[0],
				intarr.lbound[1], intarr.dims[1]);
			PQclear(intarr.res);
			PQclear(comparr.res);
			return;
		}

		inttups = PQntuples(intarr.res);
		for (x=1; x < inttups; x++)
		{
			r = PQgetf(intarr.res, x, "%int4", 0, &a);
			GETOKAY2(intarr.res, r, "PQgetf(complex.int[][] element");

			if (a != x+1)
			{
				failcnt++;
				fprintf(stderr, "  **ERROR: complex[%d].int4[][] sent=%d, recv=%d\n",
					i, x+1, a);
				PQclear(intarr.res);
				PQclear(comparr.res);
				return;
			}
		}

		PQclear(intarr.res);
	}

	PQclear(comparr.res);
	printf("  composite[] with nested composite and arrays passed\n");
}

static void test_natives(int format)
{
	int r;
	PGchar ca;
	PGchar cb;
	PGint2 i2a, i2b;
	PGint4 i4a, i4b;
	PGint8 i8a, i8b;
	PGfloat4 f4a, f4b;
	PGfloat8 f8a, f8b;
	PGmoney mona, monb;
	Oid oida, oidb;
	PGbool boola, boolb;

	PQparamReset(param);
	printf("\nNative C types: (%s)\n", format ? "binary" : "text");

	r = PQputf(param, "%char %char %int2 %int2 %int4 %int4 %int8 %int8 "
		"%float4 %float4 %float8 %float8 %money %money %oid %oid %bool %bool",
		-117, 224, -32000, 32000, INT_MIN, INT_MAX, LLONG_MIN, LLONG_MAX,
		-1.23456f, 1.23456f, -123456789.654321, 123456789.654321,
		(PGmoney)INT_MIN, 600000000054LL, 91982, 3000000000U, 0, 1);
	PUTOKAY(param, r, "PQputf(natives)");

	DROP_TABLE("libpq_natives");

	result = PQexec(conn, "CREATE TABLE libpq_natives ("
		"char_a \"char\", char_b \"char\", "
		"i2_a int2, i2_b int2, "
		"i4_a int4, i4_b int4, "
		"i8_a int8, i8_b int8, "
		"f4_a float4, f4_b float4, "
		"f8_a float8, f8_b float8, "
		"mon_a money, mon_b money, "
		"oid_a oid, oid_b oid, "
		"bool_a bool, bool_b bool)");
	CMDOKAY("creating libpq_natives table");
	PQclear(result);

	result = PQparamExec(conn, param, "INSERT INTO libpq_natives VALUES"
		"($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18)",
		format);
	CMDOKAY("PQparamExec(INSERT:natives)");
	PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_natives", format);
	TUPSOKAY("PQparamExec(SELECT:natives)");

	r = PQgetf(result, 0, "%char %char %int2 %int2 %int4 %int4 %int8 %int8 "
		"%float4 %float4 %float8 %float8 %money, %money %oid %oid %bool %bool",
		0, &ca, 1, &cb, 2, &i2a, 3, &i2b, 4, &i4a, 5, &i4b, 6, &i8a, 7, &i8b, 8,
		&f4a, 9, &f4b, 10, &f8a, 11, &f8b, 12, &mona, 13, &monb, 14, &oida,
		15, &oidb, 16, &boola, 17, &boolb);
	GETOKAY(r, "PQgetf(natives)");
	PQclear(result);

	CHKNUM("%char",   ca,    -117);
	CHKNUM("%char",   (unsigned char) cb,    224);
	CHKNUM("%int2",   i2a,   -32000);
	CHKNUM("%int2",   i2b,   32000);
	CHKNUM("%int4",   i4a,   INT_MIN);
	CHKNUM("%int4",   i4b,   INT_MAX);
	CHKNUM("%int8",   i8a,   LLONG_MIN);
	CHKNUM("%int8",   i8b,   LLONG_MAX);
	CHKNUM("%float4", f4a,   -1.23456f);
	CHKNUM("%float4", f4b,   1.23456f);
	CHKNUM("%float8", f8a,   -123456789.654321);
	CHKNUM("%float8", f8b,   123456789.654321);
	CHKNUM("%money",  mona,  (PGmoney) INT_MIN);
	CHKNUM("%money",  monb,  600000000054LL);
	CHKNUM("%oid",    oida,  91982);
	CHKNUM("%oid",    oidb,  3000000000U);
	CHKNUM("%bool",   boola, 0);
	CHKNUM("%bool",   boolb, 1);
}

static void test_geometrics(int format)
{
	int r;
	static PGpoint ptarr[6] = {{565677.342, 999112.33344},
		{-565677.342, -999112.33344}, {-565677.342, 999112.33344},
		{565677.342, 999112.33344}, {-565677.342, -999112.33344},
		{-565677.342, 999112.33344}};
	PGpoint point = {0};
	PGpoint pointval = {565677.342, 999112.33344};
	PGlseg lseg = {{{0}}};
	PGlseg lsegval = {{{565677.342, -999112.33344}, {88876.2314, 77283.323}}};
	PGbox box = {{0}};
	PGbox boxval = {{565677.342, 88876.2314}, {77283.323, -999112.33344}};
	PGcircle circle = {{0,0},0};
	PGcircle circleval = {{7765.435, -7743.2993}, 23384.102};
	PGpath path = {0, 0, NULL};
	PGpath pathval = {6, 1, ptarr};
	PGpolygon polygon = {0, NULL};
	PGpolygon polygonval = {6, ptarr};

	PQparamReset(param);
	printf("\nGeometric types: (%s)\n", format ? "binary" : "text");

	r = PQputf(param, "%point %lseg %box %circle %path %polygon",
		&pointval, &lsegval, &boxval, &circleval, &pathval, &polygonval);
	PUTOKAY(param, r, "PQputf(geos)");

	DROP_TABLE("libpq_geos");

	result = PQexec(conn, "CREATE TABLE libpq_geos ("
		"p point, l lseg, b box, c circle, pa path, poly polygon)");
	CMDOKAY("creating libpq_geos table");
	PQclear(result);

	result = PQparamExec(conn, param, "INSERT INTO libpq_geos VALUES"
		"($1,$2,$3,$4,$5,$6)", format);
	CMDOKAY("PQparamExec(INSERT:geos)");
	PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_geos", format);
	TUPSOKAY("PQparamExec(SELECT:geos)");

	r = PQgetf(result, 0, "%point %lseg %box %circle %path %polygon",
		0, &point, 1, &lseg, 2, &box, 3, &circle, 4, &path, 5, &polygon);
	GETOKAY(r, "PQgetf(geos)");

	CHKGEO(point);
	CHKGEO(lseg);
	CHKGEO(box);
	CHKGEO(circle);

	testcnt++;
	if (path.closed == pathval.closed && path.npts == pathval.npts &&
		 memcmp(path.pts, pathval.pts, path.npts * sizeof(PGpoint))==0)
		printf("  %%path - passed\n");
	else
	{
		failcnt++;
		fprintf(stderr, "  %%path - FAILED\n");
	}

	testcnt++;
	if (polygon.npts == polygon.npts &&
		 memcmp(polygon.pts, polygon.pts, polygon.npts * sizeof(PGpoint))==0)
		printf("  %%polygon - passed\n");
	else
	{
		failcnt++;
		fprintf(stderr, "  %%polygon - FAILED\n");
	}

	/* NOTE: this will invalidate path.pts and polygon.pts */
	PQclear(result);
}

/*
 * numeric is tested here because it is always exposed as a string.
 * uuid is not a varlen data type, but testing it here is ok
 */
static void test_varlen(int format)
{
	int r;
	static char byteadata[] = {0, 1, 2, 3, 4, 5, 6, 7};
	PGbpchar bpcharp=NULL;
	PGbpchar bpcharin = "character(n) blank-padded";
	PGvarchar varcharp=NULL;
	PGvarchar varcharin = "character varying(n)";
	PGtext textp=NULL;
	PGtext textin = "variable unlimited length";
	PGbytea byteap;
	PGbytea byteain = {8, byteadata};
	PGuuid uuid;
	char uuidin[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
	PGnumeric num;
	PGnumeric numin =
		"-62731893541288039212143296120112.12431212671229121291821928918211";

	PQparamReset(param);
	printf("\nVariable-length types: (%s)\n", format ? "binary" : "text");

	r = PQputf(param, "%bpchar %bpchar* %varchar %varchar* "
		"%text %text* %bytea %bytea* %uuid %numeric",
		bpcharin, bpcharin, varcharin, varcharin, textin, textin,
		&byteain, &byteain, uuidin, numin);
	PUTOKAY(param, r, "PQputf(varlen)");

	DROP_TABLE("libpq_varlen");

	result = PQexec(conn, "CREATE TABLE libpq_varlen ("
		"bp_a bpchar(32), bp_b bpchar(32), vc_a varchar(32), vc_b varchar(32), "
		"text_a text, text_b text, bytea_a bytea, bytea_b bytea, "
		"uid uuid, n numeric)");
	CMDOKAY("creating libpq_varlen table");
	PQclear(result);

	result = PQparamExec(conn, param, "INSERT INTO libpq_varlen VALUES"
		"($1,$2,$3,$4,$5,$6,$7,$8,$9,$10)", format);
	CMDOKAY("PQparamExec(INSERT:varlen)");
	PQclear(result);

	result = PQparamExec(conn, NULL,
		"SELECT bp_a,vc_a,text_a,bytea_a,uid,n FROM libpq_varlen", format);
	TUPSOKAY("PQparamExec(SELECT:varlen)");

	r = PQgetf(result, 0,
		"%bpchar %varchar %text %bytea %uuid %numeric",
		0, &bpcharp,     /* field_num, PGbpchar* */
		1, &varcharp,    /* field_num, PGvarchar* */
		2, &textp,       /* field_num, PGtext* */
		3, &byteap,      /* field_num, PGbytea* */
		4, &uuid,        /* field_num, PGuuid* */
		5, &num);        /* field_num, PGnumeric* */
	GETOKAY(r, "PQgetf(varlen)");

	/* Because we are using the '*' specifier flag, we clear the results at
	 * the end.  '*' flag is just a pointer to the result: like PQgetvalue().
	 */

	testcnt++;
	if (bpcharcmp(bpcharin, bpcharp, 32))
	{
		failcnt++;
		fprintf(stderr, "  %%bpchar - FAILED\n");
	}
	else
		printf("  %%bpchar - passed\n");

	CHKVLEN("varchar",  varcharp, varcharin);
	CHKVLEN("text",     textp, textin);

	testcnt++;
	if (byteain.len != byteap.len ||
	   memcmp(byteap.data, byteain.data, byteap.len))
	{
		failcnt++;
		fprintf(stderr, "  %%bytea* - FAILED\n");
	}
	else
		printf("  %%bytea* - passed\n");

	testcnt++;
	if (memcmp(uuid, uuidin, 16))
	{
		failcnt++;
		fprintf(stderr, "  %%uuid - FAILED\n");
	}
	else
		printf("  %%uuid - passed\n");

	CHKVLEN("numeric", num, numin);
	PQclear(result);
}

static void test_datetime(int format)
{
	int r;
	time_t t;
	struct tm *tm;
	char *tzn;
	PGdate date[3] = {{0}};
	static PGdate dateval[] = {
		{0, 1476,  9,  1, 2260432, 274, 0},         /* 1476-10-01 */
		{1, 1209,  7, 17, 1280076, 229, 1},         /* 1209-08-17 BC */
		{0, 2007, 11, 23, 2454458, 356, 0}};        /* 2007-12-23 */
	PGtime tim[3] = {{0}};
	static PGtime timeval[] = {
		{23, 10, 31,   7214, 0, -1, 0, {0}},        /* 23:10:31.007214 */
		{ 2, 19, 52,      0, 0, -1, 0, {0}},        /* 02:19:52 */
		{15, 57,  7, 881542, 0, -1, 0, {0}}};       /* 15:57:07.881542 */
	PGtime timetz[3] = {{0}};
	PGtime timetzval[] = {
		{23, 10, 31,   9888, 1, -1, -28800, {0}},   /* 23:10:31.009888-08 */
		{ 2, 19, 52,      0, 1, -1,   7200, {0}},   /* 02:19:52+02 */
		{15, 57,  7, 881542, 1, -1, -10800, {0}}};  /* 15:57:07.881542-03 */
	PGtimestamp timestamp[3] = {{0, {0}, {0}}};
	PGtimestamp timestampval[] = {
		{ -15565394969, {0, 1476,  9,  1, 2260432, 274, 0},
			{23, 10, 31, 7214, 0, -1, 0, {0}}},   /* 1476-10-01 23:10:31.007214 */
		{-100268228408, {1, 1209,  7, 17, 1280076, 229, 1},
			{ 2, 19, 52, 0, 0, -1, 0, {0}}},	  /* 1209-08-17 02:19:52 BC */
		{   1198425427, {0, 2007, 11, 23, 2454458, 356, 0},
			{15, 57, 7, 881542, 0, -1, 0, {0}}}};	/* 2007-12-23 15:57:07.881542 */
	PGinterval interval = {0};
	PGinterval intervalval = /* 2,354 years 8 mons 23 days 14:54:27.029333 */
		{2354, 8, 23, 14, 54, 27, 29333};
	PGtimestamp timestamptz;
	PGtimestamp timestamptzval;

	PQparamReset(param);
	memset(&timestamptz, 0, sizeof(PGtimestamp));
	memset(&timestamptzval, 0, sizeof(PGtimestamp));

	/* Jan 1, 1980 (returned: text=server time zone, binary=localtime) */
	t = 315532800;
	tm = localtime(&t);
	timestamptzval.epoch       = t;
	timestamptzval.time.hour   = tm->tm_hour;
	timestamptzval.time.min    = tm->tm_min;
	timestamptzval.time.sec    = tm->tm_sec;
	timestamptzval.time.usec   = 0;
	timestamptzval.time.withtz = 1;
	timestamptzval.date.isbc   = 0;
	timestamptzval.date.year   = tm->tm_year + 1900;
	timestamptzval.date.mon    = tm->tm_mon;
	timestamptzval.date.mday   = tm->tm_mday;
	PQlocalTZInfo(&t, &timestamptzval.time.gmtoff,
		&timestamptzval.time.isdst, &tzn);
	strcpy(timestamptzval.time.tzabbr, tzn);

	strcpy(timetzval[0].tzabbr, "GMT-0800");
	strcpy(timetzval[1].tzabbr, "GMT+0200");
	strcpy(timetzval[2].tzabbr, "GMT-0300");

	printf("\nDate & Time types: (%s '%s')\n",
		format ? "binary" : "text", datestyle);

	r = PQputf(param, "%date %date %date %time %time %time %timetz %timetz "
		"%timetz %timestamp %timestamp %timestamp %interval %timestamptz",
		&dateval[0], &dateval[1], &dateval[2], &timeval[0], &timeval[1],
		&timeval[2], &timetzval[0], &timetzval[1], &timetzval[2],
		&timestampval[0], &timestampval[1], &timestampval[2], &intervalval,
		&timestamptzval);
	PUTOKAY(param, r, "PQputf(datetime)");

	DROP_TABLE("libpq_datetime");

	result = PQexec(conn, "CREATE TABLE libpq_datetime ("
		"date_a date, date_b date, date_c date, "
		"time_a time, time_b time, time_c time, "
		"timetz_a timetz, timetz_b timetz, timetz_c timetz, "
		"timestamp_a timestamp, timestamp_b timestamp, timestamp_c timestamp, "
		"intvl interval, tstz timestamptz)");
	CMDOKAY("creating libpq_datetime table");
	PQclear(result);

	result = PQparamExec(conn, param, "INSERT INTO libpq_datetime VALUES"
		"($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14)", format);
	CMDOKAY("PQparamExec(INSERT:datetime)");
	PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_datetime", format);
	TUPSOKAY("PQparamExec(SELECT:datetime)");

	r = PQgetf(result, 0, "%date %date %date %time %time %time %timetz "
		"%timetz %timetz %timestamp %timestamp %timestamp %interval "
		"%timestamptz",
		 0, &date[0],       1, &date[1],       2, &date[2],
		 3, &tim[0],        4, &tim[1],        5, &tim[2],
		 6, &timetz[0],     7, &timetz[1],     8, &timetz[2],
		 9, &timestamp[0], 10, &timestamp[1], 11, &timestamp[2],
		12, &interval,     13, &timestamptz);
	GETOKAY(r, "PQgetf(datetime)");
	PQclear(result);

	for (r=0; r < 3; r++)
	{
		testcnt++;
		if (memcmp(&date[r], &dateval[r], sizeof(PGdate)) == 0)
			printf("  %%date - #%d passed\n", r+1);
		else
		{
			failcnt++;
			fprintf(stderr, "  %%date - #%d FAILED\n", r+1);
		}
	}

	for (r=0; r < 3; r++)
	{
		testcnt++;
		if (memcmp(&tim[r], &timeval[r], sizeof(PGtime)) == 0)
			printf("  %%time - #%d passed\n", r+1);
		else
		{
			failcnt++;
			fprintf(stderr, "  %%time - #%d FAILED\n", r+1);
		}
	}

	for (r=0; r < 3; r++)
	{
		testcnt++;
		if (memcmp(&timetz[r], &timetzval[r], sizeof(PGtime)) == 0)
			printf("  %%timetz - #%d passed\n", r+1);
		else
		{
			failcnt++;
			fprintf(stderr, "  %%timetz - #%d FAILED\n", r+1);
		}
	}

	for (r=0; r < 3; r++)
	{

		testcnt++;
		if (memcmp(&timestamp[r], &timestampval[r], sizeof(PGtimestamp)) == 0)
		{
			printf("  %%timestamp - #%d passed\n", r+1);
		}
		/* Windows may hit rounding issues with usecs.  In this test, usecs of
		 * 7214 were coming back as 7215.
		 */
#if defined(_WIN32) || defined(_WIN64)
		else if (timestamp[r].time.usec+1 == timestampval[r].time.usec ||
			 timestamp[r].time.usec-1 == timestampval[r].time.usec)
		{
			printf("  %%timestamp - #%d passed (warning: known usec rounding "
				"issue - sent=%d, recv=%d)\n", r+1,
				timestampval[r].time.usec, timestamp[r].time.usec);
		}
#endif
		else
		{
			failcnt++;
			fprintf(stderr, "  %%timestamp - #%d FAILED\n", r+1);
		}
	}

	/* interval usecs won't match when using a non-ISO datastyle because only
	 * 2 fractional digits are provided: 23.291876 => 23.29 secs.
	 */
	if (format == 0 && !strstr(datestyle, "ISO"))
	{
		printf("  **NOTE: non-ISO DateStyle interval text has a 2 digit limit "
			"on the microsecond value ... adjusting\n    => sent=%d, "
			"received=%d\n", intervalval.usecs, interval.usecs);
		interval.usecs = intervalval.usecs;
	}

	testcnt++;
	if (memcmp(&interval, &intervalval, sizeof(PGinterval)) == 0)
		printf("  %%interval - passed\n");
	else
	{
		failcnt++;
		fprintf(stderr, "  %%interval - FAILED\n");

		printf("    SENT: yr=%d,mon=%d,days=%d,hr=%d,min=%d,sec=%d,usec=%d\n",
			intervalval.years, intervalval.mons, intervalval.days,
			intervalval.hours, intervalval.mins, intervalval.secs,
			intervalval.usecs);
		printf("    RECV: yr=%d,mon=%d,days=%d,hr=%d,min=%d,sec=%d,usec=%d\n",
			interval.years, interval.mons, interval.days,
			interval.hours, interval.mins, interval.secs, interval.usecs);
	}

	testcnt++;

	if (timestamptz.epoch == timestamptzval.epoch)
		printf("  %%timestamptz - passed\n");
	else
	{
		failcnt++;
		fprintf(stderr, "  %%timestamptz - FAILED\n");

		printf("SENT: epoch=%lld,isbc=%d,yr=%d,mon=%d,mday=%d,jday=%d,"
			"yday=%d,wday=%d,hr=%d,min=%d,sec=%d,usec=%d,withtz=%d,"
			"isdst=%d,tzabbr=%s,gmtoff=%d\n",
			timestamptzval.epoch,
			timestamptzval.date.isbc,timestamptzval.date.year,
				timestamptzval.date.mon,
			timestamptzval.date.mday,timestamptzval.date.jday,
				timestamptzval.date.yday,
			timestamptzval.date.wday,
			timestamptzval.time.hour, timestamptzval.time.min,
				timestamptzval.time.sec,
			timestamptzval.time.usec, timestamptzval.time.withtz,
				timestamptzval.time.isdst,
			timestamptzval.time.tzabbr, timestamptzval.time.gmtoff);

		printf("RECV: epoch=%lld,isbc=%d,yr=%d,mon=%d,mday=%d,jday=%d,"
			"yday=%d,wday=%d,hr=%d,min=%d,sec=%d,usec=%d,withtz=%d,isdst=%d,"
			"tzabbr=%s,gmtoff=%d\n",
			timestamptz.epoch,
			timestamptz.date.isbc,timestamptz.date.year,timestamptz.date.mon,
			timestamptz.date.mday,timestamptz.date.jday,timestamptz.date.yday,
			timestamptz.date.wday,
			timestamptz.time.hour, timestamptz.time.min, timestamptz.time.sec,
			timestamptz.time.usec, timestamptz.time.withtz, timestamptz.time.isdst,
			timestamptz.time.tzabbr, timestamptz.time.gmtoff);
	}
}

static void test_network(int format)
{
	int r;
	PGinet ipv4, ipv4out;
	PGinet ipv6, ipv6out;
	PGmacaddr mac = {1,2,3,225,226,227};
	PGmacaddr macout;
	int have_ipv4=1;
	int have_ipv6=1;

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(HAVE_GETADDRINFO)
	struct addrinfo *info = NULL;
	struct addrinfo hints = {0};

	memset(&ipv4, 0, sizeof(PGinet));
	memset(&ipv4out, 0, sizeof(PGinet));
	memset(&ipv6, 0, sizeof(PGinet));
	memset(&ipv6out, 0, sizeof(PGinet));

	PQparamReset(param);
	printf("\nNetwork types: (%s)\n", format ? "binary" : "text");

	/* this should always work */
	if ((r = getaddrinfo("192.168.123.77", NULL, NULL, &info)))
	{
		have_ipv4 = 0;
		//testcnt++;
		//failcnt++;
		fprintf(stderr, "  IPv4: SKIPPING TEST (no support)\n");
	}
	else
	{
		ipv4.mask = 32;
		ipv4.sa_buf_len = (int)info->ai_addrlen;
		ipv4.is_cidr = 0;
		memcpy(ipv4.sa_buf, info->ai_addr, ipv4.sa_buf_len);
		memset(ipv4.sa_buf, 0, 2);
		((struct sockaddr *) ipv4.sa_buf)->sa_family = AF_INET;
		freeaddrinfo(info);
	}

#ifdef AF_INET6
	hints.ai_family = AF_INET6;
#endif

	/* windows may choke on an IPv6 address */
	if ((r = getaddrinfo("2001:4f8:3:ba:2e0:81ff:fe22:d1f1",
		NULL, &hints, &info)))
	{
		have_ipv6 = 0;
		//testcnt++;
		//failcnt++;
		fprintf(stderr, "  IPv6: SKIPPING TEST (no support - [%d] %s)\n",
			r, gai_strerror(r));
	}
	else
	{
		ipv6.mask = 32;
		ipv6.sa_buf_len = (int)info->ai_addrlen;
		ipv6.is_cidr = 0;
		memcpy(ipv6.sa_buf, info->ai_addr, ipv6.sa_buf_len);

#ifdef AF_INET6
		memset(ipv6.sa_buf, 0, 2);
    ((struct sockaddr *) ipv6.sa_buf)->sa_family = AF_INET6;
#endif

		freeaddrinfo(info);
	}

	r = have_ipv4 ? PQputf(param, "%inet", &ipv4) : PQputf(param, "%null");
	PUTOKAY(param, r, "PQputf(network-ipv4)");

	r = have_ipv6 ? PQputf(param, "%inet", &ipv6) : PQputf(param, "%null");
	PUTOKAY(param, r, "PQputf(network-ipv6)");

	r = PQputf(param, "%macaddr", &mac);
	PUTOKAY(param, r, "PQputf(network-macaddr)");

	DROP_TABLE("libpq_network");

	result = PQexec(conn, "CREATE TABLE libpq_network ("
		"ipv4 inet, ipv6 inet, mac macaddr)");
	CMDOKAY("creating libpq_network table");
	PQclear(result);

	result = PQparamExec(conn, param,
		"INSERT INTO libpq_network VALUES ($1,$2,$3)", format);
	CMDOKAY("PQparamExec(INSERT:network)");
	PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_network", format);
	TUPSOKAY("PQparamExec(SELECT:network)");

	r = PQgetf(result, 0, "%inet %inet %macaddr",
		0, &ipv4out, 1, &ipv6out, 2, &macout);
	GETOKAY(r, "PQgetf(network)");

	PQclear(result);

	if (have_ipv4)
	{
		if (memcmp(&ipv4out, &ipv4, sizeof(PGinet)))
		{
			failcnt++;
			fprintf(stderr, "  %%inet - IPv4 FAILED\n");
		}
		else
			printf("  %%inet - IPv4 passed\n");
	}

	if (have_ipv6)
	{
		if (memcmp(&ipv6out, &ipv6, sizeof(PGinet)))
		{
			failcnt++;
			fprintf(stderr, "  %%inet - IPv6 FAILED\n");
		}
		else
			printf("  %%inet - IPv6 passed\n");
	}

	testcnt++;
	if (memcmp(&macout, &mac, sizeof(PGmacaddr)))
	{
		failcnt++;
		fprintf(stderr, "  %%macaddr - FAILED\n");
	}
	else
		printf("  %%macaddr - passed\n");
#else
	printf("\nNetwork types: (%s) - skipping, no getaddrinfo()\n",
		format ? "binary" : "text");
#endif
}

/* PGtypeProc put for epoch sub-class */
static int epoch_put(PGtypeArgs *args)
{
	PGtimestamp ts;
	time_t t = va_arg(args->ap, time_t);
	struct tm *tm = localtime(&t);

	printf("  epoch_put('%ld')\n", t);

	ts.date.isbc = 0;
	ts.date.year = tm->tm_year + 1900; /* PGtimestamp requires 4-digit year */
	ts.date.mon  = tm->tm_mon;
	ts.date.mday = tm->tm_mday;
	ts.time.hour = tm->tm_hour;
	ts.time.min  = tm->tm_min;
	ts.time.sec  = tm->tm_sec;
	ts.time.usec = 0;

	PQlocalTZInfo(&t, &ts.time.gmtoff, &ts.time.isdst, NULL);
	return args->super(args, &ts);
}

/* PGtypeProc get for epoch sub-class */
static int epoch_get(PGtypeArgs *args)
{
	PGtimestamp ts;
	time_t *t = va_arg(args->ap, time_t *);

	printf("  epoch_get(%s)\n", args->format==0 ? "text" : "binary");

	if (args->super(args, &ts) == -1)
		return -1; /* args->errorf called by super class */

	/* since PGtimestamp contains an epoch member as an int8, we can
	 * just copy that value, rather than doing a mktime().  If time_t
	 * is 32-bits, the below has a limited range compared to an int8.
	 * Although, this will work fine for timestamps in a 32-bit time_t range.
	 * 64-bit time_t should always work.
	 */
	*t = (time_t)ts.epoch;
	return 0;
}

static void test_subclass(int format)
{
	int r;
	PGparam *prm;
	time_t timeout[2] = {0};
	/* 1970-01-01 00:00:00, 2000-01-01 00:00:00 */
	time_t timevals[] = {0, 946702800};

	printf("\nSub-class: (%s)\n", format ? "binary" : "text");

	DROP_TABLE("libpq_subclass");
	result = PQexec(conn,
		"CREATE TABLE libpq_subclass (n timestamptz, a timestamptz)");
	CMDOKAY("creating libpq_subclass table");
	PQclear(result);

	prm = PQparamCreate(conn);
	if (!PQputf(prm, "%epoch %epoch", timevals[0], timevals[1]))
		fprintf(stderr, "subclass_put failed: %s\n", PQgeterror());
	result = PQparamExec(conn, prm, "INSERT INTO libpq_subclass "
		"VALUES ($1, $2)", format);
	PQparamClear(prm);
	CMDOKAY("PQparamExec(INSERT:subclass)");
	PQclear(result);

	result = PQparamExec(conn, NULL, "SELECT * FROM libpq_subclass", format);
	TUPSOKAY("PQparamExec(SELECT:subclass)");

	/* use schema-qualified name */
	r = PQgetf(result, 0, "%libpq.epoch %epoch",
		0, &timeout[0], 1, &timeout[1]);
	GETOKAY(r, "PQgetf(subclass)");
	PQclear(result);

	testcnt++;
	if (memcmp(timeout, timevals, sizeof(timeout)))
	{
		failcnt++;
		fprintf(stderr, "  %%epoch - FAILED (putval=%ld,%ld, getval=%ld,%ld)\n",
			timevals[0], timevals[1], timeout[0], timeout[1]);
	}
	else
		fprintf(stderr, "  %%epoch - passed\n");
}


