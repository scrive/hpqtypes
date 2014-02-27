
/*
 * libpqtypes-int.h
 *   Private header file for libpqtypes.  All source files include
 *   this header.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#ifndef LIBPQTYPES_INT_H
#define LIBPQTYPES_INT_H

#define PLN do{ \
	printf("%s:%d\n", __FUNCTION__, __LINE__); \
	fflush(stdout); \
}while(0)

#ifdef HAVE_CONFIG_H
#	include "pqt_config.h"
#endif

/* Using the windows compiler */
#ifdef _MSC_VER
#	define PQT_MSVC _MSC_VER
#endif

/* We can/should use the Windows API */
#if defined(PQT_MSVC) || defined(__MINGW32__)
#	define PQT_WINAPI
#endif

/* Compiling on a windows platform */
#if defined(PQT_WINAPI) || defined(__CYGWIN__)
#	define PQT_WIN32
#endif

/* WINAPI is available, include needed winsock headers */
#ifdef PQT_WINAPI
#	ifdef PQT_MSVC
# 	pragma warning (disable : 4706 4100 4711 4127 4702)
#		pragma warning (disable: 4706 4100 4514 4710 4201 4206)
#	endif
# include <winsock2.h>
# include <ws2tcpip.h>
#	include <Wspiapi.h> /* need for getaddrinfo (2000 and below */
#	include <windows.h>
#else
#	if defined(__CYGWIN__) || defined(HAVE_SYS_TYPES_H)
#		include <sys/types.h>
#	endif
#	if defined(__CYGWIN__) || defined(HAVE_SYS_SOCKET_H)
#		include <sys/socket.h>
#	endif
#	if defined(__CYGWIN__) || defined(HAVE_NETDB_H)
#		include <netdb.h>
#	endif
#	if defined(__CYGWIN__) || defined(HAVE_NETINET_IN_H)
#		include <netinet/in.h>
#	endif
#	if defined(__CYGWIN__) || defined(HAVE_ARPA_INET_H)
#		include <arpa/inet.h>
#	endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <errno.h>

/* string*.h, windows platforms just include string.h */
#ifdef PQT_WIN32
#	include <string.h>
#else
#	ifdef HAVE_STRING_H
#		include <string.h>
#	endif
#	ifdef HAVE_STRINGS_H
#		include <strings.h>
#	endif
#endif

/* Include stddef.h if on windows or if we have it */
#if defined(PQT_WIN32) || defined(HAVE_STDDEF_H)
#	include <stddef.h>
#endif

/* Include limits.h if on windows or if we have it */
#if defined(PQT_WIN32) || defined(HAVE_LIMITS_H)
#	include <limits.h>
#endif

/* Include linux/limits.h if available */
#if defined(HAVE_CONFIG_H) && defined(HAVE_LINUX_LIMITS_H)
#	include <linux/limits.h>
#endif

/* Include math.h if on windows or if we have it */
#if defined(PQT_WIN32) || defined(HAVE_MATH_H)
#	include <math.h>
#endif

/* Include the public API, pulls in libpq-fe.h for us. */
#include "libpqtypes.h"
#include "libpq-events.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef TRUE
#	define TRUE 1
#endif

#ifndef FALSE
#	define FALSE 0
#endif

#ifndef NULL_LEN
#	define NULL_LEN (-1)
#endif

/* Win32 builds use _timezone and _tzname: manually define HAVE_TZNAME. */
#ifdef PQT_WIN32
#	define HAVE_TZNAME
#	define pqt_timezone _timezone
#	define pqt_tzname   _tzname
#else
#	define pqt_timezone timezone
#	define pqt_tzname   tzname
#endif

#define PQT_OUTOFMEMORY "Out of memory error"
#define PQT_MAXIDLEN 64

#define TEXTFMT 0
#define BINARYFMT 1
#define TYPFLAG_CASEFOLD 0x01
#define TYPFLAG_ARRAY    0x02
#define TYPFLAG_POINTER  0x04
#define TYPFLAG_INVALID  0x08
#define TYPFLAG_BYNAME   0x10

/* Avoid passing NULL to realloc, some systems don't support it */
#define pqt_realloc(ptr, size) (ptr) ? realloc(ptr, size) : malloc(size)

#define countof(array) (int) (sizeof(array) / sizeof(array[0]))

/* MSVC 8 deprecated quite a few POSIX names */
#if defined(PQT_MSVC) && PQT_MSVC >= 1400
#	undef strdup
#	undef sscanf
#	define strdup _strdup
#	define sscanf sscanf_s
#endif

/* define va_copy */
#ifndef va_copy
#	ifdef __va_copy
#		define va_copy(dest, src) __va_copy(dest, src)
#	else
#		define va_copy(dest, src) (dest) = (src)
#	endif
#endif

#define pqt_hex_to_dec(v) \
	(unsigned char) (((v) > '9') ? ((v) - 'a') + 10 : (v) - '0')

/* --------------------------------
 * Macros used by built-in handlers
 */

/* used by get handlers */
#define DECLLENGTH(args) \
	int valuel = PQgetlength((args)->get.result, (args)->get.tup_num, \
		(args)->get.field_num)

/* used by get handlers */
#define DECLVALUE(getargs) \
	char *value = PQgetvalue((getargs)->get.result, (getargs)->get.tup_num, \
		(getargs)->get.field_num)

/* used by put handlers */
#define PUTNULLCHK(args, valp) do{ \
	if (!(valp)) \
		return pqt_put_null(args); \
}while (0)

/* used by get handlers, checks for NULL pointers and PQgetisnull */
#define CHKGETVALS(args, outp) do{ \
	if (!(outp)) \
		RERR(args, "output pointer is NULL"); \
	memset(outp, 0, sizeof(*(outp))); \
	if (PQgetisnull((args)->get.result, (args)->get.tup_num, \
			(args)->get.field_num)) \
		return 0; \
}while (0)

/* RERR for return error: errorf always returns -1 */
#define RERR(_args, msg) return (_args)->errorf(_args, msg)
#define RERR_STR2INT(args) RERR(args, "String to integer conversion failed")
#define RERR_MEM(args) RERR(args, PQT_OUTOFMEMORY)

#ifdef STRICT_MEMORY_ALIGNMENT
#	define pqt_buf_putint2(_buffer, _val) do{ \
		short _v = (short) htons((short) (_val)); \
		memcpy(_buffer, &_v, sizeof(short)); \
	} while (0)

#	define pqt_buf_putint4(_buffer, _val) do{ \
		int _v = (int) htonl((int) (_val)); \
		memcpy(_buffer, &_v, sizeof(int)); \
	} while (0)

	short pqt_buf_getint2(char *buffer);
	int pqt_buf_getint4(char *buffer);
#else
#	define pqt_buf_putint2(_out, _val) \
	*(short *) (_out) = (short) htons((short) (_val))
#	define pqt_buf_getint2(_buffer) (short) ntohs(*(short *) (_buffer))
#	define pqt_buf_putint4(_out, _val) \
	*(int *) (_out) = (int) htonl((int) (_val))
#	define pqt_buf_getint4(_buffer) (int) ntohl(*(int *) (_buffer))
#endif

/* ----------------------------------
 * See: src/include/catalog/pg_type.h
 * Used by built-in type handlers
 */

/* numerics types */
#define INT2OID            21
#define INT4OID            23
#define INT8OID            20
#define FLOAT4OID         700
#define FLOAT8OID         701
#define NUMERICOID       1700
/* geo types */
#define POINTOID          600
#define LSEGOID           601
#define PATHOID           602
#define BOXOID            603
#define POLYGONOID        604
#define LINEOID           628 /* not supported yet */
#define CIRCLEOID         718
/* network types */
#define INETOID           869
#define CIDROID           650
#define MACADDROID        829
/* variable length types */
#define BPCHAROID        1042
#define VARCHAROID       1043
#define NAMEOID            19
#define TEXTOID            25
#define ZPBITOID         1560 /* not supported yet */
#define VARBITOID        1562 /* not supported yet */
#define BYTEAOID           17
/* date and time types */
#define DATEOID          1082
#define TIMEOID          1083
#define TIMETZOID        1266
#define TIMESTAMPOID     1114
#define TIMESTAMPTZOID   1184
#define INTERVALOID      1186
/* misc types */
#define CHAROID            18
#define BOOLOID            16
#define OIDOID             26
#define CASHOID           790
#define RECORDOID        2249
#define UUIDOID          2950

/* --------------------------------
 * Private Structures
 */

/* Represents a param value.  An array of PGvalues was choosen over
 * allocating 4 separate arrays: oids, values, lengths, formats.  Instead
 * of 4 allocations you only have one.  The PGvalue array can easily be
 * converted to 4 arrays for execution.
 */
typedef struct
{
	int ptrl;   /* Length of value's pointer */
	void *ptr;  /* value pointer, data member uses this for non-NULL values */
	int datal;  /* Length of current value: always <= ptrl */
	char *data; /* current value data, can be NULL ... thus the ptr member. */
	int format; /* format: 0=text, 1=binary */
	Oid oid;    /* Oid of the data */
} PGvalue;

/* performance driven structure.  Instead of parsing and looking up
 * type specs each put or get, PQspecPrepare can be used to compile
 * a spec format string into a PGtypeSpec object.  The biggest wins are
 * large results sets and arrays.
 */
typedef struct
{
	char *name;           /* name for the prepared spec, used for lookup. */
	char *stmt;           /* parameterized version of stmt, if supplied */
	int idcnt;            /* number of handler ids */
	int *idlist;          /* handler ids */
	unsigned char *flags; /* handler flags */
} PGtypeSpec;

/* PGparam structure */
struct pg_param
{
	int vcnt;         /* current number of param values */
	int vmax;         /* number of PGvalue structs in 'vals' array. */
	PGvalue *vals;    /* array of param values, grown when needed */

	/* assigned during PQparamCreate */
	PGtypeFormatInfo fmtinfo;
	int typhcnt;
	PGtypeHandler *typhandlers;

	int typspeccnt;
	PGtypeSpec *typspecs;
};

typedef struct
{
	PGtypeFormatInfo fmtinfo;
	int typhcnt;
	int typhmax;
	PGtypeHandler *typhandlers;

	int typspeccnt;
	int typspecmax;
	PGtypeSpec *typspecs;
} PGtypeData;

/* --------------------------------
 * Internal API functions (not exported)
 */

/* === in events.c === */

/* The libpq PGEventProc */
int pqt_eventproc(PGEventId id, void *info, void *passThrough);

void pqt_cleartypes(PGtypeData *typeData);

/* === in param.c === */

int pqt_putparam(PGparam *param, PGerror *err, const void *data, int datal,
	int flags, int format, Oid typoid);

/* === in spec.c === */

PGtypeSpec *pqt_getspec(PGtypeSpec *specs, int count, const char *name);
PGtypeSpec *pqt_dupspecs(PGtypeSpec *specs, int count);
void pqt_clearspec(PGtypeSpec *cache);
void pqt_freespecs(PGtypeSpec *specs, int count);

char *pqt_parse(PGerror *err, const char *format, PGtypeHandler *h, int hcnt,
	char *stmtBuf, size_t stmtBufLen, PGtypeHandler **out, size_t *stmtPos,
	int *typpos, int *flags);

char *pqt_parsetype(PGerror *err, const char *spec, char *schema, char *typname,
	int *flags, int typpos);

/* === in handler.c === */

int pqt_allowsptr(PGtypeHandler *h);
void pqt_getfmtinfo(const PGconn *conn, PGtypeFormatInfo *info);
PGtypeHandler *pqt_duphandlers(PGtypeHandler *handlers, int hcnt);
void pqt_freehandlers(PGtypeHandler *handlers, int hcnt);
int pqt_argssuper(PGtypeArgs *args, ...);
int pqt_argserrorf(PGtypeArgs *args, const char *format, ...);

PGtypeHandler *pqt_gethandler(PGtypeHandler *handlers, int hcnt,
	const char *schema, const char *typname);

PGtypeHandler *pqt_gethandlerbyid(PGtypeHandler *handlers,
	int hcnt, int id);

PGtypeHandler *pqt_gethandlerbyoid(PGtypeHandler *handlers,
	int hcnt, Oid oid);

/* FQTN standards for Fully Qualified Type Name.  Returns a pointer to out.
 * Only returns NULL if out is NULL or outl <= 0.
 */
char *pqt_fqtn(char *out, size_t outl,
	const char *schema, const char *typname);

/* == in utils.c == */

PGresult *pqt_copyresult(PGtypeArgs *args, int nattrs);
void pqt_swap8(void *outp, void *inp, int tonet);
int pqt_text_to_int8(char *val, void *out);
int pqt_text_to_float8(double *f8, char *text, char **endptr);

/* Checks buffer and truncates 'src' if 'dest' is too small. */
char *pqt_strcpy(char *dest, size_t size, const char *src);

/* Taken from postgres project (named pg_xxx) */
unsigned char pqt_tolower(unsigned char ch);
int pqt_strcasecmp(const char *s1, const char *s2);
int pqt_strncasecmp(const char *s1, const char *s2, size_t n);

/* == in port.c == */

/* Define a strtoll macro for windows, except for MinGW & Cygwin
 * which always have it.  MSVC has _strtoi64, but MSVC 6 and under
 * require that we declare the prototype.
 */
#ifdef PQT_MSVC
#	if PQT_MSVC <= 1200
		__int64 __cdecl _strtoi64(const char *, char **, int);
#	endif
# define strtoll _strtoi64
#endif

/* In rare cases these could be absent (older platforms).  We implement
 * drop-in replacements in port.c for those cases.  strtod handled by
 * configure, which adds a replacement for us.
 *
 * NOTE: only unixes use configure so sections wrapped around
 * HAVE_CONFIG_H exclude windows.
 */
#ifdef HAVE_CONFIG_H
#	ifndef HAVE_STRTOL
		long strtol(const char *nptr, char **endptr, int base);
#	endif
#	ifndef HAVE_STRTOUL
		unsigned long strtoul(const char *nptr, char **endptr, int base);
#	endif
#	ifndef HAVE_STRTOLL
		long long int strtoll(const char *nptr, char **endptr, int base);
#	endif
#endif

/* Handles platform differences and always returns -1 for failure.
 * On windows: MSVC 8 uses _snprintf_s, otherwise _snprintf.
 */
int pqt_snprintf(char *buf, size_t size, const char *format, ...);
int pqt_vsnprintf(char *buf, size_t size, const char *format, va_list ap);

/* --------------------------------
 * Built-in Type Handlers
 */

/* === in array.c === */

int pqt_put_array(PGtypeArgs *args);
int pqt_get_array(PGtypeArgs *args);

/* == in datetime.c == */

int pqt_put_date(PGtypeArgs *args);
int pqt_get_date(PGtypeArgs *args);
int pqt_put_time(PGtypeArgs *args);
int pqt_get_time(PGtypeArgs *args);
int pqt_put_timetz(PGtypeArgs *args);
int pqt_get_timetz(PGtypeArgs *args);
int pqt_put_timestamp(PGtypeArgs *args);
int pqt_get_timestamp(PGtypeArgs *args);
int pqt_put_timestamptz(PGtypeArgs *args);
int pqt_get_timestamptz(PGtypeArgs *args);
int pqt_put_interval(PGtypeArgs *args);
int pqt_get_interval(PGtypeArgs *args);

/* == in geo.c == */

int pqt_put_point(PGtypeArgs *args);
int pqt_get_point(PGtypeArgs *args);
int pqt_put_lseg(PGtypeArgs *args);
int pqt_get_lseg(PGtypeArgs *args);
int pqt_put_box(PGtypeArgs *args);
int pqt_get_box(PGtypeArgs *args);
int pqt_put_circle(PGtypeArgs *args);
int pqt_get_circle(PGtypeArgs *args);
int pqt_put_path(PGtypeArgs *args);
int pqt_get_path(PGtypeArgs *args);
int pqt_put_polygon(PGtypeArgs *args);
int pqt_get_polygon(PGtypeArgs *args);

/* == in misc.c == */

int pqt_put_char(PGtypeArgs *args); /* "char" type, not char(N) */
int pqt_get_char(PGtypeArgs *args);
int pqt_put_bool(PGtypeArgs *args);
int pqt_get_bool(PGtypeArgs *args);
int pqt_put_money(PGtypeArgs *args);
int pqt_get_money(PGtypeArgs *args);
int pqt_put_uuid(PGtypeArgs *args);
int pqt_get_uuid(PGtypeArgs *args);
int pqt_put_null(PGtypeArgs *args); /* no get for null */

/* == in network.c == */

int pqt_put_inet(PGtypeArgs *args); /* handles cidr */
int pqt_get_inet(PGtypeArgs *args);
int pqt_get_cidr(PGtypeArgs *args);
int pqt_put_macaddr(PGtypeArgs *args);
int pqt_get_macaddr(PGtypeArgs *args);

/* == in numerics.c == */

int pqt_put_int2(PGtypeArgs *args);
int pqt_get_int2(PGtypeArgs *args);
int pqt_put_int4(PGtypeArgs *args); /* handles oid */
int pqt_get_int4(PGtypeArgs *args); /* handles oid */
int pqt_put_int8(PGtypeArgs *args);
int pqt_get_int8(PGtypeArgs *args);
int pqt_put_float4(PGtypeArgs *args);
int pqt_get_float4(PGtypeArgs *args);
int pqt_put_float8(PGtypeArgs *args);
int pqt_get_float8(PGtypeArgs *args);
int pqt_put_numeric(PGtypeArgs *args);
int pqt_get_numeric(PGtypeArgs *args);

/* == record.c (composites) == */

int pqt_put_record(PGtypeArgs *args);
int pqt_get_record(PGtypeArgs *args);

/* == in varlena.c == */

int pqt_put_str(PGtypeArgs *args);  /* no get for str - use PQgetvalue */
int pqt_put_text(PGtypeArgs *args); /* handles varchar, bpchar and name */
int pqt_get_text(PGtypeArgs *args); /* handles varchar, bpchar and name */
int pqt_put_bytea(PGtypeArgs *args);
int pqt_get_bytea(PGtypeArgs *args);

#ifdef __cplusplus
}
#endif
#endif


