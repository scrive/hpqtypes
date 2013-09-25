
/*
 * port.c
 *   Portability functions.  Some of these functions are drop-ins for
 *   systems missing them and others just centralize differences.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

#if defined(__MINGW32__) || defined(__CYGWIN__)
#	define HAVE_VSNPRINTF
#endif

int
pqt_snprintf(char *buf, size_t size, const char *format, ...)
{
	int n;
	va_list ap;
	va_start(ap, format);
	n = pqt_vsnprintf(buf, size, format, ap);
	va_end(ap);
	return n;
}

int
pqt_vsnprintf(char *buf, size_t size, const char *format, va_list ap)
{
	int n;

#ifdef PQT_MSVC
#	if PQT_MSVC >= 1400
	/* MSVC 8 */
	n = _vsnprintf_s(buf, size, size-1, format, ap);
#	else
	/* MSVC 6 or 7 */
	n = _vsnprintf(buf, size, format, ap);
#	endif

#elif defined(HAVE_VSNPRINTF)
	/* All other platforms, including MinGW and Cygwin. */
	n = vsnprintf(buf, size, format, ap);
#else
#	error "vsnprintf is not available"
#endif

	if (n > -1 && (size_t) n < size)
		return n;

	/* Although some implementations return "required" buf size, this
	 * always return -1 to keep things consistent for caller.
	 */
	return -1;
}

#if defined(HAVE_CONFIG_H) && (!defined(HAVE_STRTOL) || \
	!defined(HAVE_STRTOUL))

static unsigned long string2long(const char *nptr, char **endptr,
	int base, int is_signed);

#ifndef HAVE_STRTOL
long
strtol(const char *nptr, char **endptr, int base)
{
	return (signed long) string2long(nptr, endptr, base, 1);
}
#endif

#ifndef HAVE_STRTOUL
unsigned long
strtoul(const char *nptr, char **endptr, int base)
{
	return (unsigned long) string2long(nptr, endptr, base, 0);
}
#endif

#define between(a, c, z) \
	((unsigned) ((c) - (a)) <= (unsigned) ((z) - (a)))

static unsigned long
string2long(const char *nptr, char ** const endptr,
	int base, int is_signed)
{
	unsigned int v;
	unsigned long val = 0;
	int c;
	int ovfl = 0, sign = 1;
	const char *startnptr = nptr, *nrstart;

	if (endptr)
		*endptr = (char *) nptr;

	while (isspace(*nptr))
		nptr++;
	c = *nptr;

	if (c == '-' || c == '+')
	{
		if (c == '-')
			sign = -1;
		nptr++;
	}
	nrstart = nptr; /* start of the number */

	/* When base is 0, the syntax determines the actual base */
	if (base == 0)
	{
		if (*nptr == '0')
		{
			if (*++nptr == 'x' || *nptr == 'X')
			{
				base = 16;
				nptr++;
			}
			else
			{
				base = 8;
			}
		}
		else
		{
			base = 10;
		}
	}
	else if (base==16 && *nptr=='0' && (*++nptr =='x' || *nptr =='X'))
	{
		nptr++;
	}

	for (;;)
	{
		c = *nptr;
		if (between('0', c, '9'))
			v = c - '0';
		else if (between('a', c, 'z'))
			v = c - 'a' + 0xa;
		else if (between('A', c, 'Z'))
			v = c - 'A' + 0xA;
		else
			break;

		if (v >= base)
			break;
		if (val > (ULONG_MAX - v) / base)
			ovfl++;

		val = (val * base) + v;
		nptr++;
	}

	if (endptr)
	{
		if (nrstart == nptr)
			*endptr = (char *) startnptr;
		else
			*endptr = (char *) nptr;
	}

	if (!ovfl)
	{
		/* Overflow is only possible when converting a signed long. */
		if (is_signed && ((sign < 0 && val > -(unsigned long) LONG_MIN)
				|| (sign > 0 && val > LONG_MAX)))
			ovfl++;
	}

	if (ovfl)
	{
		errno = ERANGE;
		if (is_signed)
		{
			if (sign < 0)
				return LONG_MIN;
			else
				return LONG_MAX;
		}
		else
		{
			return ULONG_MAX;
		}
	}

	return (long) sign * val;
}

#endif /* !strtol || !strtoul */


/* Non-windows platforms that don't have strtoll */
#if defined(HAVE_CONFIG_H) && !defined(HAVE_STRTOLL)

#define MIN_INT64 (-MAX_INT64 - PQT_INT64CONST(1))
#define MAX_INT64 PQT_INT64CONST(9223372036854775807)

/* no locale support */
long long int
strtoll(const char *nptr, char **endptr, int base)
{
	const char *s;
	/* LONGLONG */
	long long int acc, cutoff;
	int c;
	int neg, any, cutlim;

	/* endptr may be NULL */

#ifdef __GNUC__
	/* This outrageous construct just to shut up a GCC warning. */
	(void) &acc; (void) &cutoff;
#endif

	/*
	 * Skip white space and pick up leading +/- sign if any.
	 * If base is 0, allow 0x for hex and 0 for octal, else
	 * assume decimal; if base is already 16, allow 0x.
	 */
	s = nptr;
	do {
		c = (unsigned char) *s++;
	} while (isspace(c));
	if (c == '-') {
		neg = 1;
		c = *s++;
	} else {
		neg = 0;
		if (c == '+')
			c = *s++;
	}
	if ((base == 0 || base == 16) &&
	    c == '0' && (*s == 'x' || *s == 'X')) {
		c = s[1];
		s += 2;
		base = 16;
	}
	if (base == 0)
		base = c == '0' ? 8 : 10;

	/*
	 * Compute the cutoff value between legal numbers and illegal
	 * numbers.  That is the largest legal value, divided by the
	 * base.  An input number that is greater than this value, if
	 * followed by a legal input character, is too big.  One that
	 * is equal to this value may be valid or not; the limit
	 * between valid and invalid numbers is then based on the last
	 * digit.  For instance, if the range for long longs is
	 * [-9223372036854775808..9223372036854775807] and the input base
	 * is 10, cutoff will be set to 922337203685477580 and cutlim to
	 * either 7 (neg==0) or 8 (neg==1), meaning that if we have
	 * accumulated a value > 922337203685477580, or equal but the
	 * next digit is > 7 (or 8), the number is too big, and we will
	 * return a range error.
	 *
	 * Set any if any `digits' consumed; make it negative to indicate
	 * overflow.
	 */
	cutoff = neg ? MIN_INT64 : MAX_INT64;
	cutlim = (int) (cutoff % base);
	cutoff /= base;
	if (neg) {
		if (cutlim > 0) {
			cutlim -= base;
			cutoff += 1;
		}
		cutlim = -cutlim;
	}
	for (acc = 0, any = 0;; c = (unsigned char) *s++) {
		if (isdigit(c))
			c -= '0';
		else if (isalpha(c))
			c -= isupper(c) ? 'A' - 10 : 'a' - 10;
		else
			break;
		if (c >= base)
			break;
		if (any < 0)
			continue;
		if (neg) {
			if (acc < cutoff || (acc == cutoff && c > cutlim)) {
				any = -1;
				acc = MIN_INT64;
				errno = ERANGE;
			} else {
				any = 1;
				acc *= base;
				acc -= c;
			}
		} else {
			if (acc > cutoff || (acc == cutoff && c > cutlim)) {
				any = -1;
				acc = MAX_INT64;
				errno = ERANGE;
			} else {
				any = 1;
				acc *= base;
				acc += c;
			}
		}
	}
	if (endptr != 0)
		/* LINTED interface specification */
		*endptr = (char *) (any ? s - 1 : nptr);
	return (acc);
}

#endif /* !HAVE_STRTOLL */


/* Non-windows machines missing getaddrinfo (postgres's port) */
#if defined(__CYGWIN__) || (defined(HAVE_CONFIG_H) && \
	!defined(HAVE_GETADDRINFO))
#undef FRONTEND
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "getaddrinfo.h"

extern int h_errno;

/*
 * get address info for ipv4 sockets.
 *
 *	Bugs:	- only one addrinfo is set even though hintp is NULL or
 *		  ai_socktype is 0
 *		- AI_CANONNAME is not supported.
 *		- servname can only be a number, not text.
 */
int
getaddrinfo(const char *node, const char *service,
			const struct addrinfo * hintp,
			struct addrinfo ** res)
{
	struct addrinfo *ai;
	struct sockaddr_in sin,
			   *psin;
	struct addrinfo hints;

	if (hintp == NULL)
	{
		memset(&hints, 0, sizeof(hints));
		hints.ai_family = AF_INET;
		hints.ai_socktype = SOCK_STREAM;
	}
	else
		memcpy(&hints, hintp, sizeof(hints));

	if (hints.ai_family != AF_INET && hints.ai_family != AF_UNSPEC)
		return EAI_FAMILY;

	if (hints.ai_socktype == 0)
		hints.ai_socktype = SOCK_STREAM;

	if (!node && !service)
		return EAI_NONAME;

	memset(&sin, 0, sizeof(sin));

	sin.sin_family = AF_INET;

	if (node)
	{
		if (node[0] == '\0')
			sin.sin_addr.s_addr = htonl(INADDR_ANY);
		else if (hints.ai_flags & AI_NUMERICHOST)
		{
			if (!inet_aton(node, &sin.sin_addr))
				return EAI_FAIL;
		}
		else
		{
			struct hostent *hp;

#ifdef FRONTEND
			struct hostent hpstr;
			char		buf[BUFSIZ];
			int			herrno = 0;

			pqGethostbyname(node, &hpstr, buf, sizeof(buf),
							&hp, &herrno);
#else
			hp = gethostbyname(node);
#endif
			if (hp == NULL)
			{
				switch (h_errno)
				{
					case HOST_NOT_FOUND:
					case NO_DATA:
						return EAI_NONAME;
					case TRY_AGAIN:
						return EAI_AGAIN;
					case NO_RECOVERY:
					default:
						return EAI_FAIL;
				}
			}
			if (hp->h_addrtype != AF_INET)
				return EAI_FAIL;

			memcpy(&(sin.sin_addr), hp->h_addr, hp->h_length);
		}
	}
	else
	{
		if (hints.ai_flags & AI_PASSIVE)
			sin.sin_addr.s_addr = htonl(INADDR_ANY);
		else
			sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	}

	if (service)
		sin.sin_port = htons((unsigned short) atoi(service));

#ifdef HAVE_STRUCT_SOCKADDR_STORAGE_SS_LEN
	sin.sin_len = sizeof(sin);
#endif

	ai = malloc(sizeof(*ai));
	if (!ai)
		return EAI_MEMORY;

	psin = malloc(sizeof(*psin));
	if (!psin)
	{
		free(ai);
		return EAI_MEMORY;
	}

	memcpy(psin, &sin, sizeof(*psin));

	ai->ai_flags = 0;
	ai->ai_family = AF_INET;
	ai->ai_socktype = hints.ai_socktype;
	ai->ai_protocol = hints.ai_protocol;
	ai->ai_addrlen = sizeof(*psin);
	ai->ai_addr = (struct sockaddr *) psin;
	ai->ai_canonname = NULL;
	ai->ai_next = NULL;

	*res = ai;

	return 0;
}


void
freeaddrinfo(struct addrinfo * res)
{
	if (res)
	{
		if (res->ai_addr)
			free(res->ai_addr);
		free(res);
	}
}


const char *
gai_strerror(int errcode)
{
#ifdef HAVE_HSTRERROR
	int			hcode;

	switch (errcode)
	{
		case EAI_NONAME:
			hcode = HOST_NOT_FOUND;
			break;
		case EAI_AGAIN:
			hcode = TRY_AGAIN;
			break;
		case EAI_FAIL:
		default:
			hcode = NO_RECOVERY;
			break;
	}

	return hstrerror(hcode);
#else							/* !HAVE_HSTRERROR */

	switch (errcode)
	{
		case EAI_NONAME:
			return "Unknown host";
		case EAI_AGAIN:
			return "Host name lookup failure";
			/* Errors below are probably WIN32 only */
#ifdef EAI_BADFLAGS
		case EAI_BADFLAGS:
			return "Invalid argument";
#endif
#ifdef EAI_FAMILY
		case EAI_FAMILY:
			return "Address family not supported";
#endif
#ifdef EAI_MEMORY
		case EAI_MEMORY:
			return "Not enough memory";
#endif
#ifdef EAI_NODATA
#ifndef WIN32_ONLY_COMPILER		/* MSVC complains because another case has the
								 * same value */
		case EAI_NODATA:
			return "No host data of that type was found";
#endif
#endif
#ifdef EAI_SERVICE
		case EAI_SERVICE:
			return "Class type not found";
#endif
#ifdef EAI_SOCKTYPE
		case EAI_SOCKTYPE:
			return "Socket type not supported";
#endif
		default:
			return "Unknown server error";
	}
#endif   /* HAVE_HSTRERROR */
}

/*
 * Convert an ipv4 address to a hostname.
 *
 * Bugs:	- Only supports NI_NUMERICHOST and NI_NUMERICSERV
 *		  It will never resolv a hostname.
 *		- No IPv6 support.
 */
int
getnameinfo(const struct sockaddr * sa, int salen,
			char *node, int nodelen,
			char *service, int servicelen, int flags)
{
	/* Invalid arguments. */
	if (sa == NULL || (node == NULL && service == NULL))
		return EAI_FAIL;

	/* We don't support those. */
	if ((node && !(flags & NI_NUMERICHOST))
		|| (service && !(flags & NI_NUMERICSERV)))
		return EAI_FAIL;

#if defined(HAVE_IPV6) || defined(AF_INET6)
	if (sa->sa_family == AF_INET6)
		return EAI_FAMILY;
#endif

	if (node)
	{
		int			ret = -1;

		if (sa->sa_family == AF_INET)
		{
			char	   *p;

			p = inet_ntoa(((struct sockaddr_in *) sa)->sin_addr);
			ret = pqt_snprintf(node, nodelen, "%s", p);
		}
		if (ret == -1)
			return EAI_MEMORY;
	}

	if (service)
	{
		int			ret = -1;

		if (sa->sa_family == AF_INET)
		{
			ret = pqt_snprintf(service, servicelen, "%d",
						   ntohs(((struct sockaddr_in *) sa)->sin_port));
		}
		if (ret == -1)
			return EAI_MEMORY;
	}

	return 0;
}

#endif /* HAVE_GETADDRINFO */


