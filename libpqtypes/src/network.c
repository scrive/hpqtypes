
/*
 * network.c
 *   Type handler for the network data types.
 *
 * Copyright (c) 2011 eSilo, LLC. All rights reserved.
 * This is free software; see the source for copying conditions.  There is
 * NO warranty; not even for MERCHANTABILITY or  FITNESS FOR A  PARTICULAR
 * PURPOSE.
 */

#include "libpqtypes-int.h"

#if defined(__CYGWIN__) || (defined(HAVE_CONFIG_H) && \
	!defined(HAVE_GETADDRINFO))
#	include "getaddrinfo.h"
#endif

#ifndef PGSQL_AF_INET
#	define PGSQL_AF_INET	(AF_INET + 0)
#endif

#ifndef PGSQL_AF_INET6
#	define PGSQL_AF_INET6	(AF_INET + 1)
#endif

#ifndef AF_INET6
#warning NO AF_INET6 SUPPORT!
#endif

/* Some platforms don't define this, like AIX 4.3 */
#ifndef AI_NUMERICHOST
#	define AI_NUMERICHOST 0x04
#endif

/* handles cidr as well */
int
pqt_put_inet(PGtypeArgs *args)
{
	unsigned char *b = (unsigned char *)args->put.out;
	PGinet *inet = va_arg(args->ap, PGinet *);
	int family;

	PUTNULLCHK(args, inet);

	family = ((struct sockaddr *)inet->sa_buf)->sa_family;

	if (family == AF_INET)
	{
		struct sockaddr_in *sa = (struct sockaddr_in *) inet->sa_buf;
		*b++ = (unsigned char) PGSQL_AF_INET;
		*b++ = (unsigned char) inet->mask;
		*b++ = (unsigned char) (inet->is_cidr ? 1 : 0);
		*b++ = (unsigned char) 4;
		memcpy(b, &sa->sin_addr, 4);
		b += 4;
	}
#ifdef AF_INET6
	else if (family == AF_INET6)
	{
		struct sockaddr_in6 *sa = (struct sockaddr_in6 *) inet->sa_buf;
		*b++ = (unsigned char) PGSQL_AF_INET6;
		*b++ = (unsigned char) inet->mask;
		*b++ = (unsigned char) (inet->is_cidr ? 1 : 0);
		*b++ = (unsigned char) 16;
		memcpy(b, &sa->sin6_addr, 16);
		b += 16;
	}
#endif
	else
	{
		return args->errorf(args, "Unknown inet address family %d", family);
	}

	return (int) ((char *) b - args->put.out);
}

static int
get_inet2(PGtypeArgs *args, int is_cidr)
{
	DECLVALUE(args);
	unsigned short family;
	PGinet *inet = va_arg(args->ap, PGinet *);

	CHKGETVALS(args, inet);

	if (args->format == TEXTFMT)
	{
		int r;
		char *p;
		char ipstr[80];
		struct addrinfo *ai = NULL;
		struct addrinfo hints;

		pqt_strcpy(ipstr, sizeof(ipstr), value);
		if ((p = strrchr(ipstr, '/')))
		{
			*p = 0;
			inet->mask = atoi(p+1);
		}
		else
		{
			inet->mask = 32;
		}

		inet->is_cidr = is_cidr;

		/* suppress hostname lookups */
		memset(&hints, 0, sizeof(hints));
		hints.ai_flags = AI_NUMERICHOST;

		/* Without this, windows chokes with WSAHOST_NOT_FOUND */
#ifdef PQT_WIN32
		hints.ai_family = AF_INET;
#endif

		if ((r = getaddrinfo(ipstr, NULL, &hints, &ai)) || !ai)
		{
			if(r == EAI_BADFLAGS)
			{
				hints.ai_flags = 0;
				r = getaddrinfo(ipstr, NULL, &hints, &ai);
			}
			/* Another WSAHOST_NOT_FOUND work around, but for IPv6 */
#if defined(PQT_WIN32) && defined(AF_INET6)
			else if(r == WSAHOST_NOT_FOUND)
			{
				hints.ai_flags = 0;
				hints.ai_family = AF_INET6;
				r = getaddrinfo(ipstr, NULL, &hints, &ai);
			}
#endif

			if(r)
				RERR_STR2INT(args);
		}

		inet->sa_buf_len = (int) ai->ai_addrlen;
		memcpy(inet->sa_buf, ai->ai_addr, inet->sa_buf_len);

		/* Some platforms, lika AIX 4.3, do not zero this structure properly.
     * The family and port are dirty, so set the first 4 bytes to 0 and
     * then re-set the family.  I saw "0x1002" as the first 2 bytes of
     * this structure (dumb getaddrinfo), it should be "0x0002" for AF_INET.
     */
		memset(inet->sa_buf, 0, 4);
		((struct sockaddr *)inet->sa_buf)->sa_family = ai->ai_addr->sa_family;

		/* Uninitialized memory, postgres inet/cidr types don't store this.
     * Make sure its set to 0.  Another AIX problem (maybe other platforms).
		 */
#ifdef AF_INET6
		if (ai->ai_addr->sa_family == AF_INET6)
			((struct sockaddr_in6 *)inet->sa_buf)->sin6_flowinfo = 0;
#endif

		freeaddrinfo(ai);
		return 0;
	}

	family = (unsigned short) *value++;
	if (family == PGSQL_AF_INET)
	{
		struct sockaddr_in *sa = (struct sockaddr_in *) inet->sa_buf;
		sa->sin_family = AF_INET;
		inet->mask = (unsigned char) *value++;
		inet->is_cidr = *value++;
		memcpy(&sa->sin_addr, value + 1, *value);
		inet->sa_buf_len = (int) sizeof(struct sockaddr_in);
	}
#ifdef AF_INET6
	else if (family == PGSQL_AF_INET6)
	{
		struct sockaddr_in6 *sa = (struct sockaddr_in6 *) inet->sa_buf;
		sa->sin6_family = AF_INET6;
		inet->mask = (unsigned char) *value++;
		inet->is_cidr = *value++;
		memcpy(&sa->sin6_addr, value + 1, *value);
		inet->sa_buf_len = (int) sizeof(struct sockaddr_in6);
	}
#endif
	else
	{
		return args->errorf(args, "Unknown inet address family %d", family);
	}

	return 0;
}

int
pqt_get_inet(PGtypeArgs *args)
{
	return get_inet2(args, 0);
}

int
pqt_get_cidr(PGtypeArgs *args)
{
	return get_inet2(args, 1);
}

int
pqt_put_macaddr(PGtypeArgs *args)
{
	PGmacaddr *mac = va_arg(args->ap, PGmacaddr *);

	PUTNULLCHK(args, mac);

	args->put.out[0] = (unsigned char) mac->a;
	args->put.out[1] = (unsigned char) mac->b;
	args->put.out[2] = (unsigned char) mac->c;
	args->put.out[3] = (unsigned char) mac->d;
	args->put.out[4] = (unsigned char) mac->e;
	args->put.out[5] = (unsigned char) mac->f;
	return 6;
}

int
pqt_get_macaddr(PGtypeArgs *args)
{
	DECLVALUE(args);
	unsigned char *p;
	PGmacaddr *mac = va_arg(args->ap, PGmacaddr *);

	CHKGETVALS(args, mac);

	if (args->format == TEXTFMT)
	{
		int a,b,c,d,e,f;
		int count = sscanf(value, "%x:%x:%x:%x:%x:%x", &a,&b,&c,&d,&e,&f);

		if (count != 6 || (a < 0) || (a > 255) || (b < 0) || (b > 255) ||
				(c < 0) || (c > 255) || (d < 0) || (d > 255) ||
				(e < 0) || (e > 255) || (f < 0) || (f > 255))
			RERR_STR2INT(args);

		mac->a = a;
		mac->b = b;
		mac->c = c;
		mac->d = d;
		mac->e = e;
		mac->f = f;
		return 0;
	}

	p = (unsigned char *) value;
	mac->a = *p++;
	mac->b = *p++;
	mac->c = *p++;
	mac->d = *p++;
	mac->e = *p++;
	mac->f = *p;
	return 0;
}



