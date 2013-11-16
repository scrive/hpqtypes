
/*
 * Copyright (c) 2003-2008, PostgreSQL Global Development Group
 * $PostgreSQL: pgsql/src/include/getaddrinfo.h
 */

#if defined(__CYGWIN__) || (defined(HAVE_CONFIG_H) && \
	!defined(HAVE_GETADDRINFO))

#ifndef GETADDRINFO_H
#define GETADDRINFO_H

#include <sys/socket.h>
#define _XOPEN_SOURCE_EXTENDED 1
#include <netdb.h>


/* Various macros that ought to be in <netdb.h>, but might not be */

#ifndef EAI_FAIL
#define EAI_BADFLAGS	(-1)
#define EAI_NONAME		(-2)
#define EAI_AGAIN		(-3)
#define EAI_FAIL		(-4)
#define EAI_FAMILY		(-6)
#define EAI_SOCKTYPE	(-7)
#define EAI_SERVICE		(-8)
#define EAI_MEMORY		(-10)
#define EAI_SYSTEM		(-11)
#endif   /* !EAI_FAIL */

#ifndef AI_PASSIVE
#define AI_PASSIVE		0x0001
#endif

#ifndef AI_NUMERICHOST
/*
 * some platforms don't support AI_NUMERICHOST; define as zero if using
 * the system version of getaddrinfo...
 */
#if defined(HAVE_STRUCT_ADDRINFO) && defined(HAVE_GETADDRINFO)
#define AI_NUMERICHOST	0
#else
#define AI_NUMERICHOST	0x0004
#endif
#endif

#ifndef NI_NUMERICHOST
#define NI_NUMERICHOST	1
#endif
#ifndef NI_NUMERICSERV
#define NI_NUMERICSERV	2
#endif

#ifndef NI_MAXHOST
#define NI_MAXHOST	1025
#endif
#ifndef NI_MAXSERV
#define NI_MAXSERV	32
#endif

#ifdef HAVE_STRUCT_SOCKADDR_STORAGE

#ifndef HAVE_STRUCT_SOCKADDR_STORAGE_SS_FAMILY
#ifdef HAVE_STRUCT_SOCKADDR_STORAGE___SS_FAMILY
#define ss_family __ss_family
#else
#error struct sockaddr_storage does not provide an ss_family member
#endif
#endif

#ifdef HAVE_STRUCT_SOCKADDR_STORAGE___SS_LEN
#define ss_len __ss_len
#define HAVE_STRUCT_SOCKADDR_STORAGE_SS_LEN 1
#endif
#else							/* !HAVE_STRUCT_SOCKADDR_STORAGE */

/* Define a struct sockaddr_storage if we don't have one. */

#ifndef __CYGWIN__
struct sockaddr_storage
{
	union
	{
		struct sockaddr sa;		/* get the system-dependent fields */
    long long int		ss_align;	/* ensures struct is properly aligned */
		char		ss_pad[128];	/* ensures struct has desired size */
	}			ss_stuff;
};
#endif

#define ss_family	ss_stuff.sa.sa_family
/* It should have an ss_len field if sockaddr has sa_len. */
#ifdef HAVE_STRUCT_SOCKADDR_SA_LEN
#define ss_len		ss_stuff.sa.sa_len
#define HAVE_STRUCT_SOCKADDR_STORAGE_SS_LEN 1
#endif
#endif   /* HAVE_STRUCT_SOCKADDR_STORAGE */

#ifndef HAVE_STRUCT_ADDRINFO

struct addrinfo
{
	int			ai_flags;
	int			ai_family;
	int			ai_socktype;
	int			ai_protocol;
	size_t		ai_addrlen;
	struct sockaddr *ai_addr;
	char	   *ai_canonname;
	struct addrinfo *ai_next;
};

#endif   /* HAVE_STRUCT_ADDRINFO */



/* Rename private copies per comments above */
#ifdef getaddrinfo
#undef getaddrinfo
#endif

#ifdef freeaddrinfo
#undef freeaddrinfo
#endif

#ifdef gai_strerror
#undef gai_strerror
#endif

#ifdef getnameinfo
#undef getnameinfo
#endif

extern int getaddrinfo(const char *node, const char *service,
			const struct addrinfo * hints, struct addrinfo ** res);
extern void freeaddrinfo(struct addrinfo * res);
extern const char *gai_strerror(int errcode);
extern int getnameinfo(const struct sockaddr * sa, int salen,
			char *node, int nodelen,
			char *service, int servicelen, int flags);

#endif   /* GETADDRINFO_H */

#endif /* HAVE_GETADDRINFO */

