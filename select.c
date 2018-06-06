/*	$OpenBSD: select.c,v 1.2 2002/06/25 15:50:15 mickey Exp $	*/

/*
 * Copyright 2000-2007 Niels Provos <provos@citi.umich.edu>
 * Copyright 2007-2012 Niels Provos and Nick Mathewson
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "event2/event-config.h"
#include "evconfig-private.h"

#ifdef EVENT__HAVE_SELECT

#ifdef __APPLE__
/* Apple wants us to define this if we might ever pass more than
 * FD_SETSIZE bits to select(). */
#define _DARWIN_UNLIMITED_SELECT
#endif

#include <sys/types.h>
#ifdef EVENT__HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef EVENT__HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <sys/queue.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#include "event-internal.h"
#include "evsignal-internal.h"
#include "event2/thread.h"
#include "evthread-internal.h"
#include "log-internal.h"
#include "evmap-internal.h"

#ifndef EVENT__HAVE_FD_MASK

//zhou: All of the below, is a workaround to exceed the limitation of FD_SETSIZE(1024 in normal),
//      and it's quite similar to the way sys/types.h used in glibc.
//      But it's not encoughed by somebody, the alternative is:
//                  FD_SET(fd%FD_SETSIZE, &fds_array[fd/FD_SETSIZE])


/* This type is mandatory, but Android doesn't define it. */
typedef unsigned long fd_mask;
#endif

#ifndef NFDBITS
#define NFDBITS (sizeof(fd_mask)*8)
#endif

//zhou: caculate group number, each group can hold y bits, each bits stand for one event
/* Divide positive x by y, rounding up. */
#define DIV_ROUNDUP(x, y)   (((x)+((y)-1))/(y))

/* How many bytes to allocate for N fds? */
#define SELECT_ALLOC_SIZE(n) \
	(DIV_ROUNDUP(n, NFDBITS) * sizeof(fd_mask))

struct selectop {
	int event_fds;		/* Highest fd in fd set */
    //zhou: number of byte to accomodate event mask
	int event_fdsz;
    //zhou: flag to indicate fd_set changes
	int resize_out_sets;

    // zhou: these fd_sets will be kept
	fd_set *event_readset_in;
	fd_set *event_writeset_in;

    // zhou: these fd_sets will be updated by select
	fd_set *event_readset_out;
	fd_set *event_writeset_out;
};

static void *select_init(struct event_base *);
static int select_add(struct event_base *, int, short old, short events, void*);
static int select_del(struct event_base *, int, short old, short events, void*);
static int select_dispatch(struct event_base *, struct timeval *);
static void select_dealloc(struct event_base *);

const struct eventop selectops = {
	"select",
	select_init,
	select_add,
	select_del,
	select_dispatch,
	select_dealloc,
	0, /* doesn't need reinit. */
	EV_FEATURE_FDS,
	0,
};

static int select_resize(struct selectop *sop, int fdsz);
static void select_free_selectop(struct selectop *sop);

static void *
select_init(struct event_base *base)
{
	struct selectop *sop;

	if (!(sop = mm_calloc(1, sizeof(struct selectop))))
		return (NULL);

    //zhou: we set default events number as (32+1),
    //      "1" is very important, we must make sure "fd" < bit number of "fd_set"
	if (select_resize(sop, SELECT_ALLOC_SIZE(32 + 1))) {
		select_free_selectop(sop);
		return (NULL);
	}

    //zhou: register signal socket-pair FD
    //      we always register the socket even if no user be care of signal
	evsig_init_(base);

    //zhou: just generate a seed
	evutil_weakrand_seed_(&base->weakrand_seed, 0);

	return (sop);
}

#ifdef CHECK_INVARIANTS
static void
check_selectop(struct selectop *sop)
{
	/* nothing to be done here */
}
#else
#define check_selectop(sop) do { (void) sop; } while (0)
#endif

static int
select_dispatch(struct event_base *base, struct timeval *tv)
{
	int res=0, i, j, nfds;
	struct selectop *sop = base->evbase;

    //zhou: nothing. Just for protect select specific data
	check_selectop(sop);

    // zhou: fd_set size changed, the "event_xxxset_out" should follow "event_xxxset_in"
	if (sop->resize_out_sets) {
		fd_set *readset_out=NULL, *writeset_out=NULL;
		size_t sz = sop->event_fdsz;

        // zhou: pay attention again, the memory maybe moved, so we should update
        //       "sop->event_readset_out"
		if (!(readset_out = mm_realloc(sop->event_readset_out, sz)))
			return (-1);
		sop->event_readset_out = readset_out;
		if (!(writeset_out = mm_realloc(sop->event_writeset_out, sz))) {
			/* We don't free readset_out here, since it was
			 * already successfully reallocated. The next time
			 * we call select_dispatch, the realloc will be a
			 * no-op. */
			return (-1);
		}
		sop->event_writeset_out = writeset_out;
		sop->resize_out_sets = 0;
	}

    //zhou: update FD sets we are using
	memcpy(sop->event_readset_out, sop->event_readset_in,
	       sop->event_fdsz);
	memcpy(sop->event_writeset_out, sop->event_writeset_in,
	       sop->event_fdsz);

	nfds = sop->event_fds+1;

	EVBASE_RELEASE_LOCK(base, th_base_lock);

    //zhou: why not care about exceptfds?
    //      nfds, give select() a hint to know the boundary of readset and writeset
	res = select(nfds, sop->event_readset_out,
	    sop->event_writeset_out, NULL, tv);

	EVBASE_ACQUIRE_LOCK(base, th_base_lock);

	check_selectop(sop);

	if (res == -1) {
		if (errno != EINTR) {
            // zhou: In normal, due to  wrong fd which closed by accident
			event_warn("select");
			return (-1);
		}

		return (0);
	}

	event_debug(("%s: select reports %d", __func__, res));

	check_selectop(sop);

    //zhou: we got "i" between 0~nfds randomly in this way, then we can make sequence of
    //      processing events is indepented with fd number.
	i = evutil_weakrand_range_(&base->weakrand_seed, nfds);

    //zhou: why not check the res, the sum of total active events??? It will save a lot of
    //      CPU time.
	for (j = 0; j < nfds; ++j) {
		if (++i >= nfds)
			i = 0;
		res = 0;
		if (FD_ISSET(i, sop->event_readset_out))
			res |= EV_READ;
		if (FD_ISSET(i, sop->event_writeset_out))
			res |= EV_WRITE;

		if (res == 0)
			continue;

		evmap_io_active_(base, i, res);
	}
	check_selectop(sop);

	return (0);
}

static int
select_resize(struct selectop *sop, int fdsz)
{
	fd_set *readset_in = NULL;
	fd_set *writeset_in = NULL;

	if (sop->event_readset_in)
		check_selectop(sop);

	if ((readset_in = mm_realloc(sop->event_readset_in, fdsz)) == NULL)
		goto error;
	sop->event_readset_in = readset_in;
	if ((writeset_in = mm_realloc(sop->event_writeset_in, fdsz)) == NULL) {
		/* Note that this will leave event_readset_in expanded.
		 * That's okay; we wouldn't want to free it, since that would
		 * change the semantics of select_resize from "expand the
		 * readset_in and writeset_in, or return -1" to "expand the
		 * *set_in members, or trash them and return -1."
		 */
		goto error;
	}
	sop->event_writeset_in = writeset_in;
	sop->resize_out_sets = 1;

    // zhou: memset the new malloc part
	memset((char *)sop->event_readset_in + sop->event_fdsz, 0,
	    fdsz - sop->event_fdsz);
	memset((char *)sop->event_writeset_in + sop->event_fdsz, 0,
	    fdsz - sop->event_fdsz);

	sop->event_fdsz = fdsz;
	check_selectop(sop);

	return (0);

 error:
	event_warn("malloc");
	return (-1);
}


static int
select_add(struct event_base *base, int fd, short old, short events, void *p)
{
	struct selectop *sop = base->evbase;

// zhou: avoid compilation warning
	(void) p;

	EVUTIL_ASSERT((events & EV_SIGNAL) == 0);
	check_selectop(sop);
	/*
	 * Keep track of the highest fd, so that we can calculate the size
	 * of the fd_sets for select(2)
	 */
	if (sop->event_fds < fd) {
		int fdsz = sop->event_fdsz;

        // zhou: at least, we should have one fd_mask whose size is 4 bytes
		if (fdsz < (int)sizeof(fd_mask))
			fdsz = (int)sizeof(fd_mask);

		/* In theory we should worry about overflow here.  In
		 * reality, though, the highest fd on a unixy system will
		 * not overflow here. XXXX */
		while (fdsz < (int) SELECT_ALLOC_SIZE(fd + 1))
			fdsz *= 2;

		if (fdsz != sop->event_fdsz) {
			if (select_resize(sop, fdsz)) {
				check_selectop(sop);
				return (-1);
			}
		}

        // zhou: the max fd
		sop->event_fds = fd;
	}

	if (events & EV_READ)
		FD_SET(fd, sop->event_readset_in);
	if (events & EV_WRITE)
		FD_SET(fd, sop->event_writeset_in);
	check_selectop(sop);

	return (0);
}

/*
 * Nothing to be done here.
 */

static int
select_del(struct event_base *base, int fd, short old, short events, void *p)
{
	struct selectop *sop = base->evbase;
	(void)p;

	EVUTIL_ASSERT((events & EV_SIGNAL) == 0);
	check_selectop(sop);

    // zhou: impossible here
	if (sop->event_fds < fd) {
		check_selectop(sop);
		return (0);
	}

    // zhou: we never shrink fds
	if (events & EV_READ)
		FD_CLR(fd, sop->event_readset_in);

	if (events & EV_WRITE)
		FD_CLR(fd, sop->event_writeset_in);

	check_selectop(sop);
	return (0);
}

static void
select_free_selectop(struct selectop *sop)
{
	if (sop->event_readset_in)
		mm_free(sop->event_readset_in);
	if (sop->event_writeset_in)
		mm_free(sop->event_writeset_in);
	if (sop->event_readset_out)
		mm_free(sop->event_readset_out);
	if (sop->event_writeset_out)
		mm_free(sop->event_writeset_out);

	memset(sop, 0, sizeof(struct selectop));
	mm_free(sop);
}

static void
select_dealloc(struct event_base *base)
{
	evsig_dealloc_(base);

	select_free_selectop(base->evbase);
}

#endif /* EVENT__HAVE_SELECT */
