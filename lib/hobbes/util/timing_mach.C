#if defined(__APPLE__) && defined(__MACH__) && defined(OSX_BEFORE_SIERRA)

/*  from https://github.com/ChisholmKyle/PosixMachTiming
Copyright (c) 2015, Kyle Chisholm and Jason E. Aten

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#define _POSIX_C_SOURCE 200809L
#include <unistd.h>

#include <time.h>
#include "hobbes/util/timing_mach.h"

/* inline functions - maintain ANSI C compatibility */
#ifdef TIMING_C99
/* *** */
/* C99 */

extern double timespec2secd(const struct timespec *ts_in);
extern void secd2timespec(struct timespec *ts_out, const double sec_d);
extern void timespec_monodiff_lmr(struct timespec *ts_out,
                                  const struct timespec *ts_in);
extern void timespec_monodiff_rml(struct timespec *ts_out,
                                  const struct timespec *ts_in);
extern void timespec_monoadd(struct timespec *ts_out,
                             const struct timespec *ts_in);

#endif

#ifdef __MACH__
/* ******** */
/* __MACH__ */

#include <mach/mach_time.h>
#include <mach/mach.h>
#include <mach/clock.h>

/* timing struct for osx */
static struct TimingMach {
    mach_timebase_info_data_t timebase;
    clock_serv_t cclock;
} timing_mach_g;

/* mach clock port */
extern mach_port_t clock_port;

int timing_mach_init (void) {
    int retval = mach_timebase_info(&timing_mach_g.timebase);
    if (retval != 0) return retval;
    retval = host_get_clock_service(mach_host_self(),
                                    CALENDAR_CLOCK, &timing_mach_g.cclock);
    return retval;
}

#ifdef __cplusplus
/* auto initialize in C++ */
class InitTimingMach
{public:
  InitTimingMach(){ timing_mach_init(); }
};
InitTimingMach startup; // A global instance
#endif

int clock_gettime(clockid_t id, struct timespec *tspec) {
    mach_timespec_t mts;
    int retval = 0;
    if (id == CLOCK_REALTIME) {
        retval = clock_get_time(timing_mach_g.cclock, &mts);
        if (retval != 0) return retval;
        tspec->tv_sec = mts.tv_sec;
        tspec->tv_nsec = mts.tv_nsec;
    } else if (id == CLOCK_MONOTONIC) {
        retval = clock_get_time(clock_port, &mts);
        if (retval != 0) return retval;
        tspec->tv_sec = mts.tv_sec;
        tspec->tv_nsec = mts.tv_nsec;
    } else {
        /* only CLOCK_MONOTOIC and CLOCK_REALTIME clocks supported */
        return -1;
    }
    return 0;
}

int clock_nanosleep_abstime(const struct timespec *req) {
    struct timespec ts_delta;
    int retval = clock_gettime(CLOCK_MONOTONIC, &ts_delta);
    if (retval != 0) return retval;
    timespec_monodiff_rml (&ts_delta, req);
    /* mach does not properly return remainder from nanosleep */
    retval = nanosleep(&ts_delta, NULL);
    return retval;
}

/* __MACH__ */
/* ******** */
#endif

int itimer_start (struct timespec *ts_target, const struct timespec *ts_step) {
    int retval = clock_gettime(CLOCK_MONOTONIC, ts_target);
    if (retval != 0) return retval;
    /* add step size to current monotonic time */
    timespec_monoadd(ts_target, ts_step);
    return retval;
}

int itimer_step (struct timespec *ts_target, const struct timespec *ts_step) {
    int retval = clock_nanosleep_abstime(ts_target);
    if (retval != 0) return retval;
    /* move target along */
    timespec_monoadd(ts_target, ts_step);
    return retval;
}

#endif
