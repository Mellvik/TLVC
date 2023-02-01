/* This file is part of the ELKS TCP/IP stack
 *
 * (C) 2001 Harry Kalogirou (harkal@rainbow.cs.unipi.gr)
 *
 *	This program is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either version
 *	2 of the License, or (at your option) any later version.
 */

#include <sys/time.h>

#include "timer.h"

timeq_t timer_get_time(void)
{
    struct timezone	tz;
    struct timeval 	tv;

    gettimeofday(&tv, &tz);

	/* return 1/16 second ticks, 1,000,000/16 = 62500*/
    return (tv.tv_sec << 4) | ((unsigned long)tv.tv_usec / 62500U);
}
