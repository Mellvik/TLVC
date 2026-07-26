/*
 * Structure returned by the V7 ftime system call
 * Used by Venix binaries only
 */

struct timeb {
        long    time;
        unsigned int millitm;
        int     timezone;
        int     dstflag;
};
