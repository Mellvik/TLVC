#ifndef __SYS_WEAKEN_H
#define __SYS_WEAKEN_H

/* return address of weak version of passed symbol */
#define _weakaddr(sym) __extension__ ({                         \
        extern __typeof__(sym) sym __attribute__((__weak__));   \
        sym; })

#define _weakfn(sym) __extension__ ({                           \
        extern __typeof__(sym) sym __attribute__((__weak__));   \
        sym; })

#endif
