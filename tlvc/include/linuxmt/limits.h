#ifndef __LINUXMT_LIMITS_H
#define __LINUXMT_LIMITS_H

/* tunable parameters - changing these can have a large effect on kernel data size */

/* kernel */
#define MAX_TASKS       16      /* Max # processes */

#ifdef CONFIG_ARCH_PC98
#define KSTACK_BYTES    740     /* Size of kernel stacks for PC-98 */
#else
#define KSTACK_BYTES    640     /* Size of kernel stacks w/sync I/O */
#endif

#define ISTACK_BYTES    512     /* Size of interrupt stack */
#define TSTACK_BYTES    128     /* Size of temp startup stack */

#define KSTACK_GUARD    100     /* bytes before CHECK_KSTACK overflow warning */

#define MAX_POLLFD      6       /* Maximum number of polled filedescs per process */

#define MAX_SEGS        5       /* Maximum number of application code/data segments */

/* buffers */
#define NR_MAPBUFS      8       /* Default number of internal L1 buffers */
#define NR_REQUEST	20      /* Default number of asynch IO request headers,
				 * see ll_rw_blk.c */

/* filesystem */
#define NR_INODE        96      /* this should be bigger than NR_FILE */
#define NR_FILE         64      /* this can well be larger on a larger system */
#define NR_SUPER        6       /* max mounts */

#define PIPE_BUFSIZ     80      /* doesn't have to be power of two */

#define NR_ALARMS       5       /* Max number of simultaneous alarms system-wide */
#define NGROUPS         13      /* Supplementary groups */

#define MAX_PACKET_ETH 1536     /* Max packet size, 6 blocks of 256 bytes */

#endif /* !__LINUXMT_LIMITS_H */
