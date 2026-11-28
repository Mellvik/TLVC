typedef	unsigned int	daddr_t;	/* disk address */
typedef	char *		caddr_t;	/* core address */
typedef	int		ino_t;		/* i-node number */
typedef	long		time_t;		/* a time */
typedef	int		dev_t;		/* device code */
typedef	long		off_t;		/* offset */
typedef	int		size_t;
typedef	struct	_quad { long val[2]; } quad;

#define	major(x)	((int)(((unsigned)(x)>>8)&0377))
#define	minor(x)	((int)((x)&0377))
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* sys III compat */
