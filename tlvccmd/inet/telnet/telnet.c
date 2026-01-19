/*
 * Telnet for ELKS / TLVC
 *
 * Based on minix telnet client.
 * (c) 2001 Harry Kalogirou <harkal@rainbow.cs.unipi.gr>
 *
 */

#include <sys/types.h>
#include <sys/ioctl.h>
#include <errno.h>
#include <fcntl.h>
#include <termios.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#define debug(...)
//#define debug     printf
//#define RAWTELNET	/* set in telnet and telnetd for raw telnet without IAC*/

#define ESCAPE      (']'&0x1f)  /* = ^] escape session. Since we don't have a
				 * command mode, escape will terminate the session */
#define BUFSIZE		1500

/* telnet protocol */
#define IAC		255
#define IAC_SE		240
#define IAC_NOP		241
#define IAC_DataMark	242
#define IAC_BRK		243
#define IAC_IP		244
#define IAC_AO		245
#define IAC_AYT		246
#define IAC_EC		247
#define IAC_EL		248
#define IAC_GA		249
#define IAC_SB		250
#define IAC_WILL	251
#define IAC_WONT	252
#define IAC_DO		253
#define IAC_DONT	254

#define OPT_ECHO	1
#define OPT_SUPP_GA	3
#define OPT_TERMTYPE	24

#define TERMTYPE_SEND	1
#define TERMTYPE_IS	0

#define FALSE	0
#define TRUE	(!(FALSE))
#define CTRL_C	3
#define CTRL_O	15

#ifdef __linux__
int DO_echo= TRUE;
int DO_echo_allowed= TRUE;
int WILL_terminal_type= FALSE;
int WILL_terminal_type_allowed= TRUE;
int DO_suppress_go_ahead= TRUE;
int DO_suppress_go_ahead_allowed= TRUE;
#else
int DO_echo= FALSE;
int DO_echo_allowed= TRUE;
int WILL_terminal_type= FALSE;
int WILL_terminal_type_allowed= TRUE;
int DO_suppress_go_ahead= FALSE;
int DO_suppress_go_ahead_allowed= TRUE;
#endif

int tcp_fd;
char *term_env;
struct termios def_termios;
int escape;
int discard;

static int process_opt (char *bp, int count);

void finish()
{
	int nonblock = 0;
	ioctl(0, FIONBIO, &nonblock);
	tcsetattr(0, TCSANOW, &def_termios);
	exit(0);	
}

static void read_keyboard(void)
{
	int count;
	unsigned char buffer[BUFSIZE];

		count = read(0, buffer, sizeof(buffer));
		//fprintf(stderr, "got %d (0x%x)\n", count, *buffer);
		if (count <= 0 || *buffer == escape) { 
			fprintf(stderr, "\nSession terminated\n");
			finish();
			return;
		}
		if (*buffer == CTRL_C) {	/* ^C = abort output */
		    if (discard < 3) {		/* ignore if we already sent an IAC_IP */
			discard = 3;		/* discard whatever is en route */
#define USE_IAC_IP
#ifdef USE_IAC_IP				/* Some telnet servers behave better with ^C
						 * than with IAC_IP (!). */
			buffer[0] = IAC;
			buffer[1] = IAC_IP;	/* send Interrupt Process */
			count = 2;
			//fprintf(stderr, "sending IAC_IP\n");
#endif
		    }
		}	
		if (*buffer == CTRL_O && !(discard&2)) {	/* ^O - flush output toggle */
			discard ^= 1;
			buffer[0] = '^';
			buffer[1] = 'O';
			write(1, buffer, 2);
			return;			/* Don't pass on to server */
		}
		if (discard == 1) {		/* let any char typed release ^O */
			discard = 0;
			return;
		}
		count = write(tcp_fd, buffer, count);
		if (count < 0) {
			printf("Connection closed by foreign host.");
			finish();
		}
}

static void read_network(void)
{
	char *bp, *iacptr;
	int count, optsize;
	char buffer[BUFSIZE];

		count = read(tcp_fd, buffer, sizeof(buffer));

		if (count <= 0) {
			printf("\nConnection closed\n");
			finish();
		}
#ifdef RAWTELNET
		write(1, buffer, count);
#else
		bp = buffer;

		do {
			
			iacptr = memchr(bp, IAC, count);
			//fprintf(stderr, "read: %d discard %d (%x)\n", count, discard, iacptr);
			if (!iacptr) {
				if (!discard) write(1, bp, count);
				break;
			}
			if (iacptr > bp) {
				if (!discard) write(1, bp, iacptr-bp);
				count -= (iacptr-bp);
				bp = iacptr;
				continue;
			}
			optsize = process_opt(bp, count);
			if (optsize < 0)
				break;
			bp += optsize;
			count -= optsize;
		} while (count);

		return;
#endif
}

int main(int argc, char **argv)
{
	unsigned short port;
	struct sockaddr_in locadr, remadr;
	int nonblock;
	ipaddr_t ipaddr;

	if (argc < 2) {
		printf("Usage: %s [-e esc_char] host <port>\n", argv[0]);
		return 1;
	}
	escape = ESCAPE;

	if (*argv[1] == '-') {
		if (*(argv[1]+1) != 'e') {
			printf("Usage: %s [-e esc_char] host <port>\n", argv[0]);
			return 1;
		}
		if (*argv[2] != '^')
			escape = *argv[2];
		else
			escape = *(argv[2]+1);
		escape = (escape&0xdf) - '@';
		argv += 2;
	}
		
	tcgetattr(0, &def_termios);
	signal(SIGINT, finish);

	tcp_fd = socket(AF_INET, SOCK_STREAM, 0);
	if (tcp_fd < 0) {
		perror("telnet");
		return 1;
	}
	
	locadr.sin_family = AF_INET;
	locadr.sin_port = PORT_ANY;
	locadr.sin_addr.s_addr = INADDR_ANY;

#if 0	/* test code to force RST on telnet exit*/
{
	struct linger l;

	l.l_onoff = 1;
	l.l_linger = 0;		/* send RST on close */
	setsockopt(tcp_fd, SOL_SOCKET, SO_LINGER, &l, sizeof(l));
}
#endif
	if (bind(tcp_fd, (struct sockaddr *)&locadr, sizeof(struct sockaddr_in)) < 0) {
		perror("bind");
		return 1;
	}

	ipaddr = in_gethostbyname(argv[1]);
	if (!ipaddr) {
		perror(argv[1]);
		return 1;
	}
	port = argv[2] ? atoi(argv[2]): 23;

	remadr.sin_family = AF_INET;
	remadr.sin_port = htons(port);
	remadr.sin_addr.s_addr = ipaddr;

	printf("Trying %s (%s:%u)...\n", argv[1], in_ntoa(ipaddr), port);
	if (in_connect(tcp_fd, (struct sockaddr *)&remadr, sizeof(struct sockaddr_in), 10) < 0) {
		perror("Connection failed");
		return 1;
	}
	printf("Connected\n");
	printf("Escape character is '^%c'.\n", escape + '@');

	struct termios termios;
	tcgetattr(0, &termios);
	termios.c_oflag |= (OPOST | ONLCR);
	termios.c_lflag &= ~ISIG;       /* ISIG off to disable ^N/^O/^P */
#ifdef RAWTELNET
	termios.c_iflag &= ~(ICRNL|IGNCR|INLCR|IXON|IXOFF);
	termios.c_lflag &= ~(ECHO|ECHONL|ICANON);
#endif
	tcsetattr(0, TCSANOW, &termios);
	nonblock = 1;
	ioctl(0, FIONBIO, &nonblock);

	for (;;) {
		int n;
		fd_set fdset;
		struct timeval tv, *ptv;

		FD_ZERO(&fdset);
		FD_SET(0, &fdset);
		FD_SET(tcp_fd, &fdset);

		tv.tv_sec = 0;
		if (discard == 3) {	/* If telnetd at the other end never 'acks' an IAC_IP
					 * with a DataMark, a 3 second timeout will
					 * release the discard - unless the user has sent a second
					 * ^C before that. */
		    ptv = &tv;
        	    tv.tv_usec = 3000000;
		} else
        	    ptv = NULL;

        	n = select(tcp_fd + 1, &fdset, NULL, NULL, ptv);
        	if (n == 0) {
		    //fprintf(stderr, "timeout!\n");
		    discard = 0;
		    write(tcp_fd, "\r", 1);
		    continue;	
		}
		if (n < 0) {
		    perror("select");
		    break;
		}

		if (FD_ISSET(tcp_fd, &fdset))
		    read_network();
		if (FD_ISSET(0, &fdset))
		    read_keyboard();
	}
	finish();
}

#ifndef RAWTELNET
#define next_char(var) \
	if (offset < count) { (var) = bp[offset++]; } \
	else { if (read(tcp_fd, (char *)&(var), 1) != 1) printf("TELNET BAD READ2\n"); exit(1); }

static int writeall(int fd, char *buffer, int buf_size)
{
	int result;

	while (buf_size)
	{
		result = write(fd, buffer, buf_size);
		if (result <= 0)
			return -1;
		buffer += result;
		buf_size -= result;
	}
	return 0;
}

static void do_option(int optsrt)
{
	unsigned char reply[3];

	switch (optsrt) {
	case OPT_TERMTYPE:
		if (WILL_terminal_type)
			return;
		if (!WILL_terminal_type_allowed) {
			reply[0] = IAC;
			reply[1] = IAC_WONT;
			reply[2] = optsrt;
		} else {
			WILL_terminal_type = TRUE;
			term_env = getenv("TERM");
			if (!term_env)
				term_env = "unknown";
			reply[0] = IAC;
			reply[1] = IAC_WILL;
			reply[2] = optsrt;
		}
		break;
	default:
		reply[0] = IAC;
		reply[1] = IAC_WONT;
		reply[2] = optsrt;
		break;
	}
	writeall(tcp_fd, (char *)reply, 3);
}

static void will_option(int optsrt)
{
	unsigned char reply[3];

	switch (optsrt) {
	case OPT_ECHO:
		if (DO_echo)
			break;
		if (!DO_echo_allowed) {
			reply[0] = IAC;
			reply[1] = IAC_DONT;
			reply[2] = optsrt;
		} else {
			struct termios termios;
			tcgetattr(0, &termios);
			termios.c_iflag &= ~(ICRNL|IGNCR|INLCR|IXON|IXOFF);
			termios.c_oflag |= (OPOST | ONLCR);
			termios.c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG);	/* ISIG off for ^P*/
			tcsetattr(0, TCSANOW, &termios);
			DO_echo = TRUE;
			reply[0] = IAC;
			reply[1] = IAC_DO;
			reply[2] = optsrt;
		}
		writeall(tcp_fd, (char *)reply, 3);
		break;

	case OPT_SUPP_GA:
		if (DO_suppress_go_ahead)
			break;
		if (!DO_suppress_go_ahead_allowed) {
			reply[0] = IAC;
			reply[1] = IAC_DONT;
			reply[2] = optsrt;
		} else {
			DO_suppress_go_ahead = TRUE;
			reply[0] = IAC;
			reply[1] = IAC_DO;
			reply[2] = optsrt;
		}
		writeall(tcp_fd, (char *)reply, 3);
		break;

	default:
		reply[0] = IAC;
		reply[1] = IAC_DONT;
		reply[2] = optsrt;
		writeall(tcp_fd, (char *)reply, 3);
		break;
	}
}

static void dont_option(int optsrt)
{
	switch (optsrt) {
	default:
		break;
	}
}

static void wont_option(int optsrt)
{
	switch (optsrt) {
	default:
		break;
	}
}

static int sb_termtype(char *bp, int count)
{
	unsigned char command, iac, optsrt;
	unsigned char buffer[4];
	int offset, result;

	offset= 0;
	next_char(command);
	if (command == TERMTYPE_SEND) {
		buffer[0] = IAC;
		buffer[1] = IAC_SB;
		buffer[2] = OPT_TERMTYPE;
		buffer[3] = TERMTYPE_IS;
		result = writeall(tcp_fd, (char *)buffer, 4);
		if (result < 0)
			return result;
		count = strlen(term_env);
		if (!count) {
			term_env = "unknown";
			count= strlen(term_env);
		}
		result = writeall(tcp_fd, term_env, count);
		if (result < 0)
			return result;
		buffer[0]= IAC;
		buffer[1]= IAC_SE;
		result = writeall(tcp_fd, (char *)buffer, 2);
		if (result < 0)
			return result;
	} else {
		debug("got an unknown command (skipping)\n");
	}

	for (;;) {
		next_char(iac);
		if (iac != IAC)
			continue;
		next_char(optsrt);
		if (optsrt == IAC)
			continue;
		if (optsrt != IAC_SE) {
			debug("got IAC %d\n", optsrt);
		}
		break;
	}	
	return offset;
}


static int process_opt(char *bp, int count)
{
	unsigned char iac, command, optsrt, sb_command;
	int offset, result;

	offset = 0;
	next_char(iac);
	next_char(command);
	switch(command) {
	case IAC_NOP:
		break;
	case IAC_DataMark:
		discard = 0;
		debug("got DataMark\n");
		break;
	case IAC_BRK:
		debug("got BRK\n");
		break;
	case IAC_IP:
		debug("got IP\n");
		break;
	case IAC_AO:
		debug("got AO\n");
		break;
	case IAC_AYT:
		debug("got AYT\n");
		break;
	case IAC_EC:
		debug("got EC\n");
		break;
	case IAC_EL:
		debug("got EL\n");
		break;
	case IAC_GA:
		debug("got GA\n");
		break;
	case IAC_SB:		/* subnegotiation */
		next_char(sb_command);
		switch (sb_command) {
		case OPT_TERMTYPE:
			result= sb_termtype(bp+offset, count-offset);
			if (result < 0)
				return result;
			else
				return result+offset;
		default:
			debug("got unknown SB (skipping)\n");
			for (;;) {
				next_char(iac);
				if (iac != IAC)
					continue;
				next_char(optsrt);
				if (optsrt == IAC)
					continue;
				if (optsrt != IAC_SE)
					debug("got IAC %d\n", optsrt);
				break;
			}
		}
		break;
	case IAC_WILL:
		next_char(optsrt);
		will_option(optsrt);
		break;
	case IAC_WONT:
		next_char(optsrt);
		wont_option(optsrt);
		break;
	case IAC_DO:
		next_char(optsrt);
		do_option(optsrt);
		break;
	case IAC_DONT:
		next_char(optsrt);
		dont_option(optsrt);
		break;
	case IAC:
		debug("got IAC\n");
		break;
	default:
		debug("got unknown command (%d)\n", command);
	}
	return offset;
}
#endif /* RAWTELNET*/
