/*
 * TNET		A server program for MINIX which implements the TCP/IP
 *		suite of networking protocols.  It is based on the
 *		TCP/IP code written by Phil Karn et al, as found in
 *		his NET package for Packet Radio communications.
 *
 *		This module handles telnet option processing.
 *
 * Author:	Michael Temari, <temari@temari.ae.ge.com>  01/13/93
 *
 */
#include <sys/types.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <termios.h>
#include "telnet.h"
#include <stdio.h>
#include <sys/ioctl.h>


#define	IN_DATA	0
#define	IN_CR	1
#define	IN_IAC	2
#define	IN_IAC2	3
#define	IN_SB	4
#define	IN_AO	5

static void dowill(int c);
static void dowont(int c);
static void dodo(int c);
static void dodont(int c);
static void respond(int ack, int option);
static void respond_really(int ack, int option);
static void send_dmark(int);

#define	LASTTELOPT	TELOPT_SGA

static int r_winch = 0;
static int dm_flag;

static int TelROpts[LASTTELOPT+1];
static int TelLOpts[LASTTELOPT+1];

static int telfdout;

void tel_init()
{
int i;

   for(i = 0; i <= LASTTELOPT; i++) {
	TelROpts[i] = 0;
	TelLOpts[i] = 0;
   }
}

void telopt(fdout, what, option)
int fdout;
int what;
int option;
{
char buf[3];
int len;

   buf[0] = IAC;
   buf[1] = what;
   buf[2] = option;
   len = 0;

   switch(what) {
	case IAC_DO:
		if (option <= LASTTELOPT) {
			TelROpts[option] = 1;
			len = 3;
		} else if (option == TELOPT_WINCH && !r_winch) {
			r_winch = 1;
			len = 3;
		} 
		break;
	case IAC_DONT:
		if (option <= LASTTELOPT) {
			TelROpts[option] = 1;
			len = 3;
		}
		break;
	case IAC_WILL:
		if (option <= LASTTELOPT) {
			TelLOpts[option] = 1;
			len = 3;
		}
		break;
	case IAC_WONT:
		if (option <= LASTTELOPT) {
			TelLOpts[option] = 1;
			len = 3;
		}
		break;
   }
   if (len > 0)
	write(fdout, buf, len);
}

void set_winsize(int fd, unsigned int cols, unsigned int rows)
{
	struct winsize w;
	memset(&w, 0, sizeof(w));
	w.ws_col = cols;
	w.ws_row = rows;
	ioctl(fd, TIOCSWINSZ, (char *) &w);
}

void tel_in(fdout, telout, buffer, len)
int fdout;
int telout;
unsigned char *buffer;
int len;
{
static int InState = IN_DATA;
static int ThisOpt = 0;
static int winchpos = -1;
unsigned char *p, *p2;
int size, c, l = len;

   telfdout = telout;
   p = p2 = buffer;
   size = 0;

   size = 0;
   while (len > 0) {
   	c = (unsigned char)*p++;
	len--;
	switch(InState) {
		case IN_CR:
			InState = IN_DATA;
			if (c == 0 || c == '\n')
				break;
			/* fall through */
		case IN_DATA:
			if (c == IAC) {
				InState = IN_IAC;
				//fprintf(stderr, "Got IAC\n", c);
				break;
			}
			*p2++ = c;
			size++;
			if (c == '\r') InState = IN_CR;
			if (c == CTRL_C)
				dm_flag++;
				//send_dmark();	/* really too early to reply (or 'ack') the command,  */
						/* we haven't killed the process yet but
						 * there is no reliable way to verify that */
			break;
		case IN_IAC:
			//fprintf(stderr, "IAC: %d\n", c);
			switch(c) {
				case IAC:
					*p2++ = c; size++;
					InState = IN_DATA;
					break;
				case IAC_WILL:
				case IAC_WONT:
				case IAC_DO:
				case IAC_DONT:
					InState = IN_IAC2;
					ThisOpt = c;
					break;
				case IAC_SB:
				 	InState = IN_SB; 
					break;
				case IAC_IP:
					*p2++ = CTRL_C; size++;	/* turn into ^C */
					InState = IN_DATA;
					//fprintf(stderr, "got IAC_IP\n");
					//send_dmark();
					dm_flag++;
					break;
				case IAC_AO:
				case IAC_EOR:
				case IAC_SE:
				case IAC_NOP:
				case IAC_BREAK:
				case IAC_AYT:
				case IAC_EC:
				case IAC_EL:
				case IAC_GA:
					break;
				default:
					break;
			}
			break;
		case IN_IAC2:
			if (size > 0) {
				write(fdout, buffer, size);
				p2 = buffer;
				size = 0;
			}
			InState = IN_DATA;
			switch(ThisOpt) {
				case IAC_WILL:	dowill(c);	break;
				case IAC_WONT:	dowont(c);	break;
				case IAC_DO:	dodo(c);	break;
				case IAC_DONT:	dodont(c);	break;
			}
			break;
		case IN_SB:
			/* Subnegotiation. */
			if (winchpos >= 0) {
				static unsigned int winchbuf[5], iacs = 0;
				winchbuf[winchpos] = c;
				/* IAC is escaped - unescape it. */
				if (c == IAC) iacs++;
				else { iacs = 0; winchpos++; }
				if (iacs == 2) { winchpos++; iacs = 0; }
				if (winchpos >= 4) {
					/* End of WINCH data. */
					set_winsize(fdout,
					(winchbuf[0] << 8) | winchbuf[1],
					(winchbuf[2] << 8) | winchbuf[3]);
					winchpos = -1;
				}
			} else {
				static int lastiac = 0;
				switch(c) {
					case TELOPT_WINCH:
						/* Start listening. */
						winchpos = 0;
						break;
					case IAC_SE:
						if (lastiac) InState = IN_DATA;
						break;
					default:
						break;
				}
				if (c == IAC) lastiac = 1;
				else lastiac = 0;
			}
			break;
	}
   }
   //if (size > l) fprintf(stderr, "size mismatch size %d > len %d\n", size, l);
   if (size > 0)
	write(fdout, buffer, size);
}

void tel_out(int fdout, unsigned char *buf, int size)
{
   unsigned char *p;
   int got_iac, len;

   p = buf;
   while(size > 0) {
	if (dm_flag) {
		send_dmark(fdout);
		dm_flag = 0;
	}
	buf = p;
	got_iac = 0;
	if ((p = memchr(buf, IAC, size)) != NULL) {
		got_iac = 1;
		p++;
	} else
		p = buf + size;
	len = p - buf;
	if (len > 0)
		(void) write(fdout, buf, len);
	if (got_iac)
		(void) write(fdout, p - 1, 1);
	size = size - len;
   }
}

static void dowill(c)
int c;
{
int ack;

   switch(c) {
	case TELOPT_BINARY:
	case TELOPT_ECHO:
	case TELOPT_SGA:
		if (TelROpts[c] == 1)
			return;
		TelROpts[c] = 1;
		ack = IAC_DO;
		break;
	case TELOPT_WINCH:
		if (r_winch) return;
		r_winch = 1;
		ack = IAC_DO;
 		respond_really(ack, c); 
		return;
	default:
		ack = IAC_DONT;
   }

   respond(ack, c);
}

static void dowont(c)
int c;
{
   if (c <= LASTTELOPT) {
	if (TelROpts[c] == 0)
		return;
	TelROpts[c] = 0;
   }
   respond(IAC_DONT, c);
}

static void dodo(c)
int c;
{
int ack;

   switch(c) {
	default:
		ack = IAC_WONT;
   }
   respond(ack, c);
}

static void dodont(c)
int c;
{
   if (c <= LASTTELOPT) {
	if (TelLOpts[c] == 0)
		return;
	TelLOpts[c] = 0;
   }
   respond(IAC_WONT, c);
}

static void respond(ack, option)
int ack, option;
{
   /**unsigned char c[3];

   c[0] = IAC;
   c[1] = ack;
   c[2] = option;**/
/*   write(telfdout, c, 3); */
}

static void respond_really(ack, option)
int ack, option;
{
unsigned char c[3];

   c[0] = IAC;
   c[1] = ack;
   c[2] = option;
   write(telfdout, c, 3); 
}

static void send_dmark(int fd) {
    unsigned char c[2];

    //fprintf(stderr, "returned DM\n");
    c[0] = IAC;
    c[1] = IAC_DM;
    write(fd, c, 2);
}
