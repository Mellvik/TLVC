/*
 * TNET		A server program for MINIX which implements the TCP/IP
 *		suite of networking protocols.  It is based on the
 *		TCP/IP code written by Phil Karn et al, as found in
 *		his NET package for Packet Radio communications.
 *
 * 		Definitions for the TELNET protocol (see RFC XXX).
 *
 * Version:	@(#)arpa/telnet.h	1.00		07/02/92
 *
 * Authors:	Original taken from BSD 4.3/TAHOE.
 *		Fred N. van Kempen, <waltje@uwalt.nl.mugnet.org>
 */
#ifndef _ARPA_TELNET_H
#define _ARPA_TELNET_H

#define	IAC		255	/* interpret as command:		*/
#define	IAC_DONT	254	/* you are not to use option		*/
#define	IAC_DO		253	/* please, you use option		*/
#define	IAC_WONT	252	/* I won't use option			*/
#define	IAC_WILL	251	/* I will use option			*/
#define	IAC_SB		250	/* interpret as subnegotiation		*/
#define	IAC_GA		249	/* you may reverse the line		*/
#define	IAC_EL		248	/* erase the current line		*/
#define	IAC_EC		247	/* erase the current character		*/
#define	IAC_AYT		246	/* are you there			*/
#define	IAC_AO		245	/* abort output--but let prog finish	*/
#define	IAC_IP		244	/* interrupt process--permanently	*/
#define	IAC_BREAK	243	/* break				*/
#define	IAC_DM		242	/* data mark--for connect. cleaning	*/
#define	IAC_NOP		241	/* nop					*/
#define	IAC_SE		240	/* end sub negotiation			*/
#define	IAC_EOR     	239     /* end of record (transparent mode)	*/

#define SYNCH		242	/* for telfunc calls			*/

/* Telnet options. */
#define TELOPT_BINARY	0	/* 8-bit data path			*/
#define TELOPT_ECHO	1	/* echo					*/
#define	TELOPT_RCP	2	/* prepare to reconnect			*/
#define	TELOPT_SGA	3	/* suppress go ahead			*/
#define	TELOPT_NAMS	4	/* approximate message size		*/
#define	TELOPT_STATUS	5	/* give status				*/
#define	TELOPT_TM	6	/* timing mark				*/
#define	TELOPT_RCTE	7	/* remote controlled transmission and echo */
#define TELOPT_NAOL 	8	/* negotiate about output line width	*/
#define TELOPT_NAOP 	9	/* negotiate about output page size	*/
#define TELOPT_NAOCRD	10	/* negotiate about CR disposition	*/
#define TELOPT_NAOHTS	11	/* negotiate about horizontal tabstops	*/
#define TELOPT_NAOHTD	12	/* negotiate about horizontal tab disposition */
#define TELOPT_NAOFFD	13	/* negotiate about formfeed disposition	*/
#define TELOPT_NAOVTS	14	/* negotiate about vertical tab stops	*/
#define TELOPT_NAOVTD	15	/* negotiate about vertical tab disposition */
#define TELOPT_NAOLFD	16	/* negotiate about output LF disposition */
#define TELOPT_XASCII	17	/* extended ascic character set		*/
#define	TELOPT_LOGOUT	18	/* force logout				*/
#define	TELOPT_BM	19	/* byte macro				*/
#define	TELOPT_DET	20	/* data entry terminal			*/
#define	TELOPT_SUPDUP	21	/* supdup protocol			*/
#define	TELOPT_SUPDUPOUTPUT 22	/* supdup output			*/
#define	TELOPT_SNDLOC	23	/* send location			*/
#define	TELOPT_TTYPE	24	/* terminal type			*/
#define	TELOPT_EOR	25	/* end or record			*/
#define	TELOPT_WINCH	31	/* window size				*/
#define TELOPT_EXOPL	255	/* extended-options-list		*/

/* Sub-option qualifiers. */
#define	TELQUAL_IS	0	/* option is...				*/
#define	TELQUAL_SEND	1	/* send option				*/

/* Special characters */
#define CTRL_C	3		/* like IAC_IP */

void tel_init(void);
void telopt(int fdout, int what, int option);
void tel_in(int fdout, int telout, unsigned char *buffer, int len);
void tel_out(int fdout, unsigned char *buf, int size);
#endif /* _ARPA_TELNET_H */
