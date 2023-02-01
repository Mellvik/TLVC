/* uniq - compact repeated lines		Author: John Woods */
/* Uniq [-udc] [-n] [+n] [infile [outfile]]
 *
 *	Written 02/08/86 by John Woods, placed into public domain.  Enjoy.
 *
 */

/* If the symbol WRITE_ERROR is defined, uniq will exit(1) if it gets a
 * write error on the output.  This is not (of course) how V7 uniq does it,
 * so undefine the symbol if you want to lose your output to a full disk
 */

#define WRITE_ERROR 1
#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

static char buffer[BUFSIZ];
static int uflag = 1;			/* default is union of -d and -u outputs */
static int dflag = 1;			/* flags are mutually exclusive */
static int cflag = 0;
static int fields = 0;
static int chars = 0;

/* The meat of the whole affair */
static char *nowline, *prevline, buf1[1024], buf2[1024];

static int ourgetline(char *buf, int count);

static FILE *xfopen(const char *fn, const char *mode)
{
  FILE *p;

  if ((p = fopen(fn, mode)) == NULL) {
	perror("uniq");
	fflush(stdout);
	exit(1);
  }
  return(p);
}

static char *skip(char *s)
{
  int n;

  /* Skip fields */
  for (n = fields; n > 0; --n) {
	/* Skip blanks */
	while (*s && (*s == ' ' || *s == '\t')) s++;
	if (!*s) return s;
	while (*s && (*s != ' ' && *s != '\t')) s++;
	if (!*s) return s;
  }

  /* Skip characters */
  for (n = chars; n > 0; --n) {
	if (!*s) return s;
	s++;
  }
  return s;
}

static int equal(char *s1, char *s2)
{
  return !strcmp(skip(s1), skip(s2));
}

static void show(char *line, int count)
{
  if (cflag)
	printf("%4d %s", count, line);
  else {
	if ((uflag && count == 1) || (dflag && count != 1))
		printf("%s", line);
  }
}

static int uniq(void)
{
  char *p;
  int seen;

  /* Setup */
  prevline = buf1;
  if (ourgetline(prevline, 1024) < 0) return(0);
  seen = 1;
  nowline = buf2;

  /* Get nowline and compare if not equal, dump prevline and swap
   * pointers else continue, bumping seen count */
  while (ourgetline(nowline, 1024) > 0) {
	if (!equal(prevline, nowline)) {
		show(prevline, seen);
		seen = 1;
		p = nowline;
		nowline = prevline;
		prevline = p;
	} else
		seen += 1;
  }
  show(prevline, seen);
  return 0;
}


static int ourgetline(char *buf, int count)
{
  int c;
  int ct = 0;

  while (ct++ < count) {
	c = getc(stdin);
	if (c < 0) return(-1);
	*buf++ = c;
	if (c == '\n') {
		*buf++ = 0;
		return(ct);
	}
  }
  return(ct);
}

int main(int argc, char **argv)
{
  char *p;
  int inf = -1;

  setbuf(stdout, buffer);
  for (--argc, ++argv; argc > 0 && (**argv == '-' || **argv == '+');
       --argc, ++argv) {
	if (**argv == '+')
		chars = atoi(*argv + 1);
	else if (isdigit(argv[0][1]))
		fields = atoi(*argv + 1);
	else if (argv[0][1] == '\0')
		inf = 0;	/* - is stdin */
	else
		for (p = *argv + 1; *p; p++) {
			switch (*p) {
			    case 'd':
				dflag = 1;
				uflag = 0;
				break;
			    case 'u':
				uflag = 1;
				dflag = 0;
				break;
			    case 'c':	cflag = 1;	break;
			    default:	goto usage;
			}
		}
  }

  /* Input file */
  if (argc == 0)
	inf = 0;
  else if (inf == -1) {		/* if - was not given */
	fclose(stdin);
	xfopen(*argv++, "r");
	argc--;
  }
  if (argc > 0) {
	fclose(stdout);
	xfopen(*argv++, "w");
	argc--;
  }

  uniq();
  fflush(stdout);
  exit(0);

usage:
	fprintf(stderr, "Usage: uniq [-udc] [+n] [-n] [input [output]]\n");
	exit(1);
}
