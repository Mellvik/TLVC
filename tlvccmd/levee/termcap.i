/*
 * LEVEE, or Captain Video;  A vi clone
 *
 * Copyright (c) 1982-1997 David L Parsons
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by David L Parsons (orc@pell.chi.il.us).  My name may not be used
 * to endorse or promote products derived from this software without
 * specific prior written permission.  THIS SOFTWARE IS PROVIDED
 * AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
 * WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF MERCHANTIBILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.
 */
/*
 * Termcap handlers
 *
 * Routines included:
 *   tc_init() -- set up all the terminal stuff levee will need.
 *  *xtract() -- get a field out of the termcap entry.
 *  *parseit() -- parse a termcap field.
 *   tgoto()   -- put a gotoXY string into a buffer.
 *  * -> internal routine.
 */
#include <termcap.h>

#if RMX | MSDOS		/* default to ANSI.SYS termcap */
char termcap[200] = "Ansi subset:CM=\033[%d;%dH,Y,1,1:\
CE=\033[K:CL=\033[H\033[J:LINES=24:COLS=79:HO=\033[H:FkL=\033:\
CurR=C:CurL=D:CurU=A:CurD=B";

char *
parseit(ptr,savearea)
/* parse a termcap field */
char *ptr;
char **savearea;
{
    char *p = *savearea;
    char *op = *savearea;
    int tmp;

    while (*ptr && *ptr != ':') {
	if (*ptr == '\\' && ptr[1]) {
	    ++ptr;
	    switch (*ptr) {
	      case 'E':
		*p++ = '\033';
		break;
	      case '0': case '1': case '2': case '3': case '4':
	      case '5': case '6': case '7': case '8': case '9':
		  tmp = 0;
		  while (*ptr >= '0' && *ptr <= '9')
		      tmp = (tmp*8)+(*ptr++ - '0');
		  *p++ = tmp;
		  --ptr;
	      default:
		  *p++ = *ptr;
	    }
	}
	else *p++ = *ptr;
	++ptr;
    }
    *p++ = 0;
    *savearea = p;
    return op;
} /* parseit */

char *
xtract(ptr,name,savearea)
/* get something from the termcap
 *
 * arguments: tcentry -- the termcap entry
 *            what    -- the field we want to get (NULL means first field)
 *            savearea-- pointer to static buffer for parsed fields.
 */
char *ptr;
char name[];
char **savearea;
{
    int size;

    if (!ptr)
	return NULL;

    if (!name)	/* return first field of entry -- terminal name? */
	return parseit(ptr,savearea);

    size = strlen(name);
    /*
     * always skip the first (terminal name) field
     */
    while (*ptr) {
	while (*ptr && *ptr != ':') {
	    if (*ptr == '\\')
		ptr++;
	    ptr++;
	}
	if (*ptr)
	    ptr++;
	if (*ptr && strncmp(name,ptr,size) == 0 && ptr[size] == '=')
	    return parseit(&ptr[1+size],savearea);
	puts("\r");
    }
    return NULL;
} /* xtract */

char
charext(tc,what,savearea)
/* get a character field from the termcap */
char *tc, *what, **savearea;
{
    char *p = xtract(tc,what,savearea);
    if (p)
	return *p;
    return 0;
} /* charext */

/* internal variables just for termcap */
static int _Xfirst, _xpad, _ypad;
#endif

tc_init()
/* get the termcap stuff and go berserk parsing it */
/* if anything horrid goes wrong, levee will crash */
{
#if RMX
    char *p = termcap;
#else
    char *getenv();
#if MSDOS
    char *p = getenv("TERMCAP");
#else
    char *buffer, **caps;
    char *p = getenv("TERM");
#endif
#endif
    char *lp, *ptr;

#if MSDOS
    if (!p)
	p = termcap;
#endif
#if !(RMX|MSDOS)
    if (!p) {
	puts("lv: no TERM\n");
	exit(1);
    }
    TERMNAME = p;
    lp = malloc(1024);
#else
    lp = Malloc(strlen(p)+1);
#endif
    if (!lp) {
	puts("lv: out of memory\n");
	exit(1);
    }

#if RMX | MSDOS
    TERMNAME = xtract(p,NULL,&lp);
    CM   = xtract(p,"CM",&lp);
    HO   = xtract(p,"HO",&lp);
    UP   = xtract(p,"UP",&lp);
    CE   = xtract(p,"CE",&lp);
    CL   = xtract(p,"CL",&lp);
    BELL = xtract(p,"BELL",&lp);
    if (!BELL)
	BELL = "\007";
    OL   = xtract(p,"OL",&lp);
    UpS  = xtract(p,"UpS",&lp);
    CURon= xtract(p,"CURon",&lp);
    CURoff=xtract(p,"CURoff",&lp);

    FkL  = charext(p,"FkL",&lp);
    CurRT= charext(p,"CurR",&lp);
    CurLT= charext(p,"CurL",&lp);
    CurUP= charext(p,"CurU",&lp);
    CurDN= charext(p,"CurD",&lp);

    canUPSCROLL = (UpS != NULL);
    CA = (CM != NULL);

    if ((LINES=atoi(ptr=xtract(p,"LINES",&lp))) <= 0) {
	puts("lv: bad termcap");
	exit(1);
    }
    dofscroll = LINES/2;
    if ((COLS=atoi(ptr=xtract(p,"COLS",&lp))-1) <= 0 || COLS >= MAXCOLS) {
	puts("lv: bad termcap");
	exit(1);
    }

    _ypad = _xpad = 0;
    _Xfirst = 1;

    p = CM;

    while (*p && *p != ',')
	p++;
    if (!*p)
	return;
    *p++ = 0;
    if (*p != ',')
	_Xfirst = (*p++ == 'X');
    if (!*p)
	return;
    ++p;
    while (*p && *p != ',')
	_xpad = (_xpad*10) + (*p++ - '0');
    if (!*p)
	return;
    ++p;
    while (*p)
	_ypad = (_ypad*10) + (*p++ - '0');

#else

    if (tgetent(lp, p) < 1) {
	printf("lv: no termcap for %s\n", p);
	exit(1);
    }
    if ((LINES=tgetnum("li")) <= 0) {
	puts("lv: bad termcap");
	exit(1);
    }
    dofscroll = LINES/2;
    if ((COLS=tgetnum("co")-1) <= 0 || COLS >= MAXCOLS) {
	puts("lv: bad termcap");
	exit(1);
    }
    buffer = (char *)malloc(strlen(lp));
    caps = &buffer;

    HO   = tgetstr("ho", caps);/*cursor home*/
    UP   = tgetstr("up", caps);/*move cursor up one line*/
    BC   = tgetstr("le", caps);/*move cursor left one col*/
    CE   = tgetstr("ce", caps);/*clear to end of line*/
    CL   = tgetstr("cl", caps);/*clear screen*/
    BELL = tgetstr("bl", caps);/*bell*/
    if (!BELL)
	BELL = "\007";
    OL   = tgetstr("al", caps);/*insert a line*/
    UpS  = tgetstr("sr", caps);/*reverse scroll*/
    canUPSCROLL = (UpS != NULL);
    CURon= tgetstr("ve", caps);/*cursor normal*/
    CURoff=tgetstr("vi", caps);/*cursor invisible*/

    CM   = tgetstr("cm", caps);
    CA = (CM != NULL);

    functionkeys[0] = tgetstr("kr", caps);/*arrow keys*/
    functionkeys[1] = tgetstr("kl", caps);
    functionkeys[2] = tgetstr("ku", caps);
    functionkeys[3] = tgetstr("kd", caps);
    functionkeys[4] = 0;

    free(lp);
#endif
}

#if RMX | MSDOS
#define tgoto(s,y,x)	(_Xfirst?sprintf(s,CM,x+_xpad,y+_ypad):\
				sprintf(s,CM,y+_ypad,x+_xpad))
#endif
