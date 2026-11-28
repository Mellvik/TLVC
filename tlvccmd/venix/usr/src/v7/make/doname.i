# 1 "doname.c"

# 1 "./defs"

# 1 "/usr/include/stdio.h"



extern	struct	_iobuf {
	char	*_ptr;
	int	_cnt;
	char	*_base;
	char	_flag;
	char	_file;
} _iob[20];


























struct _iobuf	*fopen();
struct _iobuf	*freopen();
struct _iobuf	*fdopen();
long	ftell();
char	*fgets();
# 2 "./defs"

# 1 "/usr/include/ctype.h"








extern	char	_ctype_[];	















# 3 "./defs"


typedef long int TIMETYPE;





























extern char funny[128];




extern int sigivalue;
extern int sigqvalue;
extern int waitpid;
extern int dbgflag;
extern int prtrflag;
extern int silflag;
extern int noexflag;
extern int keepgoing;
extern int noruleflag;
extern int touchflag;
extern int questflag;
extern int ndocoms;
extern int ignerr;
extern int okdel;
extern int inarglist;
extern char *prompt;
extern char junkname[ ];



struct nameblock
	{
	struct nameblock *nxtnameblock;
	char *namep;
	struct lineblock *linep;
	int done:3;
	int septype:3;
	TIMETYPE modtime;
	};

extern struct nameblock *mainname ;
extern struct nameblock *firstname;

struct lineblock
	{
	struct lineblock *nxtlineblock;
	struct depblock *depp;
	struct shblock *shp;
	};
extern struct lineblock *sufflist;

struct depblock
	{
	struct depblock *nxtdepblock;
	struct nameblock *depname;
	};

struct shblock
	{
	struct shblock *nxtshblock;
	char *shbp;
	};

struct varblock
	{
	struct varblock *nxtvarblock;
	char *varname;
	char *varval;
	int noreset:1;
	int used:1;
	};
extern struct varblock *firstvar;

struct pattern
	{
	struct pattern *nxtpattern;
	char *patval;
	};
extern struct pattern *firstpat;

struct opendir
	{
	struct opendir *nxtopendir;
	struct _iobuf * dirfc;
	char *dirn;
	};
extern struct opendir *firstod;


struct chain
	{
	struct chain *nextp;
	char *datap;
	};

char *copys(), *concat(), *subst(), *sprintf();
int *ckalloc();
struct nameblock *srchname(), *makename();
TIMETYPE exists();
# 2 "doname.c"










doname(p, reclevel, tval)
register struct nameblock *p;
int reclevel;
TIMETYPE *tval;
{
int errstat;
int okdel1;
int didwork;
TIMETYPE td, td1, tdep, ptime, ptime1, prestime();
register struct depblock *q;
struct depblock *qtemp, *srchdir(), *suffp, *suffp1;
struct nameblock *p1, *p2;
struct shblock *implcom, *explcom;
register struct lineblock *lp;
struct lineblock *lp1, *lp2;
char sourcename[100], prefix[100], temp[100], concsuff[20];
char *pnamep, *p1namep;
char *mkqlist();
struct chain *qchain, *appendq();

if(p == 0)
	{
	*tval = 0;
	return(0);
	}

if(dbgflag)
	{
	printf("doname(%s,%d)\n",p->namep,reclevel);
	fflush((&_iob[1]));
	}

if(p->done > 0)
	{
	*tval = p->modtime;
	return(p->done == 3);
	}

errstat = 0;
tdep = 0;
implcom = 0;
explcom = 0;
ptime = exists(p->namep);
ptime1 = 0;
didwork = 0;
p->done = 1;	

qchain = 0;



for(lp = p->linep ; lp ; lp = lp->nxtlineblock)
	for(q = lp->depp ; q ; q=qtemp )
		{
		qtemp = q->nxtdepblock;
		expand(q);
		}



for(lp = p->linep ; lp ; lp = lp->nxtlineblock)
	{
	td = 0;
	for(q = lp->depp ; q ; q = q->nxtdepblock)
		{
		errstat += doname(q->depname, reclevel+1, &td1);
		if(dbgflag)
		    printf("TIME(%s)=%ld\n", q->depname->namep, td1);
		if(td1 > td) td = td1;
		if(ptime < td1)
			qchain = appendq(qchain, q->depname->namep);
		}
	if(p->septype == 2)
		{
		if(lp->shp!=0)
		     if( ptime<td || (ptime==0 && td==0) || lp->depp==0)
			{
			okdel1 = okdel;
			okdel = 0;
			setvar("@", p->namep);
			setvar("?", mkqlist(qchain) );
			qchain = 0;
			if( !questflag )
				errstat += docom(lp->shp);
			setvar("@", (char *) 0);
			okdel = okdel1;
			ptime1 = prestime();
			didwork = 1;
			}
		}

	else	{
		if(lp->shp != 0)
			{
			if(explcom)
				fprintf((&_iob[2]), "Too many command lines for `%s'\n",
					p->namep);
			else	explcom = lp->shp;
			}

		if(td > tdep) tdep = td;
		}
	}



for(lp = sufflist ; lp ; lp = lp->nxtlineblock)
    for(suffp = lp->depp ; suffp ; suffp = suffp->nxtdepblock)
	{
	pnamep = suffp->depname->namep;
	if(suffix(p->namep , pnamep , prefix))
		{
		srchdir( concat(prefix,"*",temp) , 0, (struct depblock *) 0);
		for(lp1 = sufflist ; lp1 ; lp1 = lp1->nxtlineblock)
		    for(suffp1=lp1->depp ; suffp1 ; suffp1 = suffp1->nxtdepblock)
			{
			p1namep = suffp1->depname->namep;
			if( (p1=srchname(concat(p1namep, pnamep ,concsuff))) &&
			    (p2=srchname(concat(prefix, p1namep ,sourcename))) )
				{
				errstat += doname(p2, reclevel+1, &td);
				if(ptime < td)
					qchain = appendq(qchain, p2->namep);
if(dbgflag) printf("TIME(%s)=%ld\n", p2->namep, td);
				if(td > tdep) tdep = td;
				setvar("*", prefix);
				setvar("<", copys(sourcename));
				for(lp2=p1->linep ; lp2 ; lp2 = lp2->nxtlineblock)
					if(implcom = lp2->shp) break;
				goto endloop;
				}
			}
		}
	}

endloop:


if(errstat==0 && (ptime<tdep || (ptime==0 && tdep==0) ) )
	{
	ptime = (tdep>0 ? tdep : prestime() );
	setvar("@", p->namep);
	setvar("?", mkqlist(qchain) );
	if(explcom)
		errstat += docom(explcom);
	else if(implcom)
		errstat += docom(implcom);
	else if(p->septype == 0)
		if(p1=srchname(".DEFAULT"))
			{
			setvar("<", p->namep);
			for(lp2 = p1->linep ; lp2 ; lp2 = lp2->nxtlineblock)
				if(implcom = lp2->shp)
					{
					errstat += docom(implcom);
					break;
					}
			}
		else if(keepgoing)
			{
			printf("Don't know how to make %s\n", p->namep);
			++errstat;
			}
		else
			fatal1(" Don't know how to make %s", p->namep);

	setvar("@", (char *) 0);
	if(noexflag || (ptime = exists(p->namep)) == 0)
		ptime = prestime();
	}

else if(errstat!=0 && reclevel==0)
	printf("`%s' not remade because of errors\n", p->namep);

else if(!questflag && reclevel==0  &&  didwork==0)
	printf("`%s' is up to date.\n", p->namep);

if(questflag && reclevel==0)
	exit(ndocoms>0 ? -1 : 0);

p->done = (errstat ? 3 : 2);
if(ptime1 > ptime) ptime = ptime1;
p->modtime = ptime;
*tval = ptime;
return(errstat);
}

docom(q)
struct shblock *q;
{
char *s;
struct varblock *varptr();
int ign, nopr;
char string[2500];

++ndocoms;
if(questflag)
	return(0);

if(touchflag)
	{
	s = varptr("@")->varval;
	if(!silflag)
		printf("touch(%s)\n", s);
	if(!noexflag)
		touch(1, s);
	}

else for( ; q ; q = q->nxtshblock )
	{
	subst(q->shbp,string);

	ign = ignerr;
	nopr = 0;
	for(s = string ; *s=='-' || *s=='@' ; ++s)
		if(*s == '-')  ign = 1;
		else nopr = 1;

	if( docom1(s, ign, nopr) && !ign)
		if(keepgoing)
			return(1);
		else	fatal( (char *) 0);
	}
return(0);
}



docom1(comstring, nohalt, noprint)
register char *comstring;
int nohalt, noprint;
{
register int status;

if(comstring[0] == '\0') return(0);

if(!silflag && (!noprint || noexflag) )
	{
	printf("%s%s\n", (noexflag ? "" : prompt), comstring);
	fflush((&_iob[1]));
	}

if(noexflag) return(0);

if( status = dosys(comstring, nohalt) )
	{
	if( status>>8 )
		printf("*** Error code %d", status>>8 );
	else	printf("*** Termination code %d", status );

	if(nohalt) printf(" (ignored)\n");
	else	printf("\n");
	fflush((&_iob[1]));
	}

return(status);
}







expand(q)
register struct depblock *q;
{
register char *s;
char *s1;
struct depblock *p, *srchdir();

s1 = q->depname->namep;
for(s=s1 ; ;) switch(*s++)
	{
	case '\0':
		return;

	case '*':
	case '?':
	case '[':
		if( p = srchdir(s1 , 1, q->nxtdepblock) )
			{
			q->nxtdepblock = p;
			q->depname = 0;
			}
		return;
	}
}
