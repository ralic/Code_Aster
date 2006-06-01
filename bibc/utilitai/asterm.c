/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF main utilitai  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2001  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* RESPONSABLE                                 D6BHHJP J.P.LEFEBVRE   */
/* ------------------------------------------------------------------ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
#include "UTILITES.h"
*/
# define DEBUT(chaine)
# define FIN(chaine)
# define SSCRUTE(chaine)
# define DSCRUTE(val)
# define ASSERT(condition)

#if defined CRAY || SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#include <sys/utsname.h> /* Pour le nom de la machine d'execution */
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#define INIAST iniast_
#define LMEMEX lmemex_
#define ISINTE isinte_
#define ISSUIV issuiv_
#define IVERIF iverif_
#define ISDBGJ isdbgj_
#define MEMJVX memjvx_
#define REPSPY repspy_
#define REPMAT repmat_
#define REPOUT repout_
#define REPDEX repdex_
#define ORIGIN origin_
#define NODNAM nodnam_
#define ASTER  aster_
#define VERSIO versio_
#define SEGJVX segjvx_
#define LSEGJV lsegjv_
#define VPARJV vparjv_
#define MAXBAS maxbas_
void aster_( void );
extern long  repspy_ (long *a, long *l, char *rep, unsigned long l_rep);
extern long  repmat_ (long *a, long *l, char *rep, unsigned long l_rep);
extern long  repout_ (long *a, long *l, char *rep, unsigned long l_rep);
extern long  repdex_ (long *a, long *l, char *rep, unsigned long l_rep);
extern void  nodnam_ (long *a, char *n1, char *n2, char *n3, unsigned long l1, unsigned long l2, unsigned long l3);
extern void  origin_ (long *a, char *ori, unsigned long l_ori);
extern void  VERSIO (long *a, long *b, long *c, char *v, long *d, unsigned long *l);
extern long  INIAST (long *a, long *b, long *c);
extern long  SEGJVX (long *a);
extern long  LSEGJV (long *a);
extern double VPARJV (double *a);
#elif defined PPRO_NT
extern long __stdcall INIAST (long *a, long *b, long *c);
extern long __stdcall LMEMEX (long *a);
extern long __stdcall MEMJVX (double *a);
extern long __stdcall REPSPY (long *a, long *l, char *rep, unsigned long l_rep);
extern long __stdcall REPMAT (long *a, long *l, char *rep, unsigned long l_rep);
extern long __stdcall REPOUT (long *a, long *l, char *rep, unsigned long l_rep);
extern long __stdcall REPDEX (long *a, long *l, char *rep, unsigned long l_rep);
extern void __stdcall NODNAM (long *a, char *n1, unsigned long l1, char *n2, unsigned long l2, char *n3, unsigned long l3);
extern void __stdcall ASTER ();
extern long __stdcall ISINTE (long *a);
extern long __stdcall ISSUIV (long *a);
extern long __stdcall IVERIF (long *a);
extern long __stdcall ISDBGJ (long *a);
extern void __stdcall ORIGIN (long *a, char *ori, unsigned long l_ori);
extern void __stdcall VERSIO (long *a, long *b, long *c, char *v, unsigned long *l, long *d);
extern long __stdcall SEGJVX (long *a);
extern long __stdcall LSEGJV (long *a);
extern double __stdcall VPARJV (double *a);
extern double __stdcall MAXBAS (double *a);
#elif defined HPUX
#define INIAST iniast
#define LMEMEX lmemex
#define ISINTE isinte
#define ISSUIV issuiv
#define IVERIF iverif
#define ISDBGJ isdbgj
#define MEMJVX memjvx
#define REPSPY repspy
#define REPMAT repmat
#define REPOUT repout
#define REPDEX repout
#define ORIGIN origin
#define NODNAM nodnam
#define ASTER  aster
#define VERSIO versio
#define SEGJVX segjvx
#define LSEGJV lsegjv
#define VPARJV vparjv
#define MAXBAS maxbas
extern long lmemex (long *a);
extern long memjvx (double *a);
extern long repspy (long *a, long *l, char *rep, unsigned long l_rep);
extern long repmat (long *a, long *l, char *rep, unsigned long l_rep);
extern long repout (long *a, long *l, char *rep, unsigned long l_rep);
extern long repdex (long *a, long *l, char *rep, unsigned long l_rep);
extern void nodnam (long *a, char *n1, char *n2, char *n3, unsigned long l1, unsigned long l2, unsigned long l3);
extern void aster ();
extern long isinte (long *a);
extern long issuiv (long *a);
extern long iverif (long *a);
extern long isdbgj (long *a);
extern void origin (long *a, char *ori, unsigned long l_ori);
extern void VERSIO(long *a, long *b, long *c, char *v, long *d, unsigned long *l);
extern long INIAST (long *a, long *b, long *c);
extern long SEGJVX (long *a);
extern long LSEGJV (long *a);
extern double VPARJV (double *a);
#endif

void strmaju (char *namin, char *namaj , int l)
{ 
	int iin, jjn;
	char *p,*q;

	p=namin;
	q=namaj;

	/* troncature a l caracteres et passage en majuscules */

	iin=0;
	while ((*p != '\0') && (iin < l)) {
		*q++=toupper(*p++);
		iin++;
	}
	for (jjn=iin;jjn<l;jjn++) *q++=' ';
	*q='\0';
}

void strcpBS (char *namin, char *namaj , int l , long *ll)
{ 
	int iin, jjn;
	char *p,*q;
#if defined CRAY
#define BS  '/'
#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#define BS  '/'
#elif defined PPRO_NT
#define BS  0x5c
#endif

	p=namin;
	q=namaj;

	/* troncature a l caracteres  */

	iin=0;
	while ((*p != '\0') && (iin < l)) {
		*q++=*p++;
		iin++;
	}
	if ( iin != 0) { 
		*q++=BS; 
		iin++; 
		*ll=iin;
	}
	for (jjn=iin;jjn<l;jjn++) *q++=' ';
	*q='\0';
}

char g_memory[32],g_tpmax[32];

void asterm(long argc,char** argv)
/*
** Programme principal d'Aster pour enrober le Code_Aster
** afin de traiter les arguments de la ligne de commande
** et positionner des indicateurs en consequence.
*/
{
	long ivers,iutil,iniv,ilog;
	unsigned long ldate;
	char vdate[9];
	long cerr,inter,iret;
	long r1,r2,r3;
	unsigned long l_argv,l_ori,l_nom = 16;

	DEBUT("asterm") ;
	/*
** Initialisation
*/


	iret=INIAST(&r1,&r2,&r3);


	vdate[8] = '\0' ;
#if defined PPRO_NT
	VERSIO (&ivers,&iutil,&iniv,&vdate[0],&ldate,&ilog);
#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
	VERSIO (&ivers,&iutil,&iniv,&vdate[0],&ilog,&ldate);
#endif
	*argv ++;

	/* Nom de la machine */
	{
		char nodename[17],nomos[17],nomcpu[17];
		char *pn,*os,*mach;
		long fino=0;

#if defined CRAY || HPUX || SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		struct utsname un;
		uname(&un);
		pn=un.nodename;
		os=un.sysname;
		mach=un.machine;
#elif defined PPRO_NT
		pn=getenv ("COMPUTERNAME");
		if ( pn == NULL ) {
			pn="????";
		}
		os=getenv ("OS");
		if ( os == NULL ) {
			os="????";
		}
		mach=getenv ("CPU");
		if ( mach == NULL ) {
			mach="????";
		}
#endif
		strmaju (pn, nodename, 16);
		strmaju (os, nomos, 16);
		strmaju (mach, nomcpu, 16);
#if defined CRAY
		NODNAM(&fino,nodename,nomos,nomcpu);
#elif defined PPRO_NT
		NODNAM(&fino,nodename,l_nom,nomos,l_nom,nomcpu,l_nom);
#elif defined HPUX || SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		NODNAM(&fino,nodename,nomos,nomcpu,l_nom,l_nom,l_nom);
#endif
	}

#if defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
	g_memory[0] = '\0';
	g_tpmax[0] = '\0';
#endif
	/*
** Init pour le repertoire associe a spycod
*/
#if defined CRAY
#define REP_SPY "/aster/stat/"
#define LREP  12
#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#define REP_SPY "/export/docaster/asa/aster/stat/"
#define LREP  32
#elif defined PPRO_NT
#define REP_SPY " "
#define LREP  0
#endif
	{
		char rep_spy[129];
		long fi=0;
		long ll;
		unsigned long l_rep;
		strcpy(rep_spy,REP_SPY);
		ll = LREP;
#if defined CRAY
		REPSPY(&fi,&ll,rep_spy);
#elif defined SOLARIS || HPUX || IRIX || PPRO_NT || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		l_rep = strlen(rep_spy);
		REPSPY(&fi,&ll,rep_spy,l_rep);
#endif
	}
	/*
** Init pour le repertoire associe au catalogue materiau
   pour les scripts appelables depuis aster et pour les
   donnees lues depuis aster
*/
#if defined CRAY || IRIX || TRU64 || LINUX64 || SOLARIS64 
#define REP_MAT "/aster/materiau/"
#define REP_OUT "/aster/outils/"
#define REP_DON "/aster/donnees/"
#elif defined SOLARIS || HPUX || P_LINUX 
#define REP_MAT "/logiciels/aster/materiau/"
#define REP_OUT "/logiciels/aster/outils/"
#define REP_DON "/logiciels/aster/donnees/"
#elif defined PPRO_NT
#define REP_MAT "C:\\ASTER\\MATERIAU\\"
#define REP_OUT "D:\\Progra~1\\"
#define REP_DON "C:\\ASTER\\DONNEES\\"
#endif
	{ 
		char rep_mat[129],rep_out[129],rep_don[129];
		long fi=0;
		long ll;
		unsigned long l_rep=0;
		strcpy(rep_mat,REP_MAT);
		ll = strlen(rep_mat);
#if defined CRAY
		REPMAT(&fi,&ll,rep_mat);
#elif defined SOLARIS || HPUX || IRIX || PPRO_NT || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		REPMAT(&fi,&ll,rep_mat,l_rep);
#endif
		strcpy(rep_out,REP_OUT);
		ll = strlen(rep_out);
#if defined CRAY
		REPOUT(&fi,&ll,rep_out);
#elif defined SOLARIS || HPUX || IRIX || PPRO_NT || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		REPOUT(&fi,&ll,rep_out,l_rep);
#endif
		strcpy(rep_don,REP_DON);
		ll = strlen(rep_don);
#if defined CRAY
		REPDEX(&fi,&ll,rep_don);
#elif defined SOLARIS || HPUX || IRIX || PPRO_NT || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
		REPDEX(&fi,&ll,rep_don,l_rep);
#endif
	}
	/*
** Traitement des arguments de la ligne de commande
*/
	while (*argv != NULL) {
		ASSERT(argv!=NULL) ; ASSERT(*argv!=NULL) ; SSCRUTE(*argv) ;
		/*
   ** Execution en interactif
   */
		if (strcmp(*argv,"-i") == 0) {
			inter=1;
			cerr=ISINTE(&inter);
		}
		/*
   ** Suivi interactif d'un batch
   */
		if (strcmp(*argv,"-suivi_batch") == 0) {
			inter=1;
			cerr=ISSUIV(&inter);
		}
		/*
   ** Verification de la syntaxe des commandes
   */
		if (strcmp(*argv,"-verif") == 0) {
			inter=1;
			cerr=IVERIF(&inter);
		}
		/*
   ** Limite memoire
   */
		if (strcmp(*argv,"-mem") == 0){
			*argv++;
			strcpy(g_memory,*argv);
			inter=1;
			cerr=LMEMEX(&inter);
		}
		/*
   ** Debug JEVEUX
   */
		if (strcmp(*argv,"-dbgjeveux") == 0) {
			inter=1;
			cerr=ISDBGJ(&inter);
		}
		/*
   ** Memoire JEVEUX
   */
		if (strcmp(*argv,"-memjeveux") == 0) {
			double finter;
			*argv++;
			finter=(double) atof(*argv);
			DSCRUTE(finter) ;
			cerr=MEMJVX(&finter);
		}
		/*
   ** Type parcours de la segmentation Memoire JEVEUX
   */
		if (strcmp(*argv,"-type_alloc") == 0) {
			long typseg;
			*argv++;
			typseg=atol(*argv);
			cerr=SEGJVX(&typseg);
		}
		/*
   ** Taille des segments de valeurs associ‰s (parcours de type 3)
   */
		if (strcmp(*argv,"-taille") == 0) {
			long lseg;
			*argv++;
			lseg=atol(*argv);
			cerr=LSEGJV(&lseg);
		}
		/*
   ** Partition memoire (parcours de type 4)
   */
		if (strcmp(*argv,"-partition") == 0) {
			double vpar,rerr;
			*argv++;
			vpar=(double)atof(*argv);
			rerr=VPARJV(&vpar);
		}
		/*
   ** Taille maximale des bases (en mega-octets)
   */
		if (strcmp(*argv,"-max_base") == 0) {
			double tmax,rerr;
			*argv++;
			tmax=(double) atof(*argv);
			rerr=MAXBAS(&tmax);
		}
		/*
   ** Repertoire de stockage des informations spycod
   */
		if (strcmp(*argv,"-rep") == 0) {
			char rep[129];
			unsigned long l_rep;
			long fi=0;
			long ll;
			char *p;
			*argv++;
			p=*argv;
			l_rep = strlen(p);
			strcpBS (p,rep,l_rep,&ll);
			REPSPY(&fi,&ll,rep,l_rep);
		}
		/*
   ** Repertoire des fichiers du catalogue materiau
   */
		if (strcmp(*argv,"-rep_mat") == 0) {
			char rep[129];
			unsigned long l_rep;
			long fi=0;
			long ll;
			char *p;
			*argv++;
			p=*argv;
			l_rep = strlen(p);
			strcpBS (p,rep,l_rep,&ll);
			REPMAT(&fi,&ll,rep,l_rep);
		}
		/*
   ** Repertoire des outils (logiciels externes, scripts)
   */
		if (strcmp(*argv,"-rep_outils") == 0) {
			char rep[129];
			unsigned long l_rep;
			long fi=0;
			long ll;
			char *p;
			*argv++;
			p=*argv;
			l_rep = strlen(p);
			strcpBS (p,rep,l_rep,&ll);
			REPOUT(&fi,&ll,rep,l_rep);
		}
		/*
   ** Repertoire des donnees externes
   */
		if (strcmp(*argv,"-rep_dex") == 0) {
			char rep[129];
			unsigned long l_rep;
			long fi=0;
			long ll;
			char *p;
			*argv++;
			p=*argv;
			l_rep = strlen(p);
			strcpBS (p,rep,l_rep,&ll);
			REPDEX(&fi,&ll,rep,l_rep);
		}
		/*
   ** Limite de temps CPU en secondes
   */
		if (strcmp(*argv,"-tpmax") == 0) {
			*argv++;
			strcpy(g_tpmax,*argv);
		}
		/*
   ** Application d'origine
   */
		if (strcmp(*argv,"-origine") == 0) {
			long fi=0;
			char ori[17];
			long ii=0;
			long jj;
			char* p;
			char* q;

			/* Formatage d'une chaine de caracteres compatible avec le FORTRAN */
			*argv++;
			p=*argv;
			q=ori;
			while ((*p != '\0') && (ii < 16)) {
				*q++=toupper(*p++);
				ii++;
			}
			for (jj=ii;jj<16;jj++) *q++=' ';
			*q='\0';
#if defined CRAY
			ORIGIN(&fi,ori);
#elif defined SOLARIS || HPUX || IRIX || PPRO_NT || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
			l_ori = strlen(ori);
			ORIGIN(&fi,ori,l_ori);
#endif
		}
		/*
   ** Aide en ligne
   */
		else if ((strcmp(*argv,"-help") == 0) ||
		    (strcmp(*argv,"-h")    == 0) ||
		    (strcmp(*argv,"-aide") == 0) ) {
			printf("USAGE :\n");
			printf("     -i              : execution en interactif\n");
			printf("     -suivi_batch    : suivi en interactif d'un batch\n");
			printf("                     : (le buffer du fichier de sortie est vide a chaque ligne) \n");
			printf("     -verif          : uniquement verification de la syntaxe des commandes\n");
			printf("     -dbgjeveux      : mode debug JEVEUX (positionnement a UNDEF des objets liberes)\n");
			printf("     -mem            : limite memoire superieure pour l'allocation des structures JEVEUX (en Mw)\n");
			printf("     -memjeveux val  : memoire exacte a allouer pour les structures JEVEUX (en Mw)\n");
			printf("     -type_alloc val : mode de parcours de la segmentation memoire\n");
			printf("     -taille val     : taille (en w) des segments de valeurs alloues en fin de memoire (parcours de type 3)\n");
			printf("     -partition val  : pourcentage (0.0<val<1.0) de la memoire allouee affectee a la premiere partition\n");
			printf("     -rep val        : repertoire d'accueil pour les statistiques \n");
			printf("     -rep_mat val    : repertoire des fichiers du catalogue materiau \n");
			printf("     -rep_outils val : repertoire des scripts lances par Aster \n");
			printf("     -rep_dex    val : repertoire des donnees externes lues par Aster \n");
			printf("     -tpmax val      : temps CPU maximum en secondes pour le passage \n");
			printf("     -origine val    : application d'origine qui utilise le Code_Aster(pour les statistiques)\n");
			printf("     -max_base val   : taille maximale en mega-octets des bases JEVEUX\n");
			printf("     -help ou -h ou -aide : le present message\n");
			exit(0);
		}
		*argv++;
	}

	/*
** Appel au FORTRAN
*/
	/*----------------- ASTER();*/

	/*
** On ne passe jamais par ici !!!
** ASTER() sort avec un STOP dans JEFINI
** SI !!! avec python on se glisse par ICI (AY)
*/
	FIN("asterm") ;
	return ;
}
