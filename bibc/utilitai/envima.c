/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF ENVIMA UTILITAI  DATE 12/05/2004   AUTEUR ROSE C.ROSE */
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

/* envima.c        constantes liees a l'arithmetique du processeur    */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <limits.h>

/* definition des constantes */

#if defined  CRAY
static long   ISUND  = 0605054000037000000000 ;
static long   ISMAX  = LONG_MAX ;
static double R8MAX  = DBL_MAX ;
static double R8MIN  = DBL_MIN ;
static double R8PREC = DBL_EPSILON ;
static long   ISMFIC = 4*1024*1024*1024 ;



#elif defined SOLARIS || HPUX  /* SPARC - ARCHITECTURE IEEE 754  */

/* les valeurs de R8MAX , R8MIN , R8UND peuvent etre aussi
   obtenues a l'aide des fonctions ieee (-lsunmath -lm) :
   max_normal(), min_normal(), quiet_nan()                  */

static long   ISUND    = 0x7fffffff ;
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;
static long   R8UND[2] = { 0x7ff00000 , 0x00000001 };
static long   ISMFIC   = LONG_MAX ;

#elif defined IRIX_32

static long   ISUND    = 0x7fffffff ;
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;
static long   R8UND[2] = { 0x7ff00000 , 0x00000001 };
static long   ISMFIC   = LONG_MAX ;


#elif defined IRIX_64  

static long   ISUND    = 0x7FFFFFFFFFFFFFFF ;
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;
static int R8UND[2]    = { 0x00000001 , 0x7ff00000 };
static long   ISMFIC   = 12884901888;

#elif defined TRU64 || SOLARIS64 

static long   ISUND    = 0x7FFFFFFFFFFFFFFF ;
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;
static int    R8UND[2] = { 0x00000000 , 0x7ff80000 };
static long   ISMFIC   = 12884901888;

#elif defined PPRO_NT || P_LINUX /* PENTIUM - ARCHITECTURE IEEE 754      */

static long   ISUND    = LONG_MAX ;
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;
static long   R8UND[2] = { 0x00000000 , 0x7ff80000 };
static long   ISMFIC   = LONG_MAX ;


#endif


#define  R8_PI   3.1415926535897932384626433832

#define  R8_T0   273.15

#if defined CRAY || SOLARIS || HPUX || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64  || PPRO_NT || P_LINUX
#define  R8GAME (sqrt(R8MAX*((double)1.-R8PREC)))
#else
#define  R8PREC (*(double*)R8UNPE-*(double*)R8UN)
#define  R8GAME (sqrt(*(double*)R8MAX*((double)1.-*(double*)R8UNPE+*(double*)R8UN)))
#endif


/* ---------------------- fonctions renvoyant un  LOGICAL (int)  */
/* int = logique : 1=vrai / 0=faux                               */


/* -------------------------------------------- MACHINE UTILISEE */

#if   defined  CRAY
#include <fortran.h>
int LCRAEM () {int l;l = _btol(1);return l;}
#elif defined SOLARIS || IRIX_32
int lcraem_() {return 0;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long lcraem_() {return 0;}
#elif defined HPUX
int lcraem() {return 0;}
#elif defined P_LINUX
int lcraem_() {return 0;}
#elif defined PPRO_NT
int __stdcall LCRAEM() {return 0;}
#endif

/* ---------------------- fonctions renvoyant un  INTEGER (int)  */

/* -------------------------------------------- LONGUEUR EN BITS */

#if   defined CRAY
int LBISEM  () {return 64;}
#elif defined SOLARIS
int lbisem_ () {return 32;}
#elif defined HPUX
int lbisem () {return 32;}
#elif defined IRIX_32
int lbisem_ () {return 32;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long lbisem_ () {return 64;}
#elif defined P_LINUX
int lbisem_ () {return 32;}
#elif defined PPRO_NT
int __stdcall LBISEM () {return 32;}
#endif

/* ----------------- LONGUEUR EN UNITES D'ADRESSAGE DE LA MACHINE*/

#if   defined CRAY
int LUISEM  () {return 1;}
#elif defined SOLARIS
int luisem_ () {return 4;}
#elif defined HPUX
int luisem () {return 4;}
#elif defined IRIX_32
int luisem_ () {return 4;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long luisem_ () {return 8;}
#elif defined P_LINUX
int luisem_ () {return 4;}
#elif defined PPRO_NT
int __stdcall LUISEM () {return 4;}
#endif

/* ------------------------------------------ LONGUEUR EN OCTETS */

#if   defined CRAY
int LOLSEM  () {return 8;}
#elif defined SOLARIS
int lolsem_ () {return 4;}
#elif defined HPUX
int lolsem () {return 4;}
#elif defined IRIX_32
int lolsem_ () {return 4;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long lolsem_ () {return 8;}
#elif defined P_LINUX
int lolsem_ () {return 4;}
#elif defined PPRO_NT
int __stdcall LOLSEM () {return 4;}
#endif

#if   defined CRAY
int LOISEM  () {return 8;}
#elif defined SOLARIS
int loisem_ () {return 4;}
#elif defined HPUX
int loisem () {return 4;}
#elif defined IRIX_32
int loisem_ () {return 4;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long loisem_ () {return 8;}
#elif defined P_LINUX
int loisem_ () {return 4;}
#elif defined PPRO_NT
int __stdcall LOISEM () {return 4;}
#endif

#if   defined CRAY
int LOR8EM  () {return 8;}
#elif defined SOLARIS || IRIX_32
int lor8em_ () {return 8;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long lor8em_ () {return 8;}
#elif defined HPUX
int lor8em () {return 8;}
#elif defined P_LINUX
int lor8em_ () {return 8;}
#elif defined PPRO_NT
int __stdcall LOR8EM () {return 8;}
#endif

#if   defined CRAY
int LOC8EM  () {return 16;}
#elif defined SOLARIS || IRIX_32
int loc8em_ () {return 16;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long loc8em_ () {return 16;}
#elif defined HPUX
int loc8em () {return 16;}
#elif defined P_LINUX
int loc8em_ () {return 16;}
#elif defined PPRO_NT
int __stdcall LOC8EM () {return 16;}
#endif

/* --------------- NOT.A.NUMBER (IEEE) OU UNDEF (CRAY NON IEEE) */
/* rq : ne veut rien dire pour un entier...mais utilise...      */

#if   defined CRAY
int ISNNEM  () {return (int)ISUND;}
#elif defined SOLARIS
int isnnem_ () {return (int)ISUND;}
#elif defined HPUX
int isnnem () {return (int)ISUND;}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
long isnnem_ () {return (long)ISUND;}
#elif defined P_LINUX
int isnnem_ () {return (int)ISUND;}
#elif defined PPRO_NT
int __stdcall ISNNEM () {return (int)ISUND;}
#endif

/* --------------------------- NOMBRE DE CHIFFRES SIGNIFICATIFS */
#if   defined CRAY
int NCISEM  () {return 19;}
#elif defined SOLARIS
int ncisem_ () {return 9;}
#elif defined HPUX
int ncisem () {return 9;}
#elif defined IRIX_32
int ncisem_() {return 9;} /* ???????????????? */
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long ncisem_() {return 19;} /* ???????????????? */
#elif defined P_LINUX
int ncisem_ () {return 9;}
#elif defined PPRO_NT
int __stdcall NCISEM () {return 9;}
#endif

#if   defined CRAY
int NCR8EM  () {return 14;}
#elif defined SOLARIS
int ncr8em_ () {return 16;}
#elif defined HPUX
int ncr8em () {return 16;}
#elif defined IRIX_32
int ncr8em_ () {return 16;} /* ???????????????? */
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long ncr8em_ () {return 16;} /* ???????????????? */
#elif defined P_LINUX
int ncr8em_ () {return 16;}
#elif defined PPRO_NT
int __stdcall NCR8EM () {return 16;}
#endif

/* ------------------------------------- VALEUR ENTIERE MAXIMALE */

#if   defined CRAY

int ISMAEM  () {return ISMAX;}
#elif defined SOLARIS
int ismaem_ () {return (int)ISMAX;}
#elif defined HPUX
int ismaem () {return (int)ISMAX;}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
long ismaem_ () {return (long)ISMAX;}
#elif defined P_LINUX
int ismaem_ () {return (int)ISMAX;}
#elif defined PPRO_NT
int __stdcall ISMAEM () {return (int)ISMAX;}
#endif

/* ---------------------------------  LONGUEUR UNITE D'ADRESSAGE */

#if   defined CRAY
int LOUAEM  () {return 8;}
#elif defined SOLARIS || IRIX_32
int louaem_ () {return 1;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long louaem_ () {return 1;}
#elif defined HPUX
int louaem () {return 1;}
#elif defined P_LINUX
int louaem_ () {return 1;}
#elif defined PPRO_NT
int __stdcall LOUAEM () {return 1;}
#endif

/* ------------------------------------------  TAILLE DE FICHIER */

#if   defined CRAY
int LOFIEM  () {return 523468800;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
int lofiem_ () {return 523468800;}
#elif defined HPUX
int lofiem () {return 523468800;}
#elif defined P_LINUX
int lofiem_ () {return 523468800;}
#elif defined PPRO_NT
int __stdcall LOFIEM () {return 523468800;}
#endif

/* ----------------------------------  TAILLE MAXIMUM DE FICHIER */

#if   defined CRAY
int MOFIEM  () {return ISMFIC;}
#elif defined SOLARIS
int mofiem_ () {return ISMFIC;}
#elif defined HPUX
int mofiem () {return ISMFIC;}
#elif defined IRIX_32  || IRIX_64 || TRU64 || SOLARIS64 
long mofiem_ () {return ISMFIC;}
#elif defined P_LINUX
int mofiem_ () {return ISMFIC;}
#elif defined PPRO_NT
int __stdcall MOFIEM () {return ISMFIC;}
#endif

/* ---------------------------------------------  POIDS DES BITS */

#if   defined CRAY
int ISPBEM  (int *jb) {return (int)pow(2.,(*jb-1));}
#elif defined SOLARIS
int ispbem_ (int *jb) {return (int)pow(2.,(*jb-1));}
#elif defined HPUX
int ispbem (int *jb) {return (int)pow(2.,(*jb-1));}
#elif defined IRIX_32  || IRIX_64 || TRU64 || SOLARIS64 
long ispbem_ (long *jb) {return (long)pow(2.,(*jb-1));}
#elif defined P_LINUX
int ispbem_ (int *jb) {return (int)pow(2.,(*jb-1));}
#elif defined PPRO_NT
int __stdcall ISPBEM (int *jb) {return (int)pow(2.,(*jb-1));}
#endif

/* ----------------------------------------  Base de numeration B */

#if   defined CRAY
int ISBAEM ()  {return 2;}
#elif defined SOLARIS || IRIX_32
int isbaem_ ()  {return 2;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long isbaem_ ()  {return 2;}
#elif defined HPUX
int isbaem ()  {return 2;}
#elif defined P_LINUX
int isbaem_()  {return 2;}
#elif defined PPRO_NT
int __stdcall ISBAEM () {return 2;}
#endif

/* ---------------  nombre de bits de la mantisse des flottants T */

#if   defined CRAY
int ISLBEM () {return 47;}
#elif defined SOLARIS
int islbem_ () {return 53;}
#elif defined HPUX
int islbem () {return 53;}
#elif defined IRIX_32
int islbem_ () {return 53;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long islbem_ () {return 53;}
#elif defined P_LINUX
int islbem_ () {return 53;}
#elif defined PPRO_NT
int __stdcall ISLBEM () {return 53;}
#endif

/* ---------------  exposant maximum des flottants en base 2 EMAX */

#if   defined CRAY
int IEMAEM () {return 8190;}
#elif defined SOLARIS
int iemaem_ () {return 1024;}
#elif defined HPUX
int iemaem () {return 1024;}
#elif defined IRIX_32
int iemaem_ () {return 1024;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long iemaem_ () {return 1024;}
#elif defined P_LINUX
int iemaem_ () {return 1024;}
#elif defined PPRO_NT
int __stdcall IEMAEM () {return 1024;}
#endif

/* ---------------  exposant minimum des flottants en base 2 EMIN */

#if   defined CRAY
int IEMIEM () {return -8189;}
#elif defined SOLARIS
int iemiem_ () {return -1021;}
#elif defined HPUX
int iemiem () {return -1021;}
#elif defined IRIX_32
int iemiem_ () {return -1021;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long iemiem_ () {return -1021;}
#elif defined P_LINUX
int iemiem_ () {return -1021;}
#elif defined PPRO_NT
int __stdcall IEMIEM () {return -1021;}
#endif

/* ---------------------- fonctions renvoyant un REAL*8 (double) */

/* --------------------- Plus petit increment relatif B**-T */

#if   defined CRAY
double RMIREM  () {return pow(2,-47);}
#elif defined SOLARIS
double rmirem_ () {return pow(2,-53);}
#elif defined HPUX
double rmirem () {return pow(2,-53);}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double rmirem_ () {return pow(2,-53);}
#elif defined P_LINUX
double rmirem_ () {return pow(2,-53);}
#elif defined PPRO_NT
double __stdcall RMIREM () {return pow(2,-53);}
#endif

/* -------------------- Plus grand increment relatif B**(1-T) */
/* cette valeur est normalment identique a R8PREM             */

#if   defined CRAY
double RMAREM  () {return pow(2,-46);}
#elif defined SOLARIS
double rmarem_ () {return pow(2,-52);}
#elif defined HPUX
double rmarem () {return pow(2,-52);}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double rmarem_ () {return pow(2,-52);}
#elif defined P_LINUX
double rmarem_ () {return pow(2,-52);}
#elif defined PPRO_NT
double __stdcall RMAREM () {return pow(2,-52);}
#endif

/* --------------------------- Plus petite valeur B**(EMIN-1) */
/* cette valeur est normalment identique a R8MIEM             */

#if   defined CRAY
double RMINEM  () {return pow(2,-8190);}
#elif defined SOLARIS
double rminem_ () {return pow(2,-1022);}
#elif defined HPUX
double rminem () {return pow(2,-1022);}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double rminem_ () {return pow(2,-1022);}
#elif defined P_LINUX
double rminem_ () {return pow(2,-1022);}
#elif defined PPRO_NT
double __stdcall RMINEM () {return pow(2,-1022);}
#endif

/* --------------- Plus grande valeur B**(EMAX-1) * (1-B**-T) */
/* cette valeur est normalment identique a R8MAEM             */

#if   defined CRAY
double RMAXEM  () {return (pow(2,8190)*(1.-pow(2,-47)));}
#elif defined SOLARIS
double rmaxem_ () {return (pow(2,1023)*(1.-pow(2,-53)));}
#elif defined HPUX
double rmaxem () {return (pow(2,1023)*(1.-pow(2,-53)));}
#elif defined IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double rmaxem_ () {return (pow(2,1023)*(1.-pow(2,-53)));}
#elif defined P_LINUX
double rmaxem_ () {return (pow(2,1023)*(1.-pow(2,-53)));}
#elif defined PPRO_NT
double __stdcall RMAXEM () {return (pow(2,1023)*(1.-pow(2,-53)));}
#endif



/* ---------------------REEL NOT.A.NUMBER (IEEE) OU UNDEF (CRAY) */

#if   defined CRAY
long R8NNEM  ()   {long *p; p=&ISUND; return(*p);}
#elif defined SOLARIS
double r8nnem_ () {return *(double*)R8UND;}
#elif defined HPUX
double r8nnem ()  {return *(double*)R8UND;}
#elif defined IRIX_32
double r8nnem_ () {return *(double*)R8UND;}
#elif defined IRIX_64 
double r8nnem_ () {return *(double*)R8UND;}
#elif defined TRU64 || SOLARIS64 
double r8nnem_ () {return *(double*)R8UND;}
#elif defined P_LINUX
double r8nnem_ () {return *(double*)R8UND;}
#elif defined PPRO_NT
double __stdcall R8NNEM () {return *(double*)R8UND;}
#endif

/* -------------------------------------- VALEUR MAXIMALE REELLE */

#if   defined CRAY
double R8MAEM  () {return R8MAX;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64  || P_LINUX
double r8maem_ () {return R8MAX;}
#elif defined HPUX
double r8maem () {return R8MAX;}
#elif defined PPRO_NT
double __stdcall R8MAEM () {return R8MAX;}
#endif

/* -------------------------------------- VALEUR MINIMALE REELLE */

#if   defined CRAY
double R8MIEM  () {return R8MIN;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64  || P_LINUX
double r8miem_ () {return R8MIN;}
#elif defined HPUX
double r8miem () {return R8MIN;}
#elif defined PPRO_NT
double __stdcall R8MIEM () {return R8MIN;}
#endif

/* ----------------------------  REEL A BOUCHER LES CASES (R8MAX)*/

#if   defined CRAY
long R8VIDE  () {long *p; p=&ISUND; return(*p);}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8vide_ () {return R8MAX;}
#elif defined HPUX
double r8vide () {return R8MAX;}
#elif defined P_LINUX
double r8vide_ () {return R8MAX;}
#elif defined PPRO_NT
double __stdcall R8VIDE () {return R8MAX;}
#endif

/* -----------------------------------------  BASE DE NUMERATION */


#if   defined CRAY
double R8BAEM  () {return (double)2.;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8baem_ () {return (double)2.;}
#elif defined HPUX
double r8baem () {return (double)2.;}
#elif defined P_LINUX
double r8baem_ () {return (double)2.;}
#elif defined PPRO_NT
double __stdcall R8BAEM () {return (double)2.;}
#endif

/* -----------------------------------------  PRECISION RELATIVE */

#if   defined CRAY
double R8PREM  () {return (double)R8PREC;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8prem_ () {return R8PREC;}
#elif defined HPUX
double r8prem () {return R8PREC;}
#elif defined P_LINUX
double r8prem_ () {return (double)R8PREC;}
#elif defined PPRO_NT
double __stdcall R8PREM () {return R8PREC;}
#endif


/* ----------------------------------  GAMME D"UTILISATION RELLE */

#if   defined CRAY
double R8GAEM  () {return (double)R8GAME;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8gaem_ () {return (double)R8GAME;}
#elif defined HPUX
double r8gaem () {return (double)R8GAME;}
#elif defined P_LINUX
double r8gaem_ () {return (double)R8GAME;}
#elif defined PPRO_NT
double __stdcall R8GAEM () {return (double)R8GAME;}
#endif


/* ----------- fonctions renvoyant des valeurs reelles diverses */

/* ------------------------------------------ VALXEM ZERO ABSOLU*/

#if   defined CRAY
double R8T0  () {return (double)R8_T0;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8t0_ () {return (double)R8_T0;}
#elif defined HPUX
double r8t0 () {return (double)R8_T0;}
#elif defined P_LINUX
double r8t0_ () {return (double)R8_T0;}
#elif defined PPRO_NT
double __stdcall R8T0 () {return (double)R8_T0;}
#endif

/* --------------------------------------------------- VALXEM PI*/

#if   defined CRAY
double R8PI  () {return (double)R8_PI;}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8pi_ () {return (double)R8_PI;}
#elif defined HPUX
double r8pi () {return (double)R8_PI;}
#elif defined P_LINUX
double r8pi_ () {return (double)R8_PI;}
#elif defined PPRO_NT
double __stdcall R8PI () {return (double)R8_PI;}
#endif

/* -------------------------------------------------- VALXEM 2PI*/

#if   defined CRAY
double R8DEPI  () {return (double)((double)2.*(double)R8_PI);}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8depi_ () {return (double)((double)2.*(double)R8_PI);}
#elif defined HPUX
double r8depi () {return (double)((double)2.*(double)R8_PI);}
#elif defined P_LINUX
double r8depi_ () {return (double)((double)2.*(double)R8_PI);}
#elif defined PPRO_NT
double __stdcall R8DEPI () {return (double)((double)2.*(double)R8_PI);}
#endif

/* ------------------------------------------------- VALXEM DGRD*/

#if   defined CRAY
double R8DGRD  () {return (double)((double)R8_PI/(double)180.);}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8dgrd_ () {return (double)((double)R8_PI/(double)180.);}
#elif defined HPUX
double r8dgrd () {return (double)((double)R8_PI/(double)180.);}
#elif defined P_LINUX
double r8dgrd_ () {return (double)((double)R8_PI/(double)180.);}
#elif defined PPRO_NT
double __stdcall R8DGRD () {return (double)((double)R8_PI/(double)180.);}
#endif

/* ------------------------------------------------- VALXEM RDDG*/

#if   defined CRAY
double R8RDDG  () {return (double)((double)180./(double)R8_PI);}
#elif defined SOLARIS || IRIX_32 || IRIX_64 || TRU64 || SOLARIS64 
double r8rddg_ () {return (double)((double)180./(double)R8_PI);}
#elif defined HPUX
double r8rddg () {return (double)((double)180./(double)R8_PI);}
#elif defined P_LINUX
double r8rddg_ () {return (double)((double)180./(double)R8_PI);}
#elif defined PPRO_NT
double __stdcall R8RDDG () {return (double)((double)180./(double)R8_PI);}
#endif

/* ------------------------------------ LONGUEUR de BLOC pour MULT_FRONT */
#if   defined CRAY
int LLBLOC  () {return 64;}
#elif defined SOLARIS
int llbloc_ () {return 64;}
#elif defined HPUX
int llbloc () {return 64;}
#elif defined IRIX_32
int llbloc_ () {return 64;}
#elif defined IRIX_64 || TRU64 || SOLARIS64 
long llbloc_ () {return 96;}
#elif defined P_LINUX
int llbloc_ () {return 32;}
#elif defined PPRO_NT
int __stdcall LLBLOC () {return 32;}
#endif
