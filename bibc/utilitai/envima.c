/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF ENVIMA UTILITAI  DATE 26/09/2011   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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

#include <float.h>
#include "aster.h"


/* Définition des constantes */

/* undef entier et réel */
#ifdef _USE_64_BITS
static long   ISUND    = 0x7FFFFFFFFFFFFFFF ;
#else
static long   ISUND    = LONG_MAX ;
#endif

#ifdef _NAN_BULL
static unsigned short R8UND[4] = {0, 65535, 65535, 65527};
#else
#ifdef _USE_64_BITS
static int    R8UND[2] = { 0x00000000 , 0x7ff80000 };
#else
static long   R8UND[2] = { 0x00000000 , 0x7ff80000 };
#endif
#endif

/* entier max, réel max, réel min, précision en réel simple et double */
static long   ISMAX    = LONG_MAX ;
static DOUBLE R8MAX    = DBL_MAX ;
static DOUBLE R8MIN    = DBL_MIN ;
static DOUBLE R8PREC   = DBL_EPSILON ;
static float  R4MAX    = FLT_MAX ;
static float  R4MIN    = FLT_MIN ;
static float  R4PREC   = FLT_EPSILON ;

/* taille max d'une base */
static INTEGER   ISMFIC   = 50331648;
/* taille max d'un fichier "extend" */
#ifdef _USE_64_BITS
static INTEGER   ISLFIC   = 12582912;
#else
static INTEGER   ISLFIC   = 2000*1024;
#endif

#define  R8_PI   3.1415926535897932384626433832
#define  R8_T0   273.15
#define  R8GAME (sqrt(R8MAX*((DOUBLE)1.-R8PREC)))


/* ---------------------- fonctions renvoyant un  LOGICAL (int)  */
/* int = logique : 1=vrai / 0=faux                               */


/* -------------------------------------------- MACHINE UTILISEE */
INTEGER STDCALL(LCRAEM,lcraem)() { return 0; }

/* ----------------------------- fonctions renvoyant un  INTEGER */
/* -------------------------------------------- LONGUEUR EN BITS */
INTEGER STDCALL(LBISEM,lbisem)() { return LONG_INTEGER_BITS; }

/* ------------------------------------------ LONGUEUR EN OCTETS */
INTEGER STDCALL(LOLSEM,lolsem)() { return LONG_INTEGER_MOTS; }
INTEGER STDCALL(LOISEM,loisem)() { return LONG_INTEGER_MOTS; }
INTEGER STDCALL(LOR8EM,lor8em)() { return LONG_REAL_MOTS; }
INTEGER STDCALL(LOC8EM,loc8em)() { return LONG_COMPLEX_MOTS; }

/* --------------- NOT.A.NUMBER (IEEE) OU UNDEF (CRAY NON IEEE) */
/* rq : ne veut rien dire pour un entier...mais utilise...      */
INTEGER STDCALL(ISNNEM,isnnem)() { return (INTEGER)ISUND; }

/* --------------------------- NOMBRE DE CHIFFRES SIGNIFICATIFS */
INTEGER STDCALL(NCISEM,ncisem)() { return INTEGER_NB_CHIFFRES_SIGNIFICATIFS; }
INTEGER STDCALL(NCR8EM,ncr8em)() { return REAL_NB_CHIFFRES_SIGNIFICATIFS; }

/* ------------------------------------- VALEUR ENTIERE MAXIMALE */
INTEGER STDCALL(ISMAEM,ismaem)() { return (INTEGER)ISMAX; }

/* ------------------------------------------  TAILLE DE FICHIER
 exprimee en kilo (1024) */
INTEGER STDCALL(LOFIEM,lofiem)() {
   return (INTEGER)ISLFIC;
}

/* ----------------------------------  TAILLE MAXIMUM DE FICHIER */
INTEGER STDCALL(MOFIEM,mofiem)() { return (INTEGER)ISMFIC; }

/* ---------------------------------------------  POIDS DES BITS */
INTEGER DEFP(ISPBEM, ispbem, INTEGER *jb) { return (INTEGER)pow(2.,(*jb-1)); }

/* ----------------------------------------  Base de numeration B */
INTEGER STDCALL(ISBAEM,isbaem)() { return 2; }

/* ---------------  nombre de bits de la mantisse des flottants T */
/* reste du Cray : 47 */
INTEGER STDCALL(ISLBEM,islbem)() { return 53; }

/* ---------------  exposant maximum des flottants en base 2 EMAX */
/* reste du Cray : 8190 */
INTEGER STDCALL(IEMAEM,iemaem)() { return 1024; }

/* ---------------  exposant minimum des flottants en base 2 EMIN */
/* reste du Cray : -8189 */
INTEGER STDCALL(IEMIEM,iemiem)() { return -1021; }

/* ---------------------- fonctions renvoyant un REAL*8 (DOUBLE) */
/* --------------------- Plus petit increment relatif B**-T */
DOUBLE STDCALL(RMIREM,rmirem)() { return pow(2,-53); }

/* -------------------- Plus grand increment relatif B**(1-T) */
/* cette valeur est normalment identique a R8PREM             */
DOUBLE STDCALL(RMAREM,rmarem)() { return pow(2,-52); }

/* --------------------------- Plus petite valeur B**(EMIN-1) */
/* cette valeur est normalment identique a R8MIEM             */
DOUBLE STDCALL(RMINEM,rminem)() { return pow(2,-1022); }

/* --------------- Plus grande valeur B**(EMAX-1) * (1-B**-T) */
/* cette valeur est normalment identique a R8MAEM             */
DOUBLE STDCALL(RMAXEM,rmaxem)() { return pow(2,1023)*(1.-pow(2,-53)); }

/* ---------------------REEL NOT.A.NUMBER (IEEE) OU UNDEF (CRAY) */
DOUBLE STDCALL(R8NNEM,r8nnem)() { return *(DOUBLE*)R8UND; }

/* -------------------------------------- VALEUR MAXIMALE REELLE R8*/
DOUBLE STDCALL(R8MAEM,r8maem)() { return R8MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R8*/
DOUBLE STDCALL(R8MIEM,r8miem)() { return R8MIN; }

/* ----------------------------  REEL A BOUCHER LES CASES (R8MAX)*/
DOUBLE STDCALL(R8VIDE,r8vide)() { return R8MAX; }

/* -----------------------------------------  BASE DE NUMERATION */
DOUBLE STDCALL(R8BAEM,r8baem)() { return (DOUBLE)2.; }

/* -----------------------------------------  PRECISION RELATIVE  R8*/
DOUBLE STDCALL(R8PREM,r8prem)() { return R8PREC; }


/* ----------------------------------  GAMME D"UTILISATION RELLE */
DOUBLE STDCALL(R8GAEM,r8gaem)() { return (DOUBLE)R8GAME; }


/* ----------- fonctions renvoyant des valeurs reelles diverses */
/* ------------------------------------------ VALXEM ZERO ABSOLU*/
DOUBLE STDCALL(R8T0,r8t0)() { return (DOUBLE)R8_T0; }

/* --------------------------------------------------- VALXEM PI*/
DOUBLE STDCALL(R8PI,r8pi)() { return (DOUBLE)R8_PI; }

/* -------------------------------------------------- VALXEM 2PI*/
DOUBLE STDCALL(R8DEPI,r8depi)() { return (DOUBLE)((DOUBLE)2.*(DOUBLE)R8_PI); }

/* ------------------------------------------------- VALXEM DGRD*/
DOUBLE STDCALL(R8DGRD,r8dgrd)() { return (DOUBLE)((DOUBLE)R8_PI/(DOUBLE)180.); }

/* ------------------------------------------------- VALXEM RDDG*/
DOUBLE STDCALL(R8RDDG,r8rddg)() { return (DOUBLE)((DOUBLE)180./(DOUBLE)R8_PI); }

/* ------------------------------------ LONGUEUR de BLOC pour MULT_FRONT */
INTEGER STDCALL(LLBLOC,llbloc)() { return OPT_TAILLE_BLOC_MULT_FRONT; }

/* ----------------------------------------  Pour tester un NaN */
/* on fait un chapeau (iisnan) à la fonction C isnan  pour éviter le
 * conflit avec la fonction intrinsèque (logique) isnan de fortran 95 */
INTEGER DEFP(IISNAN, iisnan, DOUBLE *x) {
    if ( isnan(*x) ) return (INTEGER)1;
    return (INTEGER)0;
}

/* -------------------------------------- VALEUR MAXIMALE REELLE R4*/
DOUBLE STDCALL(R4MAEM,r4maem)() { return R4MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R4*/
DOUBLE STDCALL(R4MIEM,r4miem)() { return R4MIN; }

/* -----------------------------------------  PRECISION RELATIVE  R4*/
DOUBLE STDCALL(R4PREM,r4prem)() { return R4PREC; }
