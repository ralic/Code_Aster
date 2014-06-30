/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
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

#ifdef _USE_64_BITS
static int    R8UND[2] = { 0x00000000 , 0x7ff80000 };
#else
static long   R8UND[2] = { 0x00000000 , 0x7ff80000 };
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


/* ----------------------------- fonctions renvoyant un  INTEGER */
/* -------------------------------------------- LONGUEUR EN BITS */
INTEGER DEF0(LBISEM,lbisem) { return 8 * ASTER_INT_SIZE; }

/* ------------------------------------------ LONGUEUR EN OCTETS */
INTEGER DEF0(LOLSEM,lolsem) { return ASTER_LOGICAL_SIZE; }
INTEGER DEF0(LOISEM,loisem) { return ASTER_INT_SIZE; }
INTEGER DEF0(LOR8EM,lor8em) { return ASTER_REAL8_SIZE; }
INTEGER DEF0(LOC8EM,loc8em) { return ASTER_COMPLEX_SIZE; }

/* --------------- NOT.A.NUMBER (IEEE) OU UNDEF (CRAY NON IEEE) */
/* rq : ne veut rien dire pour un entier...mais utilise...      */
INTEGER DEF0(ISNNEM,isnnem) { return (INTEGER)ISUND; }

/* --------------------------- NOMBRE DE CHIFFRES SIGNIFICATIFS */
INTEGER DEF0(NCISEM,ncisem) { return INTEGER_NB_CHIFFRES_SIGNIFICATIFS; }
INTEGER DEF0(NCR8EM,ncr8em) { return REAL_NB_CHIFFRES_SIGNIFICATIFS; }

/* ------------------------------------- VALEUR ENTIERE MAXIMALE */
INTEGER DEF0(ISMAEM,ismaem) { return (INTEGER)ISMAX; }

/* ------------------------------------------  TAILLE DE FICHIER
 exprimee en kilo (1024) */
INTEGER DEF0(LOFIEM,lofiem) {
   return (INTEGER)ISLFIC;
}

/* ----------------------------------  TAILLE MAXIMUM DE FICHIER */
INTEGER DEF0(MOFIEM,mofiem) { return (INTEGER)ISMFIC; }

/* ---------------------------------------------  POIDS DES BITS */
INTEGER DEFP(ISPBEM, ispbem, INTEGER *jb) { return (INTEGER)pow(2.,(*jb-1)); }

/* ----------------------------------------  Base de numeration B */
INTEGER DEF0(ISBAEM,isbaem) { return 2; }

/* ---------------  nombre de bits de la mantisse des flottants T */
/* reste du Cray : 47 */
INTEGER DEF0(ISLBEM,islbem) { return 53; }

/* ---------------  exposant maximum des flottants en base 2 EMAX */
/* reste du Cray : 8190 */
INTEGER DEF0(IEMAEM,iemaem) { return 1024; }

/* ---------------  exposant minimum des flottants en base 2 EMIN */
/* reste du Cray : -8189 */
INTEGER DEF0(IEMIEM,iemiem) { return -1021; }

/* ---------------------- fonctions renvoyant un REAL*8 (DOUBLE) */
/* --------------------- Plus petit increment relatif B**-T */
DOUBLE DEF0(RMIREM,rmirem) { return pow(2,-53); }

/* -------------------- Plus grand increment relatif B**(1-T) */
/* cette valeur est normalment identique a R8PREM             */
DOUBLE DEF0(RMAREM,rmarem) { return pow(2,-52); }

/* --------------------------- Plus petite valeur B**(EMIN-1) */
/* cette valeur est normalment identique a R8MIEM             */
DOUBLE DEF0(RMINEM,rminem) { return pow(2,-1022); }

/* --------------- Plus grande valeur B**(EMAX-1) * (1-B**-T) */
/* cette valeur est normalment identique a R8MAEM             */
DOUBLE DEF0(RMAXEM,rmaxem) { return pow(2,1023)*(1.-pow(2,-53)); }

/* ---------------------REEL NOT.A.NUMBER (IEEE) OU UNDEF (CRAY) */
DOUBLE DEF0(R8NNEM,r8nnem) { return *(DOUBLE*)R8UND; }

/* -------------------------------------- VALEUR MAXIMALE REELLE R8*/
DOUBLE DEF0(R8MAEM,r8maem) { return R8MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R8*/
DOUBLE DEF0(R8MIEM,r8miem) { return R8MIN; }

/* ----------------------------  REEL A BOUCHER LES CASES (R8MAX)*/
DOUBLE DEF0(R8VIDE,r8vide) { return R8MAX; }

/* -----------------------------------------  BASE DE NUMERATION */
DOUBLE DEF0(R8BAEM,r8baem) { return (DOUBLE)2.; }

/* -----------------------------------------  PRECISION RELATIVE  R8*/
DOUBLE DEF0(R8PREM,r8prem) { return R8PREC; }


/* ----------------------------------  GAMME D"UTILISATION RELLE */
DOUBLE DEF0(R8GAEM,r8gaem) { return (DOUBLE)R8GAME; }


/* ----------- fonctions renvoyant des valeurs reelles diverses */
/* ------------------------------------------ VALXEM ZERO ABSOLU*/
DOUBLE DEF0(R8T0,r8t0) { return (DOUBLE)R8_T0; }

/* --------------------------------------------------- VALXEM PI*/
DOUBLE DEF0(R8PI,r8pi) { return (DOUBLE)R8_PI; }

/* -------------------------------------------------- VALXEM 2PI*/
DOUBLE DEF0(R8DEPI,r8depi) { return (DOUBLE)((DOUBLE)2.*(DOUBLE)R8_PI); }

/* ------------------------------------------------- VALXEM DGRD*/
DOUBLE DEF0(R8DGRD,r8dgrd) { return (DOUBLE)((DOUBLE)R8_PI/(DOUBLE)180.); }

/* ------------------------------------------------- VALXEM RDDG*/
DOUBLE DEF0(R8RDDG,r8rddg) { return (DOUBLE)((DOUBLE)180./(DOUBLE)R8_PI); }

/* ------------------------------------ LONGUEUR de BLOC pour MULT_FRONT */
INTEGER DEF0(LLBLOC,llbloc) { return OPT_TAILLE_BLOC_MULT_FRONT; }

/* ----------------------------------------  Pour tester un NaN */
/* on fait un chapeau (iisnan) à la fonction C isnan  pour éviter le
 * conflit avec la fonction intrinsèque (logique) isnan de fortran 95 */
INTEGER DEFP(IISNAN, iisnan, DOUBLE *x) {
    if ( isnan(*x) ) return (INTEGER)1;
    return (INTEGER)0;
}

/* -------------------------------------- VALEUR MAXIMALE REELLE R4*/
DOUBLE DEF0(R4MAEM,r4maem) { return R4MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R4*/
DOUBLE DEF0(R4MIEM,r4miem) { return R4MIN; }

/* -----------------------------------------  PRECISION RELATIVE  R4*/
DOUBLE DEF0(R4PREM,r4prem) { return R4PREC; }
