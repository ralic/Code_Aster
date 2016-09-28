/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2015  EDF R&D              WWW.CODE-ASTER.ORG */
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
static ASTERDOUBLE R8MAX    = DBL_MAX ;
static ASTERDOUBLE R8MIN    = DBL_MIN ;
static ASTERDOUBLE R8PREC   = DBL_EPSILON ;
static float  R4MAX    = FLT_MAX ;
static float  R4MIN    = FLT_MIN ;
static float  R4PREC   = FLT_EPSILON ;

/* taille max d'une base */
static ASTERINTEGER   ISMFIC   = 50331648;
/* taille max d'un fichier "extend" */
#ifdef _USE_64_BITS
static ASTERINTEGER   ISLFIC   = 12582912;
#else
static ASTERINTEGER   ISLFIC   = 2000*1024;
#endif

#define  R8_PI   3.1415926535897932384626433832
#define  R8_T0   273.15
#define  R8GAME (sqrt(R8MAX*((ASTERDOUBLE)1.-R8PREC)))


/* ---------------------- fonctions renvoyant un  LOGICAL (int)  */
/* int = logique : 1=vrai / 0=faux                               */


/* ----------------------------- fonctions renvoyant un  ASTERINTEGER */
/* -------------------------------------------- LONGUEUR EN BITS */
ASTERINTEGER DEF0(LBISEM,lbisem) { return 8 * ASTER_INT_SIZE; }

/* ------------------------------------------ LONGUEUR EN OCTETS */
ASTERINTEGER DEF0(LOLSEM,lolsem) { return ASTER_LOGICAL_SIZE; }
ASTERINTEGER DEF0(LOISEM,loisem) { return ASTER_INT_SIZE; }
ASTERINTEGER DEF0(LOR8EM,lor8em) { return ASTER_REAL8_SIZE; }
ASTERINTEGER DEF0(LOC8EM,loc8em) { return ASTER_COMPLEX_SIZE; }

/* --------------- NOT.A.NUMBER (IEEE) OU UNDEF (CRAY NON IEEE) */
/* rq : ne veut rien dire pour un entier...mais utilise...      */
ASTERINTEGER DEF0(ISNNEM,isnnem) { return (ASTERINTEGER)ISUND; }

/* --------------------------- NOMBRE DE CHIFFRES SIGNIFICATIFS */
ASTERINTEGER DEF0(NCISEM,ncisem) { return INTEGER_NB_CHIFFRES_SIGNIFICATIFS; }
ASTERINTEGER DEF0(NCR8EM,ncr8em) { return REAL_NB_CHIFFRES_SIGNIFICATIFS; }

/* ------------------------------------- VALEUR ENTIERE MAXIMALE */
ASTERINTEGER DEF0(ISMAEM,ismaem) { return (ASTERINTEGER)ISMAX; }

/* ------------------------------------------  TAILLE DE FICHIER
 exprimee en kilo (1024) */
ASTERINTEGER DEF0(LOFIEM,lofiem) {
   return (ASTERINTEGER)ISLFIC;
}

/* ----------------------------------  TAILLE MAXIMUM DE FICHIER */
ASTERINTEGER DEF0(MOFIEM,mofiem) { return (ASTERINTEGER)ISMFIC; }

/* ---------------------------------------------  POIDS DES BITS */
ASTERINTEGER DEFP(ISPBEM, ispbem, ASTERINTEGER *jb) { return (ASTERINTEGER)pow(2.,(*jb-1)); }

/* ----------------------------------------  Base de numeration B */
ASTERINTEGER DEF0(ISBAEM,isbaem) { return 2; }

/* ---------------------- fonctions renvoyant un REAL*8 (ASTERDOUBLE) */
/* --------------------- Plus petit increment relatif B**-T */
ASTERDOUBLE DEF0(RMIREM,rmirem) { return pow(2,-53); }

/* -------------------- Plus grand increment relatif B**(1-T) */
/* cette valeur est normalment identique a R8PREM             */
ASTERDOUBLE DEF0(RMAREM,rmarem) { return pow(2,-52); }

/* --------------------------- Plus petite valeur B**(EMIN-1) */
/* cette valeur est normalment identique a R8MIEM             */
ASTERDOUBLE DEF0(RMINEM,rminem) { return pow(2,-1022); }

/* --------------- Plus grande valeur B**(EMAX-1) * (1-B**-T) */
/* cette valeur est normalment identique a R8MAEM             */
ASTERDOUBLE DEF0(RMAXEM,rmaxem) { return pow(2,1023)*(1.-pow(2,-53)); }

/* ---------------------REEL NOT.A.NUMBER (IEEE) OU UNDEF (CRAY) */
ASTERDOUBLE DEF0(R8NNEM,r8nnem) { return *(ASTERDOUBLE*)R8UND; }

/* -------------------------------------- VALEUR MAXIMALE REELLE R8*/
ASTERDOUBLE DEF0(R8MAEM,r8maem) { return R8MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R8*/
ASTERDOUBLE DEF0(R8MIEM,r8miem) { return R8MIN; }

/* ----------------------------  REEL A BOUCHER LES CASES (R8MAX)*/
ASTERDOUBLE DEF0(R8VIDE,r8vide) { return R8MAX; }

/* -----------------------------------------  BASE DE NUMERATION */
ASTERDOUBLE DEF0(R8BAEM,r8baem) { return (ASTERDOUBLE)2.; }

/* -----------------------------------------  PRECISION RELATIVE  R8*/
ASTERDOUBLE DEF0(R8PREM,r8prem) { return R8PREC; }


/* ----------------------------------  GAMME D"UTILISATION RELLE */
ASTERDOUBLE DEF0(R8GAEM,r8gaem) { return (ASTERDOUBLE)R8GAME; }


/* ----------- fonctions renvoyant des valeurs reelles diverses */
/* ------------------------------------------ VALXEM ZERO ABSOLU*/
ASTERDOUBLE DEF0(R8T0,r8t0) { return (ASTERDOUBLE)R8_T0; }

/* --------------------------------------------------- VALXEM PI*/
ASTERDOUBLE DEF0(R8PI,r8pi) { return (ASTERDOUBLE)R8_PI; }

/* -------------------------------------------------- VALXEM 2PI*/
ASTERDOUBLE DEF0(R8DEPI,r8depi) { return (ASTERDOUBLE)((ASTERDOUBLE)2.*(ASTERDOUBLE)R8_PI); }

/* ------------------------------------------------- VALXEM DGRD*/
ASTERDOUBLE DEF0(R8DGRD,r8dgrd) { return (ASTERDOUBLE)((ASTERDOUBLE)R8_PI/(ASTERDOUBLE)180.); }

/* ------------------------------------------------- VALXEM RDDG*/
ASTERDOUBLE DEF0(R8RDDG,r8rddg) { return (ASTERDOUBLE)((ASTERDOUBLE)180./(ASTERDOUBLE)R8_PI); }

/* ------------------------------------ LONGUEUR de BLOC pour MULT_FRONT */
ASTERINTEGER DEF0(LLBLOC,llbloc) { return OPT_TAILLE_BLOC_MULT_FRONT; }

/* -------------------------------------- VALEUR MAXIMALE REELLE R4*/
ASTERDOUBLE DEF0(R4MAEM,r4maem) { return R4MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE R4*/
ASTERDOUBLE DEF0(R4MIEM,r4miem) { return R4MIN; }
