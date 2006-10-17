/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF ENVIMA UTILITAI  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS */
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
#include "aster.h"



/* Définition des constantes */

/* undef entier et réel */
#ifdef _USE_64_BITS
static long   ISUND    = 0x7FFFFFFFFFFFFFFF ;
static int    R8UND[2] = { 0x00000000 , 0x7ff80000 };
#else
static long   ISUND    = LONG_MAX ;
static long   R8UND[2] = { 0x00000000 , 0x7ff80000 };
#endif


/* entier max, réel max, réel min, précision en réel */
static long   ISMAX    = LONG_MAX ;
static double R8MAX    = DBL_MAX ;
static double R8MIN    = DBL_MIN ;
static double R8PREC   = DBL_EPSILON ;

/* taille max d'une base */
static long   ISMFIC   = 12582912;


#define  R8_PI   3.1415926535897932384626433832
#define  R8_T0   273.15
#define  R8GAME (sqrt(R8MAX*((double)1.-R8PREC)))


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

/* ---------------------------------  LONGUEUR UNITE D'ADRESSAGE */
/* reste du Cray : 8 de longueur 1 */
INTEGER STDCALL(LOUAEM,louaem)() { return 1; }

/* ------------------------------------------  TAILLE DE FICHIER
 exprimee en kilo (1024) */
INTEGER STDCALL(LOFIEM,lofiem)() {
#ifdef _USE_64_BITS
   return (INTEGER)ISMFIC;
#else
   return (INTEGER)2000*1024;
#endif
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

/* ---------------------- fonctions renvoyant un REAL*8 (double) */
/* --------------------- Plus petit increment relatif B**-T */
double STDCALL(RMIREM,rmirem)() { return pow(2,-53); }

/* -------------------- Plus grand increment relatif B**(1-T) */
/* cette valeur est normalment identique a R8PREM             */
double STDCALL(RMAREM,rmarem)() { return pow(2,-52); }

/* --------------------------- Plus petite valeur B**(EMIN-1) */
/* cette valeur est normalment identique a R8MIEM             */
double STDCALL(RMINEM,rminem)() { return pow(2,-1022); }

/* --------------- Plus grande valeur B**(EMAX-1) * (1-B**-T) */
/* cette valeur est normalment identique a R8MAEM             */
double STDCALL(RMAXEM,rmaxem)() { return pow(2,1023)*(1.-pow(2,-53)); }

/* ---------------------REEL NOT.A.NUMBER (IEEE) OU UNDEF (CRAY) */
double STDCALL(R8NNEM,r8nnem)() { return *(double*)R8UND; }

/* -------------------------------------- VALEUR MAXIMALE REELLE */
double STDCALL(R8MAEM,r8maem)() { return R8MAX; }

/* -------------------------------------- VALEUR MINIMALE REELLE */
double STDCALL(R8MIEM,r8miem)() { return R8MIN; }

/* ----------------------------  REEL A BOUCHER LES CASES (R8MAX)*/
double STDCALL(R8VIDE,r8vide)() { return R8MAX; }

/* -----------------------------------------  BASE DE NUMERATION */
double STDCALL(R8BAEM,r8baem)() { return (double)2.; }

/* -----------------------------------------  PRECISION RELATIVE */
double STDCALL(R8PREM,r8prem)() { return R8PREC; }


/* ----------------------------------  GAMME D"UTILISATION RELLE */
double STDCALL(R8GAEM,r8gaem)() { return (double)R8GAME; }


/* ----------- fonctions renvoyant des valeurs reelles diverses */
/* ------------------------------------------ VALXEM ZERO ABSOLU*/
double STDCALL(R8T0,r8t0)() { return (double)R8_T0; }

/* --------------------------------------------------- VALXEM PI*/
double STDCALL(R8PI,r8pi)() { return (double)R8_PI; }

/* -------------------------------------------------- VALXEM 2PI*/
double STDCALL(R8DEPI,r8depi)() { return (double)((double)2.*(double)R8_PI); }

/* ------------------------------------------------- VALXEM DGRD*/
double STDCALL(R8DGRD,r8dgrd)() { return (double)((double)R8_PI/(double)180.); }

/* ------------------------------------------------- VALXEM RDDG*/
double STDCALL(R8RDDG,r8rddg)() { return (double)((double)180./(double)R8_PI); }

/* ------------------------------------ LONGUEUR de BLOC pour MULT_FRONT */
INTEGER STDCALL(LLBLOC,llbloc)() { return OPT_TAILLE_BLOC_MULT_FRONT; }

/* ----------------------------------------  Pour tester un NaN */
/* on fait un chapeau (iisnan) à la fonction C isnan  pour éviter le conflit avec la fonction intrinsèque (logique) isnan de fortran 95 */
INTEGER DEFP(IISNAN, iisnan, double *x) { return isnan(*x); }
