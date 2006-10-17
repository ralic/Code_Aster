/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_depend include  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2006  EDF R&D              WWW.CODE-ASTER.ORG */
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

#ifndef ASTER_DEPEND_H
#define ASTER_DEPEND_H

/* --------------------------------------------
   --          DEBUT aster_depend.h          --
   --------------------------------------------

Dans les routines C, ne pas utiliser les noms de plates-formes,
se ramener à _POSIX/_WIN32/_USE_64_BITS.

Pour les cas (rares) d'adhérence système (constantes, signaux),
on se limite à SOLARIS, LINUX, IRIX (cas particuliers à
l'intérieur de _POSIX).

Compatibilité ascendantes :
 HPUX       => _POSIX, _NO_UNDERSCORE
 IRIX       => _POSIX
 IRIX64     => _POSIX, IRIX, _USE_64_BITS
 P_LINUX    => _POSIX, LINUX
 LINUX64    => _POSIX, LINUX, _USE_64_BITS
 TRU64      => _POSIX, _USE_64_BITS
 SOLARIS    => _POSIX
 SOLARIS64  => _POSIX, SOLARIS, _USE_64_BITS
 PPRO_NT    => _WIN32

*/

#ifndef _POSIX
#define _POSIX
#elif defined WIN32 || PPRO_NT
#define _WIN32
#endif

#ifdef HPUX
#define _NO_UNDERSCORE
#endif

#if defined P_LINUX || LINUX64
#define LINUX
#endif

#if defined IRIX64
#define IRIX
#endif

#if defined SOLARIS64
#define SOLARIS
#endif

/* plates-formes 32/64 bits */

#if defined _USE_64_BITS || LINUX64 || TRU64 || SOLARIS64 || IRIX64
/* pour compatibilité si on arrive avec LINUX64 */
#define _USE_64_BITS
#define INTEGER long
#define LONG_INTEGER_BITS 64
#define LONG_INTEGER_MOTS 8
#define LONG_REAL_MOTS 8
#define LONG_COMPLEX_MOTS 16
#define OFF_INIT  8
#define INTEGER_NB_CHIFFRES_SIGNIFICATIFS 19
#define REAL_NB_CHIFFRES_SIGNIFICATIFS    16

#else
#define INTEGER long
#define LONG_INTEGER_BITS 32
#define LONG_INTEGER_MOTS 4
#define LONG_REAL_MOTS 8
#define LONG_COMPLEX_MOTS 16
#define OFF_INIT  4
#define INTEGER_NB_CHIFFRES_SIGNIFICATIFS  9
#define REAL_NB_CHIFFRES_SIGNIFICATIFS    16

#endif

/* Utilisation de getrlimit :
   _IGNORE_RLIMIT permet de ne pas utiliser getrlimit */
#ifndef _IGNORE_RLIMIT
#define _USE_RLIMIT
#endif

/* flags d'optimisation */
/* taille de bloc dans MULT_FRONT */
#ifdef _USE_64_BITS
#define __OPT_TAILLE_BLOC_MULT_FRONT__ 96
#else
#define __OPT_TAILLE_BLOC_MULT_FRONT__ 32
#endif

#ifndef OPT_TAILLE_BLOC_MULT_FRONT
#define OPT_TAILLE_BLOC_MULT_FRONT __OPT_TAILLE_BLOC_MULT_FRONT__
#endif

/* Valeurs par défaut pour les répertoires */
#ifndef REP_MAT
#define REP_MAT "/aster/materiau/"
#endif

#ifndef REP_OUT
#define REP_OUT "/aster/outils/"
#endif

#ifndef REP_DON
#define REP_DON "/aster/donnees/"
#endif


/* --------------------------------------------
   --      TEST DES VALEURS OBLIGATOIRES     --
   -------------------------------------------- */
#if !(defined _POSIX || _WIN32)
#error ERREUR
#endif

/* --------------------------------------------
   --           FIN  aster_depend.h          --
   -------------------------------------------- */
#endif
