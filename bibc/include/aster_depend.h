/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG */
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

#ifndef ASTER_DEPEND_H
#define ASTER_DEPEND_H

/* 
 * Exactly one of _POSIX and _WINDOWS must be defined, they are exclusive.
 * 
 * In the source code, only use _POSIX or _WINDOWS and, when required, _USE_64_BITS.
 * 
 * The only platform name used is SOLARIS in the signal features.
 * 
 */

#include "asterc_config.h"

/* test required value */
#if ! defined _POSIX && ! defined _WINDOWS
#   error ERROR _POSIX or _WINDOWS is required
#endif
#if defined _POSIX && defined _WINDOWS
#   error ERROR only one of _POSIX or _WINDOWS, not both
#endif

#if defined LINUX || LINUX64
#   define GNU_LINUX
#endif

#if defined SOLARIS64
#   define SOLARIS
#endif

/* MS Windows platforms */
#if defined _WINDOWS

/* win64 - use LLP64 model */
#   ifdef _USE_64_BITS
#       define _STRLEN_AT_END
#       define _USE_LONG_LONG_INT
#       define ASTER_INT_SIZE       8
#       define ASTER_REAL8_SIZE     8
#       define ASTERC_FORTRAN_INT   long long
#   endif

/* stdcall must be defined explicitly because it does not seem required anywhere */
#   define _STRLEN_AT_END

#else
/* Linux & Unix platforms */
#   define _STRLEN_AT_END

/* end platforms type */
#endif

#ifdef _USE_64_BITS
#   define INTEGER_NB_CHIFFRES_SIGNIFICATIFS 19
#   define REAL_NB_CHIFFRES_SIGNIFICATIFS    16
#else
#   define INTEGER_NB_CHIFFRES_SIGNIFICATIFS  9
#   define REAL_NB_CHIFFRES_SIGNIFICATIFS    16
#endif

#define STRING_SIZE         ASTERC_STRING_SIZE
#define INTEGER4            ASTERC_FORTRAN_INT4
#define INTEGER             ASTERC_FORTRAN_INT
#define DOUBLE              ASTERC_FORTRAN_REAL8
#define REAL4               ASTERC_FORTRAN_REAL4

/* flags d'optimisation */
/* taille de bloc dans MULT_FRONT */
#ifdef _USE_64_BITS
#   define __OPT_TAILLE_BLOC_MULT_FRONT__ 96
#else
#   define __OPT_TAILLE_BLOC_MULT_FRONT__ 32
#endif

#ifndef OPT_TAILLE_BLOC_MULT_FRONT
#   define OPT_TAILLE_BLOC_MULT_FRONT __OPT_TAILLE_BLOC_MULT_FRONT__
#endif

/* Comportement par défaut des FPE dans matfpe pour les blas/lapack */
#ifndef _ENABLE_MATHLIB_FPE
#   ifndef _DISABLE_MATHLIB_FPE
#       define _DISABLE_MATHLIB_FPE
#   endif    
#else
#   undef _DISABLE_MATHLIB_FPE
#endif

/* Valeurs par défaut pour les répertoires */
#ifndef REP_MAT
#   define REP_MAT "/aster/materiau/"
#endif

#ifndef REP_OUT
#   define REP_OUT "/aster/outils/"
#endif

#ifndef REP_DON
#   define REP_DON "/aster/donnees/"
#endif

#endif
