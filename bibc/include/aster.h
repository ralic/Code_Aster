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

#ifndef ASTER_H
#define ASTER_H

#include "Python.h"
#include <stdio.h>

#include "asterc_debug.h"
#include "aster_depend.h"
#include "definition.h"


#ifndef min
#define min(A,B)  ((A) < (B) ? (A) : (B))
#endif

/* pour indiquer le statut des arguments des fonctions. */

#define _IN
#define _OUT
#define _INOUT
#define _UNUSED

#if (PY_VERSION_HEX < 0x02050000)
typedef int Py_ssize_t;
#endif

/* AS_ASSERT is similar to the ASSERT macro used in fortran */
#define AS_ASSERT(cond) if ( !(cond) ) { \
            DEBUG_LOC; DBGV("Assertion failed: %s", #cond); \
            INTERRUPT(17); }

/* deprecated functions on Windows */
#ifdef _WINDOWS
#define strdup _strdup
#endif

#endif
