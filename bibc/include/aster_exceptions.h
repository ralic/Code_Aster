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
/* person_in_charge: mathieu.courtois at edf.fr */

#ifndef ASTER_EXCEPTIONS_H
#define ASTER_EXCEPTIONS_H

#include <setjmp.h>
#include "aster.h"
#include "asterc_debug.h"
#include "aster_module.h"

#define FatalError 18   /* kept for backward compatibility only */
#define EOFError   19
#define AsterError 21

#define NIVMAX     10

#define try                 _new_try(); DEBUG_EXCEPT("try: level=%d", gExcLvl); \
                            if ((gExcNumb = setjmp(gExcEnv[gExcLvl])) == 0)
#define interruptTry(val)   if(gExcLvl > 0) { \
                                DEBUG_EXCEPT("interruptTry: level=%d", gExcLvl); \
                                longjmp(gExcEnv[gExcLvl], val); } \
                            else { printf("Exception raised out of Code_Aster commands.\n"); \
                                _raiseException(val); }
#define except(val)         else if (gExcNumb == val)
#define exceptAll           else
#define endTry()            _end_try(); DEBUG_EXCEPT("endTry: level=%d", gExcLvl);
#define raiseException()    _end_try(); \
                            DEBUG_EXCEPT("raiseException: level=%d", gExcLvl); \
                            _raiseException(gExcNumb); \
                            return NULL
#define raiseExceptionString(exc, args) \
                            _end_try(); \
                            PyErr_SetString(exc, args); \
                            return NULL

/*
 *   PUBLIC FUNCTIONS
 *
 */
extern int gExcLvl;
extern int gExcNumb;
extern jmp_buf gExcEnv[NIVMAX+1];

extern void initExceptions(PyObject *dict);

/*
 *   PRIVATE/HIDDEN FUNCTIONS
 *
 */
extern void _new_try();
extern void _end_try();
void _raiseException( _IN int val );

/* FIN ASTER_EXCEPTIONS_H */
#endif
