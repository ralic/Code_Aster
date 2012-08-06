/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_exceptions include  DATE 06/08/2012   AUTEUR COURTOIS M.COURTOIS */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* RESPONSABLE COURTOIS M.COURTOIS */

#ifndef ASTER_EXCEPTIONS_H
#define ASTER_EXCEPTIONS_H

#include <setjmp.h>
#include "aster.h"

#define FatalError 18
#define EOFError   19

#define NIVMAX     10

#define try                 _exc_lvl = _new_try(); if((gExcNumb = setjmp(gExcEnv[_exc_lvl])) == 0)
#define interruptTry(val)   if(_exc_lvl > 0) {longjmp(gExcEnv[_exc_lvl], val);} else {raiseException();}
#define except(val)         else if (gExcNumb == val)
#define finally             else
#define endTry()            _end_try()
#define raiseException()    _raiseException(gExcNumb)

/* hidden variable, store the current level */
static int _exc_lvl=0;

/*
 *   PUBLIC FUNCTIONS
 *
 */
extern int gExcNumb;
extern jmp_buf gExcEnv[NIVMAX+1];
extern PyObject* gExcArgs;

extern void initExceptions(PyObject *dict);

/*
 *   PRIVATE/HIDDEN FUNCTIONS
 *
 */
extern int _new_try();
extern void _end_try();
void _raiseException( _IN int val );

/* FIN ASTER_EXCEPTIONS_H */
#endif
