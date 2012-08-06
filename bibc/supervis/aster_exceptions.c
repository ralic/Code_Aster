/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_exceptions supervis  DATE 06/08/2012   AUTEUR COURTOIS M.COURTOIS */
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

#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

#include "aster.h"
#include "aster_exceptions.h"

/*
 * Emulate the behavior of exceptions using the system functions 'setjmp/longjmp'.
 * 
 * The fortran subroutines can raise an exception by calling interruptTry via XFINI
 * or UEXCEP (usually UEXCEP if called through UTEXCP or U2MESS subroutines).
 * XFINI is called at the end of a normal execution and raise the EOFError exception.
 * 
 * try {                                            if ((gExcNumb = setjmp(env)) == 0) { <--
 *      ...                                                 ...                     |
 *      interruptTry( code );                               longjmp(env, code);   ---
 *      ...                                                 ...
 * }                                --->            }
 * except( code ) {                                 else if (gExcEnv == code ) {
 *      ...                                                 ...
 * }                                                }
 * finally {                                        else {
 *      ...                                                 ...
 * }                                                }
 * endTry();
 * 
 * NB: there are two differences with the Python behavior/syntax.
 *     1. If an exception occurs in any of the blocks (and not handled),
 *        it will not be raised at the end of the finally clause. 
 *     2. An additional statement endTry() to decrement the counter level.
 *        Do not forget endTry() if there is a return statement in a block.
 * 
 * Global variables:
 *  gExcNumb: code of the exception to raise
 *  gExcEnv : array to store the stack environment
 *  gExcArgs: arguments passed to the exception raised
 * 
 */
int gExcNumb = -1;
jmp_buf gExcEnv[NIVMAX+1];
PyObject* gExcArgs = NULL;

static int exc_level = 0;
static PyObject *exc_module = NULL;

/*
 *   PUBLIC FUNCTIONS
 * 
 */
void initExceptions(PyObject *dict)
{
    /* The exception of the 'aster' module are defined in Execution/E_Exception.py.
     * They are added to the module through the function 'add_to_dict_module'.
     */
    PyObject *res;

    exc_module = PyImport_ImportModule("Execution.E_Exception");
    if ( ! exc_module ) {
        fprintf(stderr, "\n\nWARNING:\n    ImportError of Execution.E_Exception module!\n");
        fprintf(stderr, "    No exception defined in the aster module.\n");
        fprintf(stderr, "    It may be unusable.\n\n");
        PyErr_Clear();
        Py_XDECREF(exc_module);
        return;
    }

    /* assign the dict using the method add_to_dict_module of E_Exception */
    res = PyObject_CallMethod(exc_module, "add_to_dict_module", "O", dict);
    Py_DECREF(res);
    Py_DECREF(exc_module);
}

/*
 *   PRIVATE FUNCTIONS
 * 
 */
int _new_try()
{
    /* Begin of try : `exc_level` incremented
     */
    exc_level += 1;
    if ( NIVMAX < exc_level ) {
        printf("AssertionError: too many nested try/except statements: %d\n", exc_level);
        abort();
    }
    return exc_level;
}

void _end_try()
{
    /* End if try : `exc_level` is decremented
     */
    exc_level -= 1;
}

void _raiseException( _IN int val )
{
    /* Raise the exception of code `val`
     * called by aster_oper, aster_opsexe, aster_debut, aster_poursu
     */
    PyObject *exc;

    if ( val == EOFError ) {
        PyErr_SetString(PyExc_EOFError, "exit ASTER");
    } else {
        exc = PyObject_CallMethod(exc_module, "get_exception", "i", val);
        PyErr_SetObject(exc, gExcArgs);
    }
    return;
}

