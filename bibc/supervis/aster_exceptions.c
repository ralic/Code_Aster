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

#include "Python.h"
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
 * exceptAll {                                      else {
 *      ...                                                 ...
 * }                                                }
 * endTry();
 * 
 * NB: there are two differences with the Python behavior/syntax.
 *     1. There is no finally clause.
 *     2. An additional statement endTry() to decrement the counter level.
 *        Do not forget endTry() if there is a return statement in a block.
 * 
 * except( code ) : will probably be not very usefull in C.
 * 
 * Global variables:
 *  gExcNumb: code of the exception to raise
 *  gExcEnv : array to store the stack environment
 *  gExcArgs: arguments passed to the exception raised
 * 
 */
int gExcLvl = 0;
int gExcNumb = -1;
jmp_buf gExcEnv[NIVMAX+1];

static PyObject* gExcArgs = NULL;
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
void _new_try()
{
    /* Begin of try : `gExcLvl` incremented
     */
    gExcLvl += 1;
    if ( NIVMAX < gExcLvl ) {
        printf("AssertionError: too many nested try/except statements: %d\n", gExcLvl);
        abort();
    }
}

void _end_try()
{
    /* End if try : `gExcLvl` is decremented
     */
    gExcLvl -= 1;
}

void _raiseException( _IN int val )
{
    /* Raise the exception of code `val`.
     * Called through raiseException by aster_oper, aster_opsexe, aster_debut,
     * aster_poursu.
     * 
     * WARNING: The error indicator will be reset by a PyObject_CallMethod or similar.
     * All C-Python methods must take care of that indicator (ex. UTPRIN).
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

void DEFPSPSPPPP(UEXCEP,uexcep, _IN INTEGER *exc_type,
                                _IN char *idmess, _IN STRING_SIZE lidmess,
                                _IN INTEGER *nbk, _IN char *valk, _IN STRING_SIZE lvk,
                                _IN INTEGER *nbi, _IN INTEGER *vali,
                                _IN INTEGER *nbr, _IN DOUBLE *valr)
{
    /*
     * Fortran/Python interface to raise an exception from the fortran subroutines
     */
    PyObject *tup_valk, *tup_vali, *tup_valr;
    char *kvar;
    int i;
    
    tup_valk = PyTuple_New( *nbk ) ;
    for(i=0;i<*nbk;i++){
       kvar = valk + i*lvk;
       PyTuple_SetItem( tup_valk, i, PyString_FromStringAndSize(kvar,lvk) ) ;
    }

    tup_vali = PyTuple_New( *nbi ) ;
    for(i=0;i<*nbi;i++){
       PyTuple_SetItem( tup_vali, i, PyInt_FromLong(vali[i]) ) ;
    }

    tup_valr = PyTuple_New( *nbr ) ;
    for(i=0;i<*nbr;i++){
       PyTuple_SetItem( tup_valr, i, PyFloat_FromDouble(valr[i]) ) ;
    }

    gExcArgs = PyTuple_New( 4 );
    PyTuple_SetItem( gExcArgs, (Py_ssize_t)0, PyString_FromStringAndSize(idmess, lidmess) );
    PyTuple_SetItem( gExcArgs, (Py_ssize_t)1, tup_valk );
    PyTuple_SetItem( gExcArgs, (Py_ssize_t)2, tup_vali );
    PyTuple_SetItem( gExcArgs, (Py_ssize_t)3, tup_valr );

    interruptTry((int)*exc_type);
}
