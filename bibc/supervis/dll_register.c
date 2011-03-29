/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_register supervis  DATE 29/03/2011   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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

#include <stdio.h>
#include <stdlib.h>
#include <Python.h>

/* *********************************************************************
 * 
 *  Define functions to store in a dictionnary the pointers on libraries
 *  and on symbols.
 *      dll_dict = {
 *          (libname, symbname ) : (handle_on_lib, function_pointer)
 *      }
 * 
 * *********************************************************************/

#include "aster.h"
#include "dll_register.h"


/*
 *   PUBLIC FUNCTIONS
 * 
 */
int libsymb_register(PyObject* dict, const char* libname, const char* symbname,
                     void* handle, void (*symbol)() )
{
    /* Register the handle on the library and the pointer to the function.
     */
    PyObject *key, *value;
    int iret;
    
    key = _libsymb_to_key(libname, symbname);
    value = PyTuple_New( 2 );
    iret = PyTuple_SetItem( value, 0, PyLong_FromVoidPtr(handle) );
    if ( iret == 0 ) {
        iret = PyTuple_SetItem( value, 1, PyLong_FromVoidPtr((void*)symbol) );
        if ( iret == 0 ) {
            iret = PyDict_SetItem( dict, key, value );
        }
    }
    if ( iret == 0 ) {
                PYDBG("register key : ", key)
                PYDBG("     & value : ", value)
                PYDBG("new dict = ", dict)
    } else {
                PYDBG("error during registering : ", key)
    }
    Py_XDECREF(key);
    Py_XDECREF(value);
    return iret;
}

int libsymb_release(PyObject* dict, const char* libname, const char* symbname)
{
    /* Forget the couple (lib, symbol).
     */
    PyObject *key;
    int iret;
    
    key = _libsymb_to_key(libname, symbname);
                PYDBG("release ", key)
    iret = PyDict_DelItem( dict, key );
    Py_XDECREF(key);
    return iret;
}

int libsymb_is_known(PyObject* dict, const char* libname, const char* symbname)
{
    /* Return 1 if the couple (lib, symbol) has been registered, else 0.
     */
    PyObject *key;
    int bool = 0;
    
    key = _libsymb_to_key(libname, symbname);
                PYDBG("is_known key = ", key)
                //PYDBG("     in dict = ", dict)
    bool = PyDict_Contains( dict, key );
                DBGV(" returns %d\n", bool)
    Py_XDECREF(key);
    return bool;
}

void* libsymb_get_handle(PyObject* dict, const char* libname, const char* symbname)
{
    /* Return the handle on the library
     */
    return _libsymb_get_object(dict, libname, symbname, 0);
}

void* libsymb_get_symbol(PyObject* dict, const char* libname, const char* symbname)
{
    /* Return the pointer to the function.
     */
    return _libsymb_get_object(dict, libname, symbname, 1);
}

void libsymb_apply_on_all(PyObject* dict, void (*function)(void *handle), int release)
{
    /* Apply 'function' on the handle of all registered library (typically dlclose)
     * and release them if 'release' is 1.
     */
    PyObject *key, *value, *ihand;
    void *pt;
    Py_ssize_t pos = 0;

    if ( ! dict ) return;
                PYDBG("dict = ", dict)
    while ( PyDict_Next(dict, &pos, &key, &value) ) {
                PYDBG("callback function called for key : ", key)
        ihand = PyTuple_GetItem(value, 0);
        pt = PyLong_AsVoidPtr(ihand);
        (*function)(pt);
    }
    if ( release == 1 ) {
        PyDict_Clear(dict);
    }
}

void NULL_FUNCTION() {}

/*
 *   PRIVATE FUNCTIONS - UTILITIES
 * 
 */
PyObject* _libsymb_to_key(const char* libname, const char* symbname)
{
    /* Helper function to return a key from the couple (libname, symbname).
     * Return value: New reference.
     */
    PyObject *key;
    key = PyTuple_New( 2 );
    PyTuple_SetItem( key, 0, PyString_FromString(libname) );
    PyTuple_SetItem( key, 1, PyString_FromString(symbname) );
    return key;
}

void* _libsymb_get_object(PyObject* dict, const char* libname, const char* symbname, int index)
{
    /* Return the handle on the library or the pointer on the symbol
     */
    PyObject *key, *value, *ihand;
    void *pt = NULL;
    
    key = _libsymb_to_key(libname, symbname);
    value = PyDict_GetItem(dict, key);
    if ( value != NULL ) {
        ihand = PyTuple_GetItem(value, index);
        pt = PyLong_AsVoidPtr(ihand);
    }
    Py_XDECREF(key);
    return pt;
}


/*
 *   UNITTEST
 * 
 */

/*
int main()
{
    static PyObject* dll_dict = NULL;
    int iret;
    void *handle_1, *handle_2, *res;
    //void (*pfunc)( int* x, int* y, double* z);
    int a=1, b=2;
    int *vres;
    int (*pfres)( int* x );

    int functest(int *var) { return *var + 3; }
    void callback(void *pt) { printf("run callback with pt=%ld\n", (long)pt); }
    
    Py_Initialize();
    handle_1 = (void*)(&a);
    handle_2 = (void*)(&b);
    
    dll_dict = PyDict_New();
    iret = libsymb_register(dll_dict, "libZmat_base", "", handle_1, (void*)NULL_FUNCTION );
                DR_ASSERT( iret == 0 )
                DR_ASSERT( libsymb_is_known(dll_dict, "libZmat_base", "") )
                DR_ASSERT( ! libsymb_is_known(dll_dict, "libZmat_base", "zaster") )
    iret = libsymb_register(dll_dict, "libzAster", "zaster", handle_2, (void*)functest );
                DR_ASSERT( iret == 0 )
                DR_ASSERT( libsymb_is_known(dll_dict, "libzAster", "zaster") )
    
    res = libsymb_get_handle(dll_dict, "libZmat_base", "");
                DR_ASSERT( res == handle_1 )
    res = libsymb_get_handle(dll_dict, "libzAster", "unknown");
                DR_ASSERT( ! res )
    res = libsymb_get_handle(dll_dict, "libzAster", "zaster");
    vres = (int*)res;
                DR_ASSERT( res == handle_2 )
                DR_ASSERT( *vres == 2 )
    pfres = libsymb_get_symbol(dll_dict, "libzAster", "zaster");
                DR_ASSERT( (void*)pfres == (void*)functest )
                DR_ASSERT( (*pfres)(vres) == 5 )
    
    iret = libsymb_release(dll_dict, "libZmat_base", "");
                DR_ASSERT( ! libsymb_is_known(dll_dict, "libZmat_base", "") )
                DR_ASSERT( iret == 0 )
    
    libsymb_apply_on_all(dll_dict, (void*)callback, 1);
    
    Py_Finalize();
    return 0;
}
*/
