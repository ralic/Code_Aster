/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_interface supervis  DATE 31/01/2011   AUTEUR COURTOIS M.COURTOIS */
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

#include "aster.h"
#include "aster_fort.h"
#include "definition_pt.h"

#include "dll_register.h"

#ifdef _POSIX
#include <dlfcn.h>
#endif

/* *********************************************************************
 * 
 * Utilities to Load Dynamically (optionnal) external Libraries
 * 
 * Supported components : ZMAT
 *                        UMAT
 * 
 * *********************************************************************/

/* Global dictionnary used to register (libraries, symbol) couples */
static PyObject* DLL_DICT = NULL;

void dll_init()
{
    /* Initialization */
    if ( ! DLL_DICT ) {
        DLL_DICT = PyDict_New();
    }
}

PyObject* get_dll_register_dict()
{
    /* Return the register dictionnary.
     * For external modules. */
    dll_init();
    return DLL_DICT;
}

void STDCALL(DLLCLS, dllcls)()
{
#ifdef _POSIX
    /* Unload all components
     */
    dll_init();
    libsymb_apply_on_all(DLL_DICT, (FUNC_PTR)dlclose, 1);
    Py_DECREF(DLL_DICT);
    DLL_DICT = NULL;
#endif
}

