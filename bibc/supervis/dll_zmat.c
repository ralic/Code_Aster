/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_zmat supervis  DATE 19/05/2011   AUTEUR SELLENET N.SELLENET */
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

#include <Python.h>

#include "aster.h"
#include "aster_fort.h"
#include "definition_pt.h"

#include "dll_register.h"

#ifdef _POSIX
#include <dlfcn.h>
PyObject* get_dll_register_dict();


/* *********************************************************************
 * 
 *                          ZMAT interface
 * 
 * *********************************************************************/
#define LIB_Zmatbase "libZmat_base.so"
#define SYMB_Zmatbase ""
#define LIB_Zmat "libzAster.so"
#define SYMB_Zmat "zaster_"
#define SYMB_Zini "zasini_"

/* declaration of pointers to ZMAT functions */
#define ZMAT_ARGUMENTS ( \
    INTEGER* iel, INTEGER* modele, INTEGER* nvar, INTEGER* ndef, INTEGER* nunit, DOUBLE* instam, \
        DOUBLE* instap, \
    DOUBLE* nvarcm, DOUBLE* nomvar, DOUBLE* varplu, DOUBLE* varmoi, DOUBLE* varref, \
    DOUBLE* epsm, DOUBLE* deps, DOUBLE* sigm, DOUBLE* vim, INTEGER* nopt, DOUBLE* angmas, \
        DOUBLE* sigp, DOUBLE* vip, \
    DOUBLE* dsidep, INTEGER* codret \
)


void load_zmat_lib()
{
    /* load ZMAT library and initialize pointers to ZMAT functions
     */
    char *error;
    void *zmat_handle;
    void *zmatbase_handle;
    void (*f_zaster) ZMAT_ARGUMENTS = NULL;
    void (*f_zasini) () = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    printf("Loading libZmat_base.so... ");
    zmatbase_handle = dlopen(LIB_Zmatbase, RTLD_NOW | RTLD_GLOBAL);
    if ( ! zmatbase_handle ) {
        printf("%s\n", dlerror());
        CALL_U2MESS("F", "FERMETUR_4");
    }
    printf("done\n");

    printf("Loading libzAster.so... ");
    zmat_handle = dlopen(LIB_Zmat, RTLD_NOW);
    if ( ! zmat_handle ) {
        printf("%s\n", dlerror());
        CALL_U2MESS("F", "FERMETUR_4");
    }
    dlerror();    /* Clear any existing error */

    /* zaster */
    printf("searching symbol '%s'... ", SYMB_Zmat);
    *(void **) (&f_zaster) = dlsym(zmat_handle, SYMB_Zmat);
    if ((error = dlerror()) != NULL)  {
        printf("%s\n", error);
        CALL_U2MESS("F", "FERMETUR_4");
    }
    
    /* zasini */
    printf("searching symbol '%s'... ", SYMB_Zini);
    *(void **) (&f_zasini) = dlsym(zmat_handle, SYMB_Zini);
    if ((error = dlerror()) != NULL)  {
        printf("%s\n", error);
        CALL_U2MESS("F", "FERMETUR_4");
    }
    printf("found\n");
    
    /* register ZMAT lib and symbols */
    if ( libsymb_register(DLL_DICT, LIB_Zmatbase, SYMB_Zmatbase,
                            zmatbase_handle, (void*)NULL_FUNCTION) ) {
        printf("Registering '%s' and '%s' failed!\n", LIB_Zmatbase, SYMB_Zmatbase);
    }
    if ( libsymb_register(DLL_DICT, LIB_Zmat, SYMB_Zmat,
                            zmat_handle, (void*)f_zaster) ) {
        printf("Registering '%s' and '%s' failed!\n", LIB_Zmat, SYMB_Zmat);
    }
    if ( libsymb_register(DLL_DICT, LIB_Zmat, SYMB_Zini,
                            zmat_handle, (void*)f_zasini) ) {
        printf("Registering '%s' and '%s' failed!\n", LIB_Zmat, SYMB_Zini);
    }
}
#endif


void STDCALL(ZASWRP, zaswrp) (
    INTEGER* iel, INTEGER* modele, INTEGER* nvar, INTEGER* ndef, INTEGER* nunit, DOUBLE* instam, DOUBLE* instap,
    DOUBLE* nvarcm, DOUBLE* nomvar, DOUBLE* varplu, DOUBLE* varmoi, DOUBLE* varref,
    DOUBLE* epsm, DOUBLE* deps, DOUBLE* sigm, DOUBLE* vim, INTEGER* nopt, DOUBLE* angmas, DOUBLE* sigp, DOUBLE* vip,
    DOUBLE* dsidep, INTEGER* codret )
{
#ifdef _POSIX
    /* ZASter WRaPper : wrapper to Zaster C++ function through the function pointer
     * Load the library if necessary (at the first call).
    */
    void (*f_zaster) ZMAT_ARGUMENTS = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    if ( ! libsymb_is_known(DLL_DICT, LIB_Zmat, SYMB_Zmat) ) {
        load_zmat_lib();
    }
    
    f_zaster = libsymb_get_symbol(DLL_DICT, LIB_Zmat, SYMB_Zmat);

    (*f_zaster)(iel, modele, nvar, ndef, nunit, instam, instap,
                nvarcm, nomvar, varplu, varmoi, varref,
                epsm, deps, sigm, vim, nopt, angmas, sigp, vip,
                dsidep, codret );
#endif
}

void STDCALL(ZASWRI,zaswri) ()
{
#ifdef _POSIX
    /* ZASter WRapper Init : wrapper to Zasini C++ function
    */
    void (*f_zasini) () = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    if ( ! libsymb_is_known(DLL_DICT, LIB_Zmat, SYMB_Zini) ) {
        load_zmat_lib();
    }
    
    f_zasini = libsymb_get_symbol(DLL_DICT, LIB_Zmat, SYMB_Zini);

    (*f_zasini)();
#endif
}

