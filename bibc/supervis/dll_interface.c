/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_interface supervis  DATE 24/08/2010   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2010  EDF R&D              WWW.CODE-ASTER.ORG */
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
#include <dlfcn.h>

#include <Python.h>

#include "aster.h"
#include "aster_fort.h"

#include "dll_register.h"

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

void STDCALL(DLLCLS, dllcls)()
{
    /* Unload all components
     */
    dll_init();
    libsymb_apply_on_all(DLL_DICT, (void*)dlclose, 1);
    Py_DECREF(DLL_DICT);
    DLL_DICT = NULL;
}


/* *********************************************************************
 * 
 *                          ZMAT interface
 * 
 * *********************************************************************/
#define LIB_Zmatbase "libZmat_base.so"
#define SYMB_Zmatbase ""
#define LIB_Zmat "libzAster.so"
#define SYMB_Zmat "zaster_"

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
    printf("searching symbol '%s'... ", SYMB_Zmat);
    dlerror();    /* Clear any existing error */

    *(void **) (&f_zaster) = dlsym(zmat_handle, SYMB_Zmat);

    if ((error = dlerror()) != NULL)  {
        printf("%s\n", error);
        CALL_U2MESS("F", "FERMETUR_4");
    }
    printf("found\n");
    
    /* register ZMAT libs */
    if ( libsymb_register(DLL_DICT, LIB_Zmatbase, SYMB_Zmatbase,
                            zmatbase_handle, (void*)NULL_FUNCTION) ) {
        printf("Registering '%s' and '%s' failed!\n", LIB_Zmatbase, SYMB_Zmatbase);
    }
    if ( libsymb_register(DLL_DICT, LIB_Zmat, SYMB_Zmat,
                            zmat_handle, (void*)f_zaster) ) {
        printf("Registering '%s' and '%s' failed!\n", LIB_Zmat, SYMB_Zmat);
    }
}


void STDCALL(ZASWRP, zaswrp) (
    INTEGER* iel, INTEGER* modele, INTEGER* nvar, INTEGER* ndef, INTEGER* nunit, DOUBLE* instam, DOUBLE* instap,
    DOUBLE* nvarcm, DOUBLE* nomvar, DOUBLE* varplu, DOUBLE* varmoi, DOUBLE* varref,
    DOUBLE* epsm, DOUBLE* deps, DOUBLE* sigm, DOUBLE* vim, INTEGER* nopt, DOUBLE* angmas, DOUBLE* sigp, DOUBLE* vip,
    DOUBLE* dsidep, INTEGER* codret )
{
    /* ZASter WRaPper : wrapper to Zaster C++ function through the function pointer
     * Load the library if necessary (at the first call).
    */
    void (*f_zaster) ZMAT_ARGUMENTS = NULL;
    
    dll_init();
    if ( ! libsymb_is_known(DLL_DICT, LIB_Zmat, SYMB_Zmat) ) {
        load_zmat_lib();
    }
    
    f_zaster = libsymb_get_symbol(DLL_DICT, LIB_Zmat, SYMB_Zmat);

    (*f_zaster)(iel, modele, nvar, ndef, nunit, instam, instap,
                nvarcm, nomvar, varplu, varmoi, varref,
                epsm, deps, sigm, vim, nopt, angmas, sigp, vip,
                dsidep, codret );
}


/* *********************************************************************
 * 
 *                          UMAT interface
 * 
 * *********************************************************************/

/* declarations of pointers on UMAT functions
 * can not be defined because of DEFUMAT */

void load_umat_lib(const char* libname, const char* symbol)
{
    /* load UMAT library and initialize pointers to UMAT functions
     */
    void *umat_handle;
    char *error;
    char symbol_[18], *valk, *val_i;
    INTEGER ibid=0, n0=0, nk=0;
    DOUBLE rbid=0.;
    void DEFUMAT(*f_umat,
        DOUBLE* stress, DOUBLE* statev, DOUBLE* ddsdde, DOUBLE* sse, DOUBLE* spd, DOUBLE* scd,
        DOUBLE* rpl, DOUBLE* ddsddt, DOUBLE* drplde, DOUBLE* drpldt,
        DOUBLE* stran, DOUBLE* dstran, DOUBLE* time, DOUBLE* dtime, DOUBLE* temp, DOUBLE* dtemp,
            DOUBLE* predef, DOUBLE* dpred, char* cmname, STRING_SIZE lcmname, 
        INTEGER* ndi, INTEGER* nshr, INTEGER* ntens, INTEGER* nstatv, DOUBLE* props, INTEGER* nprops,
            DOUBLE* coords, DOUBLE* drot, DOUBLE* pnewdt, 
        DOUBLE* celent, DOUBLE* dfgrd0, DOUBLE* dfgrd1, INTEGER* noel, INTEGER* npt, INTEGER* layer,
            INTEGER* kspt, INTEGER* kstep, INTEGER* kinc
     ) = NULL;
    
    strcpy(symbol_, symbol);

    printf("Loading '%s'... ", libname);
    umat_handle = dlopen(libname, RTLD_NOW);
    if ( ! umat_handle ) {
        printf("\n%s\n", dlerror());
        nk = 1;
        valk = (char *)malloc(nk*VALK_SIZE*sizeof(char));
        STRING_FCPY(valk, VALK_SIZE, libname, strlen(libname));
        CALL_U2MESG("F", "FERMETUR_13", &nk, valk, &n0, &ibid, &n0, &rbid);
        free(valk);  // uncallable
    }
    printf("searching symbol '%s'... ", symbol);
    dlerror();    /* Clear any existing error */

    *(void **) (&f_umat) = dlsym(umat_handle, symbol);
    if ((error = dlerror()) != NULL)  {
        dlerror();
        strcat(symbol_, "_");
        printf("trying symbol '%s'... ", symbol_);
        *(void **) (&f_umat) = dlsym(umat_handle, symbol_);
    }

    if ((error = dlerror()) != NULL)  {
        printf("\n%s\n", error);
        nk = 2;
        valk = (char *)malloc(nk*VALK_SIZE*sizeof(char));
        STRING_FCPY(valk, VALK_SIZE, libname, strlen(libname));
        val_i = &valk[VALK_SIZE];
        STRING_FCPY(val_i, VALK_SIZE, symbol, strlen(symbol));
        CALL_U2MESG("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid);
        free(valk);  // uncallable
    }
    printf("found\n");

    /* register these UMAT lib */
    if ( libsymb_register(DLL_DICT, libname, symbol,
                            umat_handle, (void*)f_umat) ) {
        printf("Registering of '%s' and '%s' failed!\n", libname, symbol);
    }
}


void DEFUMATWRAP(UMATWP, umatwp,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    DOUBLE* stress, DOUBLE* statev, DOUBLE* ddsdde, DOUBLE* sse, DOUBLE* spd, DOUBLE* scd,
    DOUBLE* rpl, DOUBLE* ddsddt, DOUBLE* drplde, DOUBLE* drpldt,
    DOUBLE* stran, DOUBLE* dstran, DOUBLE* time, DOUBLE* dtime, DOUBLE* temp, DOUBLE* dtemp,
        DOUBLE* predef, DOUBLE* dpred, char* cmname, STRING_SIZE lcmname, 
    INTEGER* ndi, INTEGER* nshr, INTEGER* ntens, INTEGER* nstatv, DOUBLE* props, INTEGER* nprops,
        DOUBLE* coords, DOUBLE* drot, DOUBLE* pnewdt, 
    DOUBLE* celent, DOUBLE* dfgrd0, DOUBLE* dfgrd1, INTEGER* noel, INTEGER* npt, INTEGER* layer,
        INTEGER* kspt, INTEGER* kstep, INTEGER* kinc )
{
    /* UMAT WraPper : wrapper to the UMAT function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char libname[129], symbol[17];
    int lon1, lon2;
    void DEFUMAT(*f_umat,
        DOUBLE* stress, DOUBLE* statev, DOUBLE* ddsdde, DOUBLE* sse, DOUBLE* spd, DOUBLE* scd,
        DOUBLE* rpl, DOUBLE* ddsddt, DOUBLE* drplde, DOUBLE* drpldt,
        DOUBLE* stran, DOUBLE* dstran, DOUBLE* time, DOUBLE* dtime, DOUBLE* temp, DOUBLE* dtemp,
            DOUBLE* predef, DOUBLE* dpred, char* cmname, STRING_SIZE lcmname, 
        INTEGER* ndi, INTEGER* nshr, INTEGER* ntens, INTEGER* nstatv, DOUBLE* props, INTEGER* nprops,
            DOUBLE* coords, DOUBLE* drot, DOUBLE* pnewdt, 
        DOUBLE* celent, DOUBLE* dfgrd0, DOUBLE* dfgrd1, INTEGER* noel, INTEGER* npt, INTEGER* layer,
            INTEGER* kspt, INTEGER* kstep, INTEGER* kinc
     ) = NULL;
    
    dll_init();
    lon1 = (int)lnomlib;
    lon2 = (int)lnomsub;
    while (nomlib[lon1-1] == ' ')  lon1--;
    while (nomsub[lon2-1] == ' ')  lon2--;
    STRING_FCPY(libname, 128, nomlib, lon1);
    libname[lon1] = '\0';
    STRING_FCPY(symbol, 16, nomsub, lon2);
    symbol[lon2] = '\0';
        DBGVV(" libname = >%s<, len = %d\n", libname, lon1)
        DBGVV("  symbol = >%s<, len = %d\n", symbol, lon2)
    
    if ( ! libsymb_is_known(DLL_DICT, libname, symbol) ) {
        load_umat_lib(libname, symbol);
    }
    f_umat = libsymb_get_symbol(DLL_DICT, libname, symbol);

    CALLUMAT(*f_umat,
        stress, statev, ddsdde, sse, spd, scd, rpl, ddsddt, drplde, drpldt,
        stran, dstran, time, dtime, temp, dtemp, predef, dpred, cmname, 
        ndi, nshr, ntens, nstatv, props, nprops, coords, drot, pnewdt, 
        celent, dfgrd0, dfgrd1, noel, npt, layer, kspt, kstep, kinc );
}


