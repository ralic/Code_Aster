/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_umat supervis  DATE 10/10/2012   AUTEUR COURTOIS M.COURTOIS */
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

#include "Python.h"
#include "aster.h"
#include "aster_fort.h"
#include "aster_utils.h"
#include "definition_pt.h"

#include "dll_register.h"

#ifdef _POSIX
#include <dlfcn.h>
PyObject* get_dll_register_dict();


/* *********************************************************************
 * 
 *                          UMAT interface
 * 
 * *********************************************************************/

/* declarations of pointers on UMAT functions */
#define FUNC_UMAT(NAME)  void DEFUMAT(*NAME, \
        DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, \
        DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, \
        DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, \
            DOUBLE*, DOUBLE*, char*, STRING_SIZE,  \
        INTEGER*, INTEGER*, INTEGER*, INTEGER*, DOUBLE*, INTEGER*, \
            DOUBLE*, DOUBLE*, DOUBLE*,  \
        DOUBLE*, DOUBLE* dfgrd0, DOUBLE* dfgrd1, INTEGER*, INTEGER*, INTEGER*, \
            INTEGER*, INTEGER*, INTEGER* )

void load_umat_lib(const char* libname, const char* symbol)
{
    /* load UMAT library and initialize pointers to UMAT functions
     */
    void *umat_handle;
    char *error;
    char symbol_[18], *valk;
    INTEGER ibid=0, n0=0, nk=0;
    DOUBLE rbid=0.;
    FUNC_UMAT(f_umat) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    strcpy(symbol_, symbol);

    printf("Loading '%s'... ", libname);
    umat_handle = dlopen(libname, RTLD_NOW);
    if ( ! umat_handle ) {
        printf("\n%s\n", dlerror());
        nk = 2;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "UMAT", VALK_SIZE);
        SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
        CALL_U2MESG("F", "FERMETUR_13", &nk, valk, &n0, &ibid, &n0, &rbid);
        FreeStr(valk);  // uncallable
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
        nk = 3;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "UMAT", VALK_SIZE);
        SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
        SetTabFStr(valk, 2, (char *)symbol, VALK_SIZE);
        CALL_U2MESG("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid);
        FreeStr(valk);  // uncallable
    }
    printf("found\n");

    /* register these UMAT lib */
    if ( libsymb_register(DLL_DICT, libname, symbol,
                            umat_handle, (FUNC_PTR)f_umat) ) {
        printf("Registering of '%s' and '%s' failed!\n", libname, symbol);
    }
}
#endif


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
#ifdef _POSIX
    /* UMAT WraPper : wrapper to the UMAT function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char *libname, *symbol;
    FUNC_UMAT(f_umat) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);

        DBGVV(" libname = >%s<, len = %d\n", libname, lon1)
        DBGVV("  symbol = >%s<, len = %d\n", symbol, lon2)
    
    if ( ! libsymb_is_known(DLL_DICT, libname, symbol) ) {
        load_umat_lib(libname, symbol);
    }
    f_umat = (FUNC_UMAT())libsymb_get_symbol(DLL_DICT, libname, symbol);

    CALLUMAT(*f_umat,
        stress, statev, ddsdde, sse, spd, scd, rpl, ddsddt, drplde, drpldt,
        stran, dstran, time, dtime, temp, dtemp, predef, dpred, cmname, 
        ndi, nshr, ntens, nstatv, props, nprops, coords, drot, pnewdt, 
        celent, dfgrd0, dfgrd1, noel, npt, layer, kspt, kstep, kinc );
    FreeStr(libname);
    FreeStr(symbol);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}
