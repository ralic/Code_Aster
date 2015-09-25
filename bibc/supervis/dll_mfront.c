/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2015  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* person_in_charge: nicolas.sellenet at edf.fr */

#include "Python.h"
#include "aster.h"
#include "aster_fort.h"
#include "aster_utils.h"
#include "definition_pt.h"

#include "dll_register.h"
#include "dll_mfront.h"

#ifdef HAVE_MFRONT
#include "MFrontBehaviour.h"
#endif

#ifdef _POSIX
#include <dlfcn.h>
PyObject* get_dll_register_dict();


/* *********************************************************************
 *
 *                          MFRONT interface
 *
 * *********************************************************************/

void DEFSSSSP(MFRONT_SET_DOUBLE_PARAMETER, mfront_set_double_parameter,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    char* nomparam, STRING_SIZE lnomparam, DOUBLE* value)
{
#ifdef _POSIX
    /* MFRONT Wrapper : wrapper to the MFRONT set function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char *libname, *symbol, *model, *symbname=NULL, *nom_param;
    FUNC_MFRONT_SET_DOUBLE(f_mfront) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);
    model = MakeCStrFromFStr(nommod, lnommod);
    nom_param = MakeCStrFromFStr(nomparam, lnomparam);

    mfront_name(libname, symbol, model, "_setParameter", &symbname);
    if ( symbname == NULL ) return;

    f_mfront = (FUNC_MFRONT_SET_DOUBLE())libsymb_get_symbol(DLL_DICT, libname, symbname);
    CALLMFRONTSETDOUBLE(f_mfront, nom_param, *value);
    FreeStr(libname);
    FreeStr(symbol);
    FreeStr(model);
    FreeStr(nom_param);
    FreeStr(symbname);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFSSSSP(MFRONT_SET_INTEGER_PARAMETER, mfront_set_integer_parameter,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    char* nomparam, STRING_SIZE lnomparam, INTEGER* value)
{
#ifdef _POSIX
    /* MFRONT Wrapper : wrapper to the MFRONT set function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char *libname, *symbol, *model, *symbname=NULL, *nom_param;
    FUNC_MFRONT_SET_INTEGER(f_mfront) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);
    model = MakeCStrFromFStr(nommod, lnommod);
    nom_param = MakeCStrFromFStr(nomparam, lnomparam);

    mfront_name(libname, symbol, model, "_setUnsignedShortParameter", &symbname);
    if ( symbname == NULL ) return;

    f_mfront = (FUNC_MFRONT_SET_INTEGER())libsymb_get_symbol(DLL_DICT, libname, symbname);
    CALLMFRONTSETINTEGER(f_mfront, nom_param, (unsigned short)(*value));
    FreeStr(libname);
    FreeStr(symbol);
    FreeStr(model);
    FreeStr(nom_param);
    FreeStr(symbname);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFPPSP(MFRONT_GET_EXTERNAL_STATE_VARIABLE,
             mfront_get_external_state_variable,
             INTEGER* pliesv, INTEGER* pnbesv,
             char* txval, STRING_SIZE ltx, INTEGER* nbvarc)
{
#ifdef _POSIX
    /* MFRONT Wrapper
    */

    char** ext_var = (char**)*pliesv;

    unsigned short* nb_ext_var = (unsigned short*)*pnbesv;
    AS_ASSERT(*nb_ext_var <= ltx);
    *nbvarc = *nb_ext_var;

    unsigned short i;
    for ( i = 0; i < *nb_ext_var; ++i )
    {
        SetTabFStr( txval, i, ext_var[i], 8 );
    }
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFSSSPPPPP(MFRONT_GET_POINTERS,
                 mfront_get_pointers,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    INTEGER* pliesv, INTEGER* pnbesv, INTEGER* pfcmfr,
    INTEGER* pmatprop, INTEGER* pnbprop)
{
#ifdef _POSIX
    /* MFRONT Wrapper
    */
    char *libname, *symbol, *model, *symbname=NULL;
    int retour = 0;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);
    model = MakeCStrFromFStr(nommod, lnommod);

    if ( ! libsymb_is_known(DLL_DICT, libname, symbol) ) {
        retour = load_mfront_lib(libname, symbol);
        if (retour == 1)
        {
            error_symbol_not_found(libname, symbname);
        }
    }
    *pfcmfr = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, symbol);

    mfront_name(libname, symbol, model, "_ExternalStateVariables", &symbname);
    if ( symbname == NULL )
    {
        error_symbol_not_found(libname, symbname);
    }

//     char** test_char = libsymb_get_symbol(DLL_DICT, libname, symbname);
    *pliesv = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, symbname);

    mfront_name(libname, symbol, model, "_nExternalStateVariables", &symbname);
    if ( symbname == NULL ) {
        error_symbol_not_found(libname, symbname);
    }
//     int* test_int = libsymb_get_symbol(DLL_DICT, libname, symbname);
    *pnbesv = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, symbname);
    if ( symbname == NULL ) {
        error_symbol_not_found(libname, symbname);
    }

    // may be used for performance reason: pointers in a cache
    *pmatprop = (INTEGER)0;
    *pnbprop = (INTEGER)0;

    FreeStr(libname);
    FreeStr(model);
    FreeStr(symbol);
    FreeStr(symbname);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFSSSPP(MFRONT_GET_NBVARI, mfront_get_nbvari,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    INTEGER* ndim, INTEGER* nbvari)
{
#ifdef _POSIX
    /* MFRONT Wrapper
    */
    char *libname, *symbol, *model, *symbname=NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);
    model = MakeCStrFromFStr(nommod, lnommod);

    mfront_name(libname, symbol, model, "_InternalStateVariablesTypes", &symbname);
    if ( symbname == NULL ) {
        error_symbol_not_found(libname, symbol);
    }

    int* int_var = (int*)libsymb_get_symbol(DLL_DICT, libname, symbname);

    mfront_name(libname, symbol, model, "_nInternalStateVariables", &symbname);
    if ( symbname == NULL ) {
        error_symbol_not_found(libname, symbol);
    }
    unsigned short* nb_int_var = (unsigned short*)libsymb_get_symbol(DLL_DICT, libname, symbname);

    *nbvari = 0;
    unsigned short i;
    for ( i = 0; i < *nb_int_var; ++i )
    {
        if ( int_var[i] == 0 )
        {
            ++(*nbvari);
        }
        else if ( int_var[i] == 1 )
        {
            if ( *ndim == 2 )
            {
                (*nbvari) += 4;
            }
            else if ( *ndim == 3 )
            {
                (*nbvari) += 6;
            }
            else
            {
                AS_ASSERT( *ndim == 2 || *ndim == 3 );
            }
        }
       else if ( int_var[i] == 3 )
        {
                 (*nbvari) += 9;
        }
        else
        {
            AS_ASSERT( int_var[i] == 0 || int_var[i] == 1 || int_var[i] == 3);
        }
    }

    FreeStr(libname);
    FreeStr(symbol);
    FreeStr(model);
    FreeStr(symbname);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFPPPPPPPPPPPPPPPPPP(MFRONT_BEHAVIOUR, mfront_behaviour,
    INTEGER* pfcmfr, DOUBLE* stress, DOUBLE* statev, DOUBLE* ddsdde, DOUBLE* stran,
    DOUBLE* dstran, DOUBLE* dtime, DOUBLE* temp, DOUBLE* dtemp, DOUBLE* predef,
    DOUBLE* dpred, INTEGER* ntens, INTEGER* nstatv, DOUBLE* props, INTEGER* nprops,
    DOUBLE* drot, DOUBLE* pnewdt, INTEGER* nummod)
{
#ifdef _POSIX
    /* MFRONT Wrapper : wrapper to the MFRONT function through the function pointer
     * Load the library if necessary (at the first call).
    */
    FUNC_MFRONT(f_mfront) = NULL;

    f_mfront = (FUNC_MFRONT())(*pfcmfr);

    CALLMFRONTBEHAVIOUR(*f_mfront,
        stress, statev, ddsdde, stran, dstran,
        dtime, temp, dtemp, predef, dpred,
        ntens, nstatv, props, nprops, drot,
        pnewdt, nummod);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

/**
 * \brief Fill the array of the material properties names
 * @param pmatprop  Pointer on the data in the library
 * @param nbval     Number of values of material properties
 * @param txval     Array of strings
 */
void DEFSPS(MFRONT_GET_MATER_PROP,
            mfront_get_mater_prop,
             _IN char* rela, STRING_SIZE lrela,
            _OUT INTEGER* nbval,
            _OUT char* txval, STRING_SIZE ltx)
{
#ifdef HAVE_MFRONT
    /* MFRONT Wrapper
    */
    char *crela;
    char library[] = "lib" ASTERBEHAVIOUR;
    unsigned int i, size;
    char **props;
    AS_ASSERT(ltx == 16);

    crela = MakeCStrFromFStr(rela, lrela);
    props = getTridimMaterialPropertiesNames(crela, &size);
    for (i = 0; i < size; ++i) {
        SetTabFStr( txval, i, props[i], 16 );
        free(props[i]);
    }
    *nbval = (INTEGER)size;
    free(props);
    FreeStr(crela);
#else
    printf("MFront library is required for this functionnality.\n");
    abort();
#endif
}

int load_mfront_lib(const char* libname, const char* symbol)
{
    void *mfront_handle;
    char *error;
    char symbol_[256], *valk;
    INTEGER ibid=0, n0=0, nk=0;
    DOUBLE rbid=0.;
    FUNC_MFRONT(f_mfront) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    AS_ASSERT(strlen(symbol) < 255);
    strcpy(symbol_, symbol);

    DEBUG_DLL_VV("Loading '%s'%s ", libname, "...");
    mfront_handle = dlopen(libname, RTLD_NOW);
    if ( ! mfront_handle ) {
        printf("\n%s\n", dlerror());
        nk = 2;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "MFRONT", VALK_SIZE);
        SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
        CALL_UTMESS_CORE("F", "FERMETUR_13", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
        FreeStr(valk);  // uncallable
    }
    DEBUG_DLL_VV("searching symbol '%s'%s ", symbol, "...");
    dlerror();    /* Clear any existing error */

    *(void **) (&f_mfront) = dlsym(mfront_handle, symbol);
    if ((error = dlerror()) != NULL)  {
        dlerror();
        strcat(symbol_, "_");
        DEBUG_DLL_VV("trying symbol '%s'%s ", symbol_, "...");
        *(void **) (&f_mfront) = dlsym(mfront_handle, symbol_);
    }

    if ((error = dlerror()) != NULL)  {
        DEBUG_DLL_VV("not found %s%s\n", ":-(", "");
        return 1;
    }
    DEBUG_DLL_VV("found: %s %p", "address", (char *)f_mfront);

    /* register these MFRONT lib */
    if ( libsymb_register(DLL_DICT, libname, symbol,
                            mfront_handle, (FUNC_PTR)f_mfront) ) {
        printf("Registering of '%s' and '%s' failed!\n", libname, symbol);
    }
    return 0;
}
#endif

char* test_mfront_symbol(const char* libname, char* name1, char* name2)
{
    int retour = 0;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    if ( ! libsymb_is_known(DLL_DICT, libname, name1) )
    {
        retour = load_mfront_lib(libname, name1);
        if ( retour == 0 )
            return name1;
    }
    else
        return name1;
    if ( ! libsymb_is_known(DLL_DICT, libname, name2) )
    {
        retour = load_mfront_lib(libname, name2);
        if ( retour == 0 )
            return name2;
    }
    else
        return name2;
    return NULL;
}

void mfront_name(
         _IN char* libname, _IN char* symbol, _IN char* model,
         _IN char* basename, _OUT char** name)
{
    char *name1, *name2;

    name1 = (char *)malloc(strlen(symbol) + strlen(model) + strlen(basename) + 1);
    strcpy(name1, symbol);
    strcat(name1, model);
    strcat(name1, basename);

    name2 = (char *)malloc(strlen(symbol) + strlen(basename) + 1);
    strcpy(name2, symbol);
    strcat(name2, basename);
    DEBUG_DLL_VV("name1: '%s' name2: '%s'", name1, name2);

    *name = test_mfront_symbol(libname, name1, name2);
    if ( *name == NULL ) {
        DEBUG_DLL_VV(" libname = >%s<%s", libname, " ")
        DEBUG_DLL_VV(" symbol1 = >%s<, symbol2 = >%s<", name1, name2)
        free(name1);
        free(name2);
    }
    else if ( strcmp(*name, name1) == 0 ) {
        free(name2);
    }
    else if ( strcmp(*name, name2) == 0 ) {
        free(name1);
    }
}

void error_symbol_not_found(const char* libname, const char* symbname)
{
    char *valk;
    INTEGER ibid=0, n0=0, nk=3;
    DOUBLE rbid=0.;
    valk = MakeTabFStr(nk, VALK_SIZE);
    SetTabFStr(valk, 0, "MFRONT", VALK_SIZE);
    SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
    SetTabFStr(valk, 2, (char *)symbname, VALK_SIZE);
    CALL_UTMESS_CORE("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
    FreeStr(valk);  // uncallable
}
