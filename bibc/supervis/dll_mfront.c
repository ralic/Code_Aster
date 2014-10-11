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

#ifdef _POSIX
#include <dlfcn.h>
PyObject* get_dll_register_dict();


/* *********************************************************************
 *
 *                          MFRONT interface
 *
 * *********************************************************************/

/* declarations of pointers on MFRONT functions */
#define FUNC_MFRONT(NAME)  void DEFMFRONTBEHAVIOUR(*NAME, \
        DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, \
            DOUBLE*, DOUBLE*, INTEGER*, INTEGER*, DOUBLE*, INTEGER*, \
            DOUBLE*, DOUBLE*, INTEGER*)
#define FUNC_MFRONT_SET_DOUBLE(NAME)  void DEFMFRONTSETDOUBLE(*NAME, char*, DOUBLE, STRING_SIZE)
#define FUNC_MFRONT_SET_INTEGER(NAME)  void DEFMFRONTSETINTEGER(*NAME, char*, INTEGER, STRING_SIZE)

int load_mfront_lib(const char* libname, const char* symbol)
{
    /* load MFRONT library and initialize pointers to MFRONT functions
     */
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
        return 1;
    }
    DEBUG_DLL_VV("found%s%s", "", "");

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

/**
 * \brief Return the two function names with and without modelization.
 *        Both name1 and name2 must be freed by the caller.
 */
void mfront_names(
         _IN char* nomsub, STRING_SIZE lnomsub,
         _IN char* nommod, STRING_SIZE lnommod,
         _IN char* basename,
        _OUT char** name1,
        _OUT char** name2)
{
    char *sym, *mod;

    sym = MakeCStrFromFStr(nomsub, lnomsub);
    mod = MakeCStrFromFStr(nommod, lnommod);

    *name1 = (char *)malloc(strlen(sym) + strlen(mod) + strlen(basename) + 1);
    strcpy(*name1, sym);
    strcat(*name1, mod);
    strcat(*name1, basename);

    *name2 = (char *) malloc(strlen(sym) + strlen(basename) + 1);
    strcpy(*name2, sym);
    strcat(*name2, basename);
    DEBUG_DLL_VV("#DEBUG: name1: '%s' name2: '%s'\n", *name1, *name2);
    FreeStr(sym);
    FreeStr(mod);
}

void DEFMFRONTSETDOUBLEWRAP(MFRONT_SET_DOUBLE_PARAMETER, mfront_set_double_parameter,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    char* nomparam, STRING_SIZE lnomparam, DOUBLE* value)
{
#ifdef _POSIX
    /* MFRONT Wrapper : wrapper to the MFRONT set function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char *libname, *symb_set_param, *symb_set_param_m, *nom_param, *tmp = NULL;
    char* set_param = "_setParameter";
    FUNC_MFRONT_SET_DOUBLE(f_mfront) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    nom_param = MakeCStrFromFStr(nomparam, lnomparam);

    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_setParameter",
                 &symb_set_param_m, &symb_set_param);

    tmp = test_mfront_symbol(libname, symb_set_param_m, symb_set_param);
    if ( tmp == NULL ) return;

        DEBUG_DLL_VV(" libname = >%s<, len = %d\n", libname, (int)strlen(libname))
        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", tmp, (int)strlen(tmp))

    f_mfront = (FUNC_MFRONT_SET_DOUBLE())libsymb_get_symbol(DLL_DICT, libname, tmp);
    CALLMFRONTSETDOUBLE(f_mfront, nom_param, *value);
    FreeStr(libname);
    FreeStr(symb_set_param);
    FreeStr(symb_set_param_m);
    FreeStr(nom_param);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFMFRONTSETINTEGERWRAP(MFRONT_SET_INTEGER_PARAMETER, mfront_set_integer_parameter,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    char* nomparam, STRING_SIZE lnomparam, INTEGER* value)
{
#ifdef _POSIX
    /* MFRONT Wrapper : wrapper to the MFRONT set function through the function pointer
     * Load the library if necessary (at the first call).
    */
    char *libname, *symb_set_param_m, *symb_set_param, *nom_param, *tmp = NULL;
    char* set_param = "_setUnsignedShortParameter";
    FUNC_MFRONT_SET_INTEGER(f_mfront) = NULL;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    nom_param = MakeCStrFromFStr(nomparam, lnomparam);

    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_setUnsignedShortParameter",
                 &symb_set_param_m, &symb_set_param);

    tmp = test_mfront_symbol(libname, symb_set_param_m, symb_set_param);
    if ( tmp == NULL ) return;

        DEBUG_DLL_VV(" libname = >%s<, len = %d\n", libname, (int)strlen(libname))
        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", tmp, (int)strlen(tmp))

    f_mfront = (FUNC_MFRONT_SET_INTEGER())libsymb_get_symbol(DLL_DICT, libname, tmp);
    CALLMFRONTSETINTEGER(f_mfront, nom_param, (unsigned short)(*value));
    FreeStr(libname);
    FreeStr(symb_set_param);
    FreeStr(symb_set_param_m);
    FreeStr(nom_param);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFMFRONTGETEXTSTVARWRAP(MFRONT_GET_EXTERNAL_STATE_VARIABLE,
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
    INTEGER* pgetmprop, INTEGER* pgetnbprop)
{
#ifdef _POSIX
    /* MFRONT Wrapper
    */
    char *libname, *symbol, *symb_txt_get_ext_var, *symb_txt_nb_ext_var, *tmp = NULL;
    char *symb_txt_get_ext_var_m, *symb_txt_nb_ext_var_m;
    char* txt_get_ext_var = "_ExternalStateVariables";
    char* txt_nb_ext_var = "_nExternalStateVariables";
    char *valk;
    int retour = 0;
    INTEGER ibid=0, n0=0, nk=0;
    DOUBLE rbid=0.;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);
    symbol = MakeCStrFromFStr(nomsub, lnomsub);

    if ( ! libsymb_is_known(DLL_DICT, libname, symbol) ) {
        retour = load_mfront_lib(libname, symbol);
        if (retour == 1)
        {
            nk = 3;
            valk = MakeTabFStr(nk, VALK_SIZE);
            SetTabFStr(valk, 0, "MFRONT", VALK_SIZE);
            SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
            SetTabFStr(valk, 2, (char *)symbol, VALK_SIZE);
            CALL_UTMESS_CORE("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
            FreeStr(valk);  // uncallable
        }
    }
    *pfcmfr = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, symbol);

    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_ExternalStateVariables",
                 &symb_txt_get_ext_var_m, &symb_txt_get_ext_var);
    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_nExternalStateVariables",
                 &symb_txt_nb_ext_var_m, &symb_txt_nb_ext_var);

    tmp = test_mfront_symbol(libname, symb_txt_get_ext_var_m, symb_txt_get_ext_var);
    if ( tmp == NULL )
    {
        nk = 3;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "MFRONT", VALK_SIZE);
        SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
        SetTabFStr(valk, 2, (char *)symb_txt_nb_ext_var_m, VALK_SIZE);
        CALL_UTMESS_CORE("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
        FreeStr(valk);  // uncallable
    }

        DEBUG_DLL_VV(" libname = >%s<, len = %d\n", libname, (int)strlen(libname))
        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", tmp, (int)strlen(tmp))

//     char** test_char = libsymb_get_symbol(DLL_DICT, libname, tmp);
    *pliesv = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, tmp);

        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", symbol, (int)strlen(symb_txt_nb_ext_var))

    tmp = test_mfront_symbol(libname, symb_txt_nb_ext_var_m, symb_txt_nb_ext_var);
//     int* test_int = libsymb_get_symbol(DLL_DICT, libname, tmp);
    *pnbesv = (INTEGER)libsymb_get_symbol(DLL_DICT, libname, tmp);

    *pgetmprop = (INTEGER)0;
    *pgetnbprop = (INTEGER)0;

    FreeStr(libname);
    FreeStr(symbol);
    FreeStr(symb_txt_get_ext_var_m);
    FreeStr(symb_txt_nb_ext_var_m);
    FreeStr(symb_txt_get_ext_var);
    FreeStr(symb_txt_nb_ext_var);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFMFRONTGETNBVARIWRAP(MFRONT_GET_NBVARI, mfront_get_nbvari,
    char* nomlib, STRING_SIZE lnomlib, char* nomsub, STRING_SIZE lnomsub,
    char* nommod, STRING_SIZE lnommod,
    INTEGER* ndim, INTEGER* nbvari)
{
#ifdef _POSIX
    /* MFRONT Wrapper
    */
    char *libname, *symb_txt_int_var_type, *symb_nb_int_var;
    char *symb_txt_int_var_type_m, *symb_nb_int_var_m, *tmp = NULL;
    char* txt_int_var_type = "_InternalStateVariablesTypes";
    char* txt_nb_int_var = "_nInternalStateVariables";
    char *valk;
    INTEGER ibid=0, n0=0, nk=0;
    DOUBLE rbid=0.;
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();

    libname = MakeCStrFromFStr(nomlib, lnomlib);

    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_InternalStateVariablesTypes",
                 &symb_txt_int_var_type_m, &symb_txt_int_var_type);

    mfront_names(nomsub, lnomsub, nommod, lnommod,
                 "_nInternalStateVariables",
                 &symb_nb_int_var_m, &symb_nb_int_var);

    tmp = test_mfront_symbol(libname, symb_txt_int_var_type_m, symb_txt_int_var_type);
    if ( tmp == NULL )
    {
        nk = 3;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "MFRONT", VALK_SIZE);
        SetTabFStr(valk, 1, (char *)libname, VALK_SIZE);
        SetTabFStr(valk, 2, (char *)symb_txt_int_var_type_m, VALK_SIZE);
        CALL_UTMESS_CORE("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
        FreeStr(valk);  // uncallable
    }

        DEBUG_DLL_VV(" libname = >%s<, len = %d\n", libname, (int)strlen(libname))
        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", tmp, (int)strlen(tmp))

    int* int_var = (int*)libsymb_get_symbol(DLL_DICT, libname, tmp);

    tmp = test_mfront_symbol(libname, symb_nb_int_var_m, symb_nb_int_var);

        DEBUG_DLL_VV("  symbol = >%s<, len = %d\n", tmp, (int)strlen(tmp))

    unsigned short* nb_int_var = (unsigned short*)libsymb_get_symbol(DLL_DICT, libname, tmp);

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
    FreeStr(symb_txt_int_var_type);
    FreeStr(symb_nb_int_var);
    FreeStr(symb_txt_int_var_type_m);
    FreeStr(symb_nb_int_var_m);
#else
    printf("Not available under Windows.\n");
    abort();
#endif
}

void DEFMFRONTBEHAVIOURWRAP(MFRONT_BEHAVIOUR, mfront_behaviour, INTEGER* pfcmfr,
    DOUBLE* stress, DOUBLE* statev, DOUBLE* ddsdde, DOUBLE* stran,
    DOUBLE* dstran, DOUBLE* dtime, DOUBLE* temp, DOUBLE* dtemp,
    DOUBLE* predef, DOUBLE* dpred, INTEGER* ntens, INTEGER* nstatv,
    DOUBLE* props, INTEGER* nprops, DOUBLE* drot, DOUBLE* pnewdt,
    INTEGER* nummod)
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
