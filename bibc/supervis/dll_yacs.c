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
/* person_in_charge: mathieu.courtois at edf.fr */

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
 *                      YACS/CALCIUM interface
 * 
 * *********************************************************************/
#define LIB_YACS "libCalciumC.so"

#define FUNC_CPECH(NAME) void DEF_P_PPPPSPSP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERREAL4*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*)

#define FUNC_CPEDB(NAME) void DEF_P_PPPPSPPP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERDOUBLE*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, \
        ASTERDOUBLE*, ASTERINTEGER4*)

#define FUNC_CPEEN(NAME) void DEF_P_PPPPSPPP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERREAL4*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, \
        ASTERINTEGER4*, ASTERINTEGER4*)

#define FUNC_CPLCH(NAME) void DEF_P_PPPPPSPPSP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERREAL4*, ASTERREAL4*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*)

#define FUNC_CPLDB(NAME) void DEF_P_PPPPPSPPPP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERDOUBLE*, ASTERDOUBLE*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, ASTERINTEGER4*, \
        ASTERDOUBLE*, ASTERINTEGER4*)

#define FUNC_CPLEN(NAME) void DEF_P_PPPPPSPPPP(*NAME, \
        ASTERINTEGER *, ASTERINTEGER4*, ASTERREAL4*, ASTERREAL4*, ASTERINTEGER4*, \
        char*, STRING_SIZE, ASTERINTEGER4*, ASTERINTEGER4*, \
        ASTERINTEGER4*, ASTERINTEGER4*)

void load_yacs_lib()
{
    /* load the Yacs/Calcium library and initialize pointers to Yacs functions
     */
    char *error;
    void *yacs_handle;
    char *valk;
    int iret=0;
    ASTERINTEGER n0=0, nk=0, ibid=0;
    ASTERDOUBLE rbid=0.;
    char symbol[12];
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    printf("Loading %s... ", LIB_YACS);
    yacs_handle = dlopen(LIB_YACS, RTLD_NOW);
    if ( ! yacs_handle ) {
        printf("\n%s\n", dlerror());
        nk = 2;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "YACS/Calcium", VALK_SIZE);
        SetTabFStr(valk, 1, LIB_YACS, VALK_SIZE);
        CALL_UTMESS_CORE("F", "FERMETUR_13", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
        FreeStr(valk);  // uncallable
    }

    iret = 0;
    /*  SUBROUTINE CPECH(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,NOMPAL,INFO)
     */
    FUNC_CPECH(f_cpech) = NULL;
    /* string sizes are fixed : lnomvar=144, lnompal=8 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPECH,cpech));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpech) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cpech) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPEDB(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,PARAMR,INFO)
     */
    FUNC_CPEDB(f_cpedb) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPEDB,cpedb));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpedb) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cpedb) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPEEN(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,PARAMI,INFO)
     */
    FUNC_CPEEN(f_cpeen) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPEEN,cpeen));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpeen) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cpeen) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /* SUBROUTINE CPLCH(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                NOMVAR,IDIM,TAILLE,NOMPAL,INFO)
     */
    FUNC_CPLCH(f_cplch) = NULL;
    /* string sizes are fixed : lnomvar=144, lnompal=8 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPLCH,cplch));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cplch) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cplch) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPLDB(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                 NOMVAR,IDIM,TAILLE,PARAMR,INFO)
     */
    FUNC_CPLDB(f_cpldb) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPLDB,cpldb));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpldb) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cpldb) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPLEN(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                 NOMVAR,IDIM,TAILLE,PARAMI,INFO)
     */
    FUNC_CPLEN(f_cplen) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, S_FUNC(CPLEN,cplen));
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cplen) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (FUNC_PTR)f_cplen) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /* in case of error... */
    if ( iret != 0 )  {
        error = dlerror();
        printf("\n%s\n", error);
        nk = 3;
        valk = MakeTabFStr(nk, VALK_SIZE);
        SetTabFStr(valk, 0, "YACS/Calcium", VALK_SIZE);
        SetTabFStr(valk, 1, LIB_YACS, VALK_SIZE);
        SetTabFStr(valk, 2, symbol, VALK_SIZE);
        CALL_UTMESS_CORE("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid, " ");
        FreeStr(valk);  // unreachable
    }
}
#endif

/* *********************************************************************
 * 
 * The following wrapper functions are called from fortran source files.
 * 
 * *********************************************************************/

/* SUBROUTINE CPECH */
void DEFPPPPSPSP(CPECH,cpech,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERREAL4 *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim,
        char *fnompal, STRING_SIZE lnompal, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar, *nompal;
    PyObject* DLL_DICT;
    FUNC_CPECH(f_cpech) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPECH,cpech));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpech = (FUNC_CPECH())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    //assert lnompal ==  8!
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    nompal = MakeCStrFromFStr(fnompal, lnompal);
    CALL_P_PPPPSPSP(*f_cpech, icompo, cpiter, tf, numpas,
                              nomvar, idim, nompal, info );
    FreeStr(nomvar);
    FreeStr(nompal);
#endif
}


/* SUBROUTINE CPEDB */
void DEFPPPPSPPP(CPEDB,cpedb,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERDOUBLE *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim,
        ASTERDOUBLE *paramr, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar;
    PyObject* DLL_DICT;
    FUNC_CPEDB(f_cpedb) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPEDB,cpedb));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpedb = (FUNC_CPEDB())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    CALL_P_PPPPSPPP(*f_cpedb, icompo, cpiter, tf, numpas,
                              nomvar, idim, paramr, info );
    FreeStr(nomvar);
#endif
}


/* SUBROUTINE CPEEN */
void DEFPPPPSPPP(CPEEN,cpeen,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERREAL4 *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim,
        ASTERINTEGER4 *parami, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar;
    PyObject* DLL_DICT;
    FUNC_CPEEN(f_cpeen) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPEEN,cpeen));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpeen = (FUNC_CPEEN())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    CALL_P_PPPPSPPP(*f_cpeen, icompo, cpiter, tf, numpas,
                              nomvar, idim, parami, info );
    FreeStr(nomvar);
#endif
}


/* SUBROUTINE CPLCH */
void DEFPPPPPSPPSP(CPLCH,cplch,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERREAL4 *ti,
        ASTERREAL4 *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim, ASTERINTEGER4 *taille,
        char *fnompal, STRING_SIZE lnompal, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar, *nompal;
    PyObject* DLL_DICT;
    FUNC_CPLCH(f_cplch) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPLCH,cplch));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cplch = (FUNC_CPLCH())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    //assert lnompal ==  8!
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    nompal = MakeCStrFromFStr(fnompal, lnompal);
    CALL_P_PPPPPSPPSP(*f_cplch, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, nompal, info );
    FreeStr(nomvar);
    FreeStr(nompal);
#endif
}


/* SUBROUTINE CPLDB */
void DEFPPPPPSPPPP(CPLDB,cpldb,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERDOUBLE *ti,
        ASTERDOUBLE *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim, ASTERINTEGER4 *taille,
        ASTERDOUBLE *paramr, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar;
    PyObject* DLL_DICT;
    FUNC_CPLDB(f_cpldb) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPLDB,cpldb));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpldb = (FUNC_CPLDB())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    CALL_P_PPPPPSPPPP(*f_cpldb, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, paramr, info );
    FreeStr(nomvar);
#endif
}


/* SUBROUTINE CPLEN */
void DEFPPPPPSPPPP(CPLEN,cplen,
        ASTERINTEGER *icompo, ASTERINTEGER4 *cpiter, ASTERREAL4 *ti,
        ASTERREAL4 *tf, ASTERINTEGER4 *numpas,
        char *fnomvar, STRING_SIZE lnomvar, ASTERINTEGER4 *idim, ASTERINTEGER4 *taille,
        ASTERINTEGER4 *parami, ASTERINTEGER4 *info )
{
#ifdef _POSIX
    char symbol[12];
    char *nomvar;
    PyObject* DLL_DICT;
    FUNC_CPLEN(f_cplen) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, S_FUNC(CPLEN,cplen));
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cplen = (FUNC_CPLEN())libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    nomvar = MakeCStrFromFStr(fnomvar, lnomvar);
    CALL_P_PPPPPSPPPP(*f_cplen, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, parami, info );
    FreeStr(nomvar);
#endif
}
