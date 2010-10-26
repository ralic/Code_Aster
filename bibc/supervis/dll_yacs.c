/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_yacs supervis  DATE 25/10/2010   AUTEUR COURTOIS M.COURTOIS */
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
#include "definition_pt.h"

#include "dll_register.h"

PyObject* get_dll_register_dict();


/* *********************************************************************
 * 
 *                      YACS/CALCIUM interface
 * 
 * *********************************************************************/
#define LIB_YACS "libCalciumC.so"


void load_yacs_lib()
{
    /* load the Yacs/Calcium library and initialize pointers to Yacs functions
     */
    char *error;
    void *yacs_handle;
    char *valk, *valk_i;
    int iret=0;
    INTEGER n0=0, nk=0, ibid=0;
    DOUBLE rbid=0.;
    char symbol[12];
    PyObject* DLL_DICT;
    DLL_DICT = get_dll_register_dict();
    
    printf("Loading %s... ", LIB_YACS);
    yacs_handle = dlopen(LIB_YACS, RTLD_NOW);
    if ( ! yacs_handle ) {
        printf("\n%s\n", dlerror());
        nk = 2;
        valk = (char *)malloc(nk*VALK_SIZE*sizeof(char));
        STRING_FCPY(valk,   VALK_SIZE, "YACS/Calcium", strlen("YACS/Calcium"));
        valk_i = &valk[VALK_SIZE];
        STRING_FCPY(valk_i, VALK_SIZE, LIB_YACS,       strlen(LIB_YACS));
        CALL_U2MESG("F", "FERMETUR_13", &nk, valk, &n0, &ibid, &n0, &rbid);
        free(valk);  // uncallable
    }

    iret = 0;
    /*  SUBROUTINE CPECH(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,NOMPAL,INFO)
     */
    void DEF_P_PPPPSPSP(*f_cpech,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144, lnompal=8 */
    if ( iret == 0 ) {
        strcpy(symbol, "cpech_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpech) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cpech) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPEDB(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,PARAMR,INFO)
     */
    void DEF_P_PPPPSPPP(*f_cpedb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        DOUBLE *paramr, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, "cpedb_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpedb) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cpedb) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPEEN(ICOMPO,CPITER,TF,NUMPAS,
     * &                 NOMVAR,IDIM,PARAMI,INFO)
     */
    void DEF_P_PPPPSPPP(*f_cpeen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        INTEGER4 *parami, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, "cpeen_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpeen) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cpeen) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /* SUBROUTINE CPLCH(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                NOMVAR,IDIM,TAILLE,NOMPAL,INFO)
     */
    void DEF_P_PPPPPSPPSP(*f_cplch,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144, lnompal=8 */
    if ( iret == 0 ) {
        strcpy(symbol, "cplch_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cplch) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cplch) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPLDB(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                 NOMVAR,IDIM,TAILLE,PARAMR,INFO)
     */
    void DEF_P_PPPPPSPPPP(*f_cpldb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *ti, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        DOUBLE *paramr, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, "cpldb_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cpldb) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cpldb) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /*  SUBROUTINE CPLEN(ICOMPO,CPITER,TI,TF,NUMPAS,
     * &                 NOMVAR,IDIM,TAILLE,PARAMI,INFO)
     */
    void DEF_P_PPPPPSPPPP(*f_cplen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        INTEGER4 *parami, INTEGER4 *info) = NULL;
    /* string sizes are fixed : lnomvar=144 */
    if ( iret == 0 ) {
        strcpy(symbol, "cplen_");
        printf("searching symbol '%s'... ", symbol);
        dlerror();    /* Clear any existing error */

        *(void **) (&f_cplen) = dlsym(yacs_handle, symbol);
        if ((error = dlerror()) != NULL)  {
            iret = 4;
        } else {
            printf("found\n");
            /* register yacs function */
            if ( libsymb_register(DLL_DICT, LIB_YACS, symbol,
                                  yacs_handle, (void*)f_cplen) ) {
                printf("Registering '%s' and '%s' failed!\n", LIB_YACS, symbol);
            }
        }
    }

    /* in case of error... */
    if ( iret != 0 )  {
        error = dlerror();
        printf("\n%s\n", error);
        nk = 3;
        valk = (char *)malloc(nk*VALK_SIZE*sizeof(char));
        STRING_FCPY(valk,   VALK_SIZE, "YACS/Calcium", strlen("YACS/Calcium"));
        valk_i = &valk[VALK_SIZE];
        STRING_FCPY(valk_i, VALK_SIZE, LIB_YACS,       strlen(LIB_YACS));
        valk_i = &valk[VALK_SIZE*2];
        STRING_FCPY(valk_i, VALK_SIZE, symbol,         strlen(symbol));
        CALL_U2MESG("F", "FERMETUR_14", &nk, valk, &n0, &ibid, &n0, &rbid);
        free(valk);  // unreachable
    }
}


/* *********************************************************************
 * 
 * The following wrapper functions are called from fortran source files.
 * 
 * *********************************************************************/

/* SUBROUTINE CPECH */
void DEFPPPPSPSP(CPECH,cpech,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPSPSP(*f_cpech,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cpech_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpech = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    //assert lnompal ==  8!
    CALL_P_PPPPSPSP(*f_cpech, icompo, cpiter, tf, numpas,
                              nomvar, idim, nompal, info );
}


/* SUBROUTINE CPEDB */
void DEFPPPPSPPP(CPEDB,cpedb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        DOUBLE *paramr, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPSPPP(*f_cpedb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        DOUBLE *paramr, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cpedb_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpedb = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    CALL_P_PPPPSPPP(*f_cpedb, icompo, cpiter, tf, numpas,
                              nomvar, idim, paramr, info );
}


/* SUBROUTINE CPEEN */
void DEFPPPPSPPP(CPEEN,cpeen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        INTEGER4 *parami, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPSPPP(*f_cpeen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim,
        INTEGER4 *parami, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cpeen_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpeen = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    CALL_P_PPPPSPPP(*f_cpeen, icompo, cpiter, tf, numpas,
                              nomvar, idim, parami, info );
}


/* SUBROUTINE CPLCH */
void DEFPPPPPSPPSP(CPLCH,cplch,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPPSPPSP(*f_cplch,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        char *nompal, STRING_SIZE lnompal, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cplch_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cplch = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    //assert lnompal ==  8!
    CALL_P_PPPPPSPPSP(*f_cplch, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, nompal, info );
}


/* SUBROUTINE CPLDB */
void DEFPPPPPSPPPP(CPLDB,cpldb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *ti, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        DOUBLE *paramr, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPPSPPPP(*f_cpldb,
        INTEGER *icompo, INTEGER4 *cpiter, DOUBLE *ti, DOUBLE *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        DOUBLE *paramr, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cpldb_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cpldb = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    CALL_P_PPPPPSPPPP(*f_cpldb, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, paramr, info );
}


/* SUBROUTINE CPLEN */
void DEFPPPPPSPPPP(CPLEN,cplen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        INTEGER4 *parami, INTEGER4 *info )
{
    char symbol[12];
    PyObject* DLL_DICT;
    void DEF_P_PPPPPSPPPP(*f_cplen,
        INTEGER *icompo, INTEGER4 *cpiter, REAL4 *ti, REAL4 *tf, INTEGER4 *numpas,
        char *nomvar, STRING_SIZE lnomvar, INTEGER4 *idim, INTEGER4 *taille,
        INTEGER4 *parami, INTEGER4 *info) = NULL;
    DLL_DICT = get_dll_register_dict();

    strcpy(symbol, "cplen_");
    if ( ! libsymb_is_known(DLL_DICT, LIB_YACS, symbol) ) {
        load_yacs_lib();
    }
    
    f_cplen = libsymb_get_symbol(DLL_DICT, LIB_YACS, symbol);

    //assert lnomvar == 144 !
    CALL_P_PPPPPSPPPP(*f_cplen, icompo, cpiter, ti, tf, numpas,
                               nomvar, idim, taille, parami, info );
}


