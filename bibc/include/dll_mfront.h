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

#ifndef DLL_MFRONT_H
#define DLL_MFRONT_H

#include "aster.h"

/* declarations of pointers on MFRONT functions */
#define FUNC_MFRONT(NAME)  void DEFMFRONTBEHAVIOUR(*NAME, \
        DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, DOUBLE*, \
            DOUBLE*, DOUBLE*, INTEGER*, INTEGER*, DOUBLE*, INTEGER*, \
            DOUBLE*, DOUBLE*, INTEGER*)
#define FUNC_MFRONT_SET_DOUBLE(NAME)  void DEFMFRONTSETDOUBLE(*NAME, char*, DOUBLE, STRING_SIZE)
#define FUNC_MFRONT_SET_INTEGER(NAME)  void DEFMFRONTSETINTEGER(*NAME, char*, INTEGER, STRING_SIZE)
#define FUNC_MFRONT_SET_OUTOFBOUNDS_POLICY(NAME)  void DEFMFRONTSETOUTOFBOUNDSPOLICY(*NAME, INTEGER)

/*
 *   PUBLIC FUNCTIONS
 *
 */


/*
 *   PRIVATE FUNCTIONS - UTILITIES
 *
 */

/**
 * \brief Return the symbol name of the MFront lib after testing if it should
 *        contain the modelization or not.
 * @param libname   Name of library
 * @param symbol    Name of the main function (ex. asterbehaviourname)
 * @param model     Name of the modelization
 * @param basename  Basename/suffix of the symbol to find
 * @param name      Pointer on the string containing the found symbol name,
 *                  it must be freed by the caller.
 */
void mfront_name(
         _IN char* libname, _IN char* symbol, _IN char* model,
         _IN char* basename, _OUT char** name);
/**
 * \brief Raise an error 'symbol not found'
 */
void error_symbol_not_found(const char* libname, const char* symbname);

/**
 * \brief Clean parameter names: convert 'name[i]' into 'name_i'
 */
void clean_parameter(_IN const char* src, _OUT char** dest);

/**
 * \brief Load MFRONT library and initialize pointers to MFRONT functions
 */
int load_mfront_lib(const char* libname, const char* symbol);

char* test_mfront_symbol(const char* libname, char* name1, char* name2);


/* FIN DLL_MFRONT_H */
#endif
