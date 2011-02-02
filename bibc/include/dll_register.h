/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF dll_register include  DATE 02/02/2011   AUTEUR COURTOIS M.COURTOIS */
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

#ifndef DLL_REGISTER_H
#define DLL_REGISTER_H

#if dll_register_DEBUG
#define DBGV(fmt, a) printf(fmt, a); fflush(stdout);
#define DBGVV(fmt, a, b) printf(fmt, a, b); fflush(stdout);
#define PYDBG(label, pyobj) fprintf(stdout, label); \
            PyObject_Print(pyobj, stdout, 0); \
            fprintf(stdout, "\n"); \
            fflush(stdout);
#define DR_ASSERT(cond) if ( !(cond) ) { fprintf(stdout,"--- Assertion FAILED: %s\n", #cond); } \
            else { fprintf(stdout,"--- OK : %s\n", #cond); } \
            fflush(stdout);
#else
#define DBGV(a, b)
#define DBGVV(a, b, c)
#define PYDBG(a, b)
#define DR_ASSERT(cond)
#endif


/*
 *   PUBLIC FUNCTIONS
 * 
 */

// to avoid mistake when casting to pointer on function
#define FUNC_PTR            void (*)(void *)

int libsymb_register(PyObject* dict, const char* libname, const char* symbname,
                     void* handle, FUNC_PTR);
int libsymb_release(PyObject* dict, const char* libname, const char* symbname);
int libsymb_is_known(PyObject* dict, const char* libname, const char* symbname);
void* libsymb_get_handle(PyObject* dict, const char* libname, const char* symbname);
void* libsymb_get_symbol(PyObject* dict, const char* libname, const char* symbname);
void libsymb_apply_on_all(PyObject* dict, FUNC_PTR, int release);

void NULL_FUNCTION();

/*
 *   PRIVATE FUNCTIONS - UTILITIES
 * 
 */

PyObject* _libsymb_to_key(const char* libname, const char* symbname);
void* _libsymb_get_object(PyObject* dict, const char* libname, const char* symbname, int index);

/* FIN DLL_REGISTER_H */
#endif
