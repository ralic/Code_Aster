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

#ifndef ASTERC_DEBUG_H
#define ASTERC_DEBUG_H

#include "Python.h"
#include <stdlib.h>
#include <stdio.h>

/*! Here are defined some flags to add debugging informations.

If the flag is defined, a function prints informations on stdout.
If the flag is not defined, the function must be empty macro.

to enable DEBUG_ASSERT
#define __DEBUG_ASSERT__

to add all traces
#define __DEBUG_ALL__
*/

/*! print the filename and line where the error occurred */
#define DEBUG_LOC       fprintf(stdout, "DEBUG: %s #%d: ", __FILE__, __LINE__); fflush(stdout);

/*! interrupt the execution, return SIGABRT */
#define INTERRUPT(code) { DEBUG_LOC; fprintf(stdout,"ABORT - exit code %d\n",code); \
            fflush(stdout); abort(); }

/*! internal utility to print a PyObject */
#define PYDBG(label, pyobj) { DEBUG_LOC; fprintf(stdout, label); \
            PyObject_Print(pyobj, stdout, 0); \
            printf("\n"); fflush(stdout); }

#define DBG(label)          { DEBUG_LOC; printf(label); printf("\n"); fflush(stdout); }
#define DBGV(fmt, a)        { DEBUG_LOC; printf(fmt, a); printf("\n"); fflush(stdout); }
#define DBGVV(fmt, a, b)    { DEBUG_LOC; printf(fmt, a, b); printf("\n"); fflush(stdout); }



/*! enable DEBUG_ASSERT */
#if defined(__DEBUG_ASSERT__) || defined(__DEBUG_ALL__)
#   define DEBUG_ASSERT(cond)  AS_ASSERT(cond)
#else
#   define DEBUG_ASSERT(cond)
#endif

/*! enable DEBUG_DLL */
#if defined(__DEBUG_DLL__) || defined(__DEBUG_ALL__)
#   define DEBUG_DLL_PYOB(label, pyobj)  PYDBG(label, pyobj)
#   define DEBUG_DLL_VV(fmt, a, b)  DBGVV(fmt, a, b)
#else
#   define DEBUG_DLL_PYOB(label, pyobj)
#   define DEBUG_DLL_VV(fmt, a, b)
#endif

/*! enable DEBUG_EXCEPT, not in __DEBUG_ALL__ */
#if defined(__DEBUG_EXCEPT__)
#   define DEBUG_EXCEPT(fmt, a)  DBGV(fmt, a)
#else
#   define DEBUG_EXCEPT(fmt, a)
#endif

/*! debug MPI communicator as aster_comm_t */
#if defined(__DEBUG_MPICOM__) || defined(__DEBUG_ALL__)
    #define COMM_DEBUG(c) { DEBUG_LOC; \
            printf("%-8s #%d (%d/@", (c).name, (int)MPI_Comm_c2f((c).id), (c).level); \
            if ((c).parent) { printf("%-8s", (c).parent->name); } \
            else { printf("        "); } \
            printf(")\n"); fflush(stdout); }
#else
    #define COMM_DEBUG(c)
#endif

/*! debug MPI communications */
#if defined(__DEBUG_MPI__) || defined(__DEBUG_ALL__)
    #define DEBUG_MPI(fmt, a, b) DBGVV(fmt, a, b)
#else
    #define DEBUG_MPI(fmt, a, b)
#endif

/*! enable DEBUG_ASTER_FONCTIONS */

#endif
