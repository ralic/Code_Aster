/* ------------------------------------------------------------------ */
/* person_in_charge: j-pierre.lefebvre at edf.fr */
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
/* Minimal main program -- everything is loaded from the library */

/* NOTE: 
 *  Since Python may define some pre-processor definitions which affect the
 *  standard headers on some systems, you must include "Python.h" before any
 *  standard headers are included.
 *  The warning on _POSIX_C_SOURCE redefinition must not occur.
 * 
 *  source: http://docs.python.org/c-api/intro.html
 */
#include "Python.h"

#ifdef _USE_MPI
#   include <stdio.h>
#   include <stdlib.h>
#   include "mpi.h"
#endif

#include "aster_error.h"

extern DL_EXPORT(int) Py_Main();
extern void initaster();
extern void initaster_core();
extern void initaster_fonctions();
extern void initmed_fonctions();

#ifndef _MAIN_
#define _MAIN_ main
#endif

int
_MAIN_(argc, argv)
    int argc;
    char **argv;
{
    int ierr;
#ifdef _USE_MPI
    int rc;
    rc = MPI_Init(&argc,&argv);
    if (rc != MPI_SUCCESS) {
         fprintf(stderr, "MPI Initialization failed: error code %d\n",rc);
         abort();
    }
    /* terminate is defined in aster_error */
    ierr = atexit(terminate);
    if (ierr != 0) {
        fprintf(stderr, "cannot set exit function\n");
        exit(EXIT_FAILURE);
    }
#endif
    PyImport_AppendInittab("aster_core",initaster_core);
    PyImport_AppendInittab("aster",initaster);

    /* Module définissant des opérations sur les objets fonction_sdaster */
    PyImport_AppendInittab("aster_fonctions",initaster_fonctions);
#ifndef _DISABLE_MED
    PyImport_AppendInittab("med_aster",initmed_fonctions);
#endif
    ierr= Py_Main(argc, argv);
    return ierr;
}
