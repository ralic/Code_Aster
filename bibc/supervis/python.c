/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF python supervis  DATE 21/05/2012   AUTEUR COURTOIS M.COURTOIS */
/* RESPONSABLE LEFEBVRE J-P.LEFEBVRE */
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
/* Minimal main program -- everything is loaded from the library */

#include "Python.h"
#ifdef _USE_MPI
#include "mpi.h"
#endif

extern DL_EXPORT(int) Py_Main();
extern void initaster();
extern void initaster_core();
extern void initaster_fonctions();
extern void initmed_fonctions();

#ifdef _USE_MPI
void terminate(){
  printf("Fin interpreteur Python\n");
  MPI_Finalize();
}
#endif

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
    atexit(terminate);
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
