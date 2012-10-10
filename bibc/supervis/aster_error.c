/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_error supervis  DATE 10/10/2012   AUTEUR COURTOIS M.COURTOIS */
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
/* RESPONSABLE COURTOIS M.COURTOIS */

#include "aster.h"

#include <stdio.h>
#include <stdlib.h>

#ifdef _USE_MPI
#   include "mpi.h"
#endif

#include "aster_fort.h"

/*
 * Define a dedicated function to abort a Code_Aster execution.
 */
int gErrFlg = 0;

void DEFP( ASABRT, asabrt, _IN INTEGER *iret )
{
    /* Function to interrupt the execution.
     * - In a sequential version, it just calls abort().
     * - In a MPI execution, it set a global flag and calls `MPI_Abort`.
     * 
     * The usage of atexit seems required in a Python interpreter
     * certainly because `sys.exit()` probably calls the `exit` system
     * function (so we can't add a `MPI_Finalize` call before exiting).
     * But if `MPI_Finalize` is executed after a `MPI_Abort` call, all
     * the processes are not interrupted.
     * That's why a global flag is used to by-pass `MPI_Finalize` in
     * case of error.
     * 
     * to test MPI_Abort : http://www.netlib.org/blacs/blacs_errata.html
     */
    gErrFlg = 1;
#ifdef _USE_MPI
    MPI_Abort( MPI_COMM_WORLD, (int)(*iret) );
#else
    CALL_ABORTF();
#endif
    return;
}

void terminate( void )
{
    /* Function registered using atexit() in main.
     */
    printf("End of the Code_Aster execution");
#ifdef _USE_MPI
    if ( gErrFlg == 0 ) {
        printf(" - MPI exits normally\n");
        MPI_Finalize();
    } else {
        printf(" - MPI exits with errors\n");
    }
#endif
    printf("\n");
}

