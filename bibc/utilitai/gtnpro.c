/* ------------------------------------------------------------------*/
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF GTNPRO utilitai  DATE 02/02/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* ------------------------------------------------------------------ */
/* RECUPERE LE NUMERO DU PROCESSEUR COURANT SOUS MPI                  */
/* RETOURNE 0 SI MPI N'A PAS ETE ACTIVEE                              */
/* ------------------------------------------------------------------ */
#include "aster.h"

#ifdef _USE_MPI
#include "mpi.h"
#endif

INTEGER STDCALL(GTNPRO, gtnpro)()
{
   int nproc;
#ifdef _USE_MPI
   MPI_Comm_rank(MPI_COMM_WORLD, &nproc);
#else
   nproc=0;
#endif
   return (INTEGER)nproc;
}
