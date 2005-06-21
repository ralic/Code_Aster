/* ------------------------------------------------------------------*/
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF GTNPRO utilitai  DATE 20/06/2005   AUTEUR BOITEAU O.BOITEAU */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2005  EDF R&D              WWW.CODE-ASTER.ORG */
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
#ifdef MPI
#include "mpi.h"
#endif
#ifdef CRAY
   long GTNPRO()
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long gtnpro_()
#elif defined HPUX
   long gtnpro()
#elif defined PPRO_NT
   long __stdcall GTNPRO()
#endif
{
int nprocs;
#ifdef MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &nprocs);
#else
  nprocs=0;
#endif
return((long) nprocs);
}
