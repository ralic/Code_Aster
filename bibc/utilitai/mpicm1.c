/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MPICM1 UTILITAI  DATE 24/11/2008   AUTEUR PELLET J.PELLET */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2008  EDF R&D              WWW.CODE-ASTER.ORG */
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

#include "aster.h"


void DEFSSPPP(MPICM1, mpicm1,
                   _IN char *optmpi, STRING_SIZE loptmpi,
                   _IN char *typsca, STRING_SIZE ltypsca,
                   _IN INTEGER *nbv, _INOUT INTEGER *vi, _INOUT DOUBLE *vr)

{
#if defined _USE_MPI
   void DEFSSPPP(MPICM1A, mpicm1a, char *, STRING_SIZE, char *, STRING_SIZE, INTEGER *, INTEGER *, DOUBLE *);
   #define CALL_MPICM1A(a,b,c,d,e) CALLSSPPP(MPICM1A,mpicm1a,a,b,c,d,e)

   CALL_MPICM1A(optmpi, typsca, nbv, vi, vr);


#endif
}
