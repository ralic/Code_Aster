/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF FETMPI UTILITAI  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS */
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
#include "aster.h"


void DEFPPPPPPSSSP(FETMPI, fetmpi,
            long *opt, long *nbsd, long *ifm, long *niv, long *rang, long *nbproc,
            char *k124, int lk124,char *k224, int lk224, char *k324, int lk324,
            float *argr1)
{
#if defined _USE_MPI_FETI
   void DEFPPPPPPSSSP(FETAM, fetam, long *, long *, long *, long *, long *, long *,
            char *, int, char *, int, char *, int, float *);
   #define CALL_FETAM(a,b,c,d,e,f,s1,s2,s3,g) CALLPPPPPPSSSP(FETAM,fetam,a,b,c,d,e,f,s1,s2,s3,g)

   CALL_FETAM(opt, nbsd, ifm, niv, rang, nbproc, k124, k224, k324, argr1);

#else
   void DEFPPPPPP(FETSM, fetsm, long *, long *, long *, long *, long *, long *);
   #define CALL_FETSM(a,b,c,d,e,f) CALLPPPPPP(FETSM,fetsm,a,b,c,d,e,f)

   CALL_FETSM(opt, nbsd, ifm, niv, rang, nbproc);

#endif
}
