/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF FETMPI UTILITAI  DATE 30/09/2008   AUTEUR COURTOIS M.COURTOIS */
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
            INTEGER *opt, INTEGER *nbsd, INTEGER *ifm,
            INTEGER *niv, INTEGER *rang, INTEGER *nbproc,
            char *k124, STRING_SIZE lk124,
            char *k224, STRING_SIZE lk224,
            char *k324, STRING_SIZE lk324,
            DOUBLE *argr1)
{
#if defined _USE_MPI_FETI
   void DEFPPPPPPSSSP(FETAM, fetam, INTEGER *, INTEGER *, INTEGER *, INTEGER *, INTEGER *, INTEGER *,
            char *, STRING_SIZE, char *, STRING_SIZE, char *, STRING_SIZE, DOUBLE *);
   #define CALL_FETAM(a,b,c,d,e,f,s1,s2,s3,g) CALLPPPPPPSSSP(FETAM,fetam,a,b,c,d,e,f,s1,s2,s3,g)

   CALL_FETAM(opt, nbsd, ifm, niv, rang, nbproc, k124, k224, k324, argr1);

#else
   void DEFPPPPPP(FETSM, fetsm, INTEGER *, INTEGER *, INTEGER *, INTEGER *, INTEGER *, INTEGER *);
   #define CALL_FETSM(a,b,c,d,e,f) CALLPPPPPP(FETSM,fetsm,a,b,c,d,e,f)

   CALL_FETSM(opt, nbsd, ifm, niv, rang, nbproc);

#endif
}
