/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8GEMV UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2001  EDF R&D              WWW.CODE-ASTER.ORG */
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

#ifdef CRAY
#include <fortran.h>
   void R8GEMV(_fcd transF, long *m,long *n,
                float*alpha,float *a,long *lda,float *sx,
                long *incx, float *beta,float *sy,long *incy,long ltrans)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   void r8gemv_(char *trans, long *m,long *n,
                double *alpha,double *a,long *lda,double *sx,
                long *incx, double *beta, double *sy, long *incy,long ltrans)
#elif defined HPUX
   void r8gemv(char *trans, long *m,long *n,
                double *alpha,double *a,long *lda,double *sx,
                long *incx, double *beta, double *sy, long *incy,long ltrans)
#elif defined PPRO_NT
   extern void __stdcall R8GEMV(char *trans, unsigned long ltrans,long *m,long *n,
                                double *alpha,double *a,long *lda,double *sx,
                                long *incx, double *beta, double *sy, long *incy)
#endif
{
#if defined CRAY
char *trans;
extern void SGEMV(char *trans, long *m,long *n,
                  float*alpha,float *a,long *lda,float *sx,
                  long *incx, float *beta,float *sy,long *incy);
trans = _fcdtocp(transF);
SGEMV(trans,m,n,alpha,a,lda,sx,incx,beta,sy,incy);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
extern void r8gemv_(char *trans, long *m,long *n,
                    double *alpha,double *a,long *lda,double *sx,
                    long *incx, double *beta, double *sy, long *incy,long ltrans);
blgemv_(trans,m,n,alpha,a,lda,sx,incx,beta,sy,incy,ltrans);

#elif defined HPUX
extern void r8gemv(char *trans, long *m,long *n,
                    double *alpha,double *a,long *lda,double *sx,
                    long *incx, double *beta, double *sy, long *incy,long ltrans);
blgemv(trans,m,n,alpha,a,lda,sx,incx,beta,sy,incy,ltrans);

#elif defined PPRO_NT
extern void __stdcall BLGEMV(char *trans,unsigned long ltrans,long *m,long *n,
                            double *alpha,double *a,long *lda,double *sx,
                            long *incx,double *beta,double *sy,long *incy);
BLGEMV(trans,ltrans,m,n,alpha,a,lda,sx,incx,beta,sy,incy);

#endif
}
