/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8AXPY UTILITAI  DATE 16/12/2002   AUTEUR ROSE C.ROSE */
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
   void R8AXPY(long *n, float *sa, float *sx, long *incx, float *sy, long *incy)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   void r8axpy_(long *n, float *sa, double *sx, long *incx, double *sy, long *incy)
#elif defined HPUX
   void r8axpy(long *n, float *sa, double *sx, long *incx, double *sy, long *incy)
#elif defined PPRO_NT
   extern void __stdcall R8AXPY(long *n, double *sa, double *sx, long *incx, double *sy, long *incy)
#endif
{
#if defined CRAY
extern void SAXPY(long *n, float *sa, float *sx, long *incx, float *sy, long *incy);
SAXPY(n,sa,sx,incx,sy,incy);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
extern void daxpy_(long *n, float *sa, double *sx, long *incx, double *sy, long *incy);
daxpy_(n,sa,sx,incx,sy,incy);

#elif defined HPUX
extern void blaxpy(long *n, float *sa, double *sx, long *incx, double *sy, long *incy);blaxpy(n,sa,sx,incx,sy,incy);

#elif defined PPRO_NT
extern void __stdcall BLAXPY(long *n,double *sa, double *sx, long *incx, double *sy, long *incy);
BLAXPY(n,sa,sx,incx,sy,incy);

#endif
}
