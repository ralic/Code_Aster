/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8COPY UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   void R8COPY(long *n, float *sx, long *incx, float *sy, long *incy)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   void r8copy_(long *n, double *sx, long *incx, double *sy, long *incy)
#elif defined HPUX
   void r8copy(long *n, double *sx, long *incx, double *sy, long *incy)
#elif defined PPRO_NT
   extern void __stdcall R8COPY(long *n, double *sx, long *incx, double *sy, long *incy)
#endif
{
#if defined CRAY
extern void SCOPY(long *n, float *sx, long *incx, float *sy, long *incy);
SCOPY(n,sx,incx,sy,incy);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
extern void blcopy_(long *n, double *sx, long *incx, double *sy, long *incy);
blcopy_(n,sx,incx,sy,incy);

#elif defined HPUX
extern void blcopy(long *n, double *sx, long *incx, double *sy, long *incy);
blcopy(n,sx,incx,sy,incy);

#elif defined PPRO_NT
extern void __stdcall BLCOPY(long *n, double *sx, long *incx, double *sy, long *incy);
BLCOPY(n,sx,incx,sy,incy);

#endif
}
