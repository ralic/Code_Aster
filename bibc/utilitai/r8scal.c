/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8SCAL UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   void R8SCAL(long *n, float *sa, float *sx, long *incx)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   void r8scal_(long *n, double *sa, double *sx, long *incx)
#elif defined HPUX
   void r8scal(long *n, double *sa, double *sx, long *incx)
#elif defined PPRO_NT
   extern void __stdcall R8SCAL(long *n, double *sa, double *sx, long *incx)
#endif
{
#if defined CRAY
extern void SSCAL(long *n, float *sa, float *sx, long *incx);
SSCAL(n,sa,sx,incx);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
extern void blscal_(long *n, double *sa, double *sx, long *incx);
blscal_(n,sa,sx,incx);

#elif defined HPUX
extern void blscal(long *n, double *sa, double *sx, long *incx);
blscal(n,sa,sx,incx);

#elif defined PPRO_NT
extern void __stdcall BLSCAL(long *n, double *sa, double *sx, long *incx);
BLSCAL(n,sa,sx,incx);

#endif
}
