/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8NRM2 UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   float R8NRM2(int *n, float *sa, int *incx)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   double r8nrm2_(long *n, float *sa, long *incx)
#elif defined HPUX
   double r8nrm2(long *n, float *sa, long *incx)
#elif defined PPRO_NT
   extern double __stdcall R8NRM2(long *n, double *sa, long *incx)
#endif
{
#if defined CRAY
float rval;
extern float SNRM2(int *n, float *sa, int *incx);
rval = SNRM2(n,sa,incx);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
double rval;
extern double blnrm2_(long *n, float *sa, long *incx);
rval=blnrm2_(n,sa,incx);

#elif defined HPUX
double rval;
extern double blnrm2(long *n, float *sa, long *incx);
rval=blnrm2(n,sa,incx);

#elif defined PPRO_NT
double rval;
extern double __stdcall BLNRM2(long *n,double *sa, long *incx);
rval=BLNRM2(n,sa,incx);

#endif
return(rval);
}
