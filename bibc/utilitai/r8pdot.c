/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF R8PDOT UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   float R8PDOT(long *n, float *sy, long *indx, float *sx)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   double r8pdot_(long *n, double *sy, long *indx, double *sx)
#elif defined HPUX
   double r8pdot(long *n, double *sy, long *indx, double *sx)
#elif defined PPRO_NT
   extern double __stdcall R8PDOT(long *n, double *sy, long *indx, double *sx)
#endif
{
#if defined CRAY
float rval;
extern float SPDOT(long *n, float *sy, long *indx, float *sx);
rval=SPDOT(n,sy,indx,sx);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
double rval;
extern double blpdot_(long *n, double *sy, long *indx, double *sx);
rval=blpdot_(n,sy,indx,sx);

#elif defined HPUX
double rval;
extern double blpdot(long *n, double *sy, long *indx, double *sx);
rval=blpdot(n,sy,indx,sx);

#elif defined PPRO_NT
double rval;
extern double __stdcall BLPDOT(long *n, double *sy, long *indx, double *sx);
rval=BLPDOT(n,sy,indx,sx);

#endif
return (rval);
}
