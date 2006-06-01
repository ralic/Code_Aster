/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF SSPMVA UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
   void SSPMVA(long *n, long *p, float *front, long *ad, float *t1, float *t2)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
   void sspmva_(long *n, long *p, float *front, long *ad, float *t1, float *t2)
#elif defined HPUX
   void sspmva(long *n, long *p, float *front, long *ad, float *t1, float *t2)
#elif defined PPRO_NT
   extern void __stdcall SSPMVA (long *n, long *p, float *front, long *ad, float *t1, float *t2)
#endif
{
#if defined CRAY
    SSPMVA@ (n,p,front,ad,t1,t2);

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
    extern void  sspmvb_ (long *n,long *p,float *front,long *ad,float *t1,float *t2);
    sspmvb_ (n,p,front,ad,t1,t2);

#elif defined HPUX
    extern void sspmvb (long *n,long *p,float *front,long *ad,float *t1,float *t2);
    sspmvb (n,p,front,ad,t1,t2);

#elif defined PPRO_NT
    extern void __stdcall SSPMVB (long *n,long *p,float *front,long *ad,float *t1,float *t2);
     SSPMVB (n,p,front,ad,t1,t2);

#endif
}
