/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF DGEMX UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
#if defined IRIX || TRU64 || SOLARIS64
 void dgemx_(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
             
 {
   extern void dgemm_(char *tra,char *trb,int *n,int *m,int *k,double *alpha,double *mat,int *lda,
                      double *b,int *ldb,double *beta,double *c,int *ldc,unsigned int *ltra,
                      unsigned int *ltrb);

   int ilda,ildb,ildc;
   int ln,lm,lk;
   double alpha,beta ;
   char transa[2],transb[2];
   unsigned int la,lb;

   ln = *n;
   lm = *m;
   lk = *k;
   ilda = *lda;
   ildb = *ldb;
   ildc = *ldc;
   alpha = -1.;
   beta  = 0.;
   la=lb=1;
   transa[0] = 'N';
   transa[1] = '\0';
   transb[0] = 'N';
   transb[1] = '\0';

   dgemm_(transa,transb,&ln,&lm,&lk,&alpha,mat,&ilda,b,&ildb,&beta,c,&ildc,&la,&lb);
 }
#elif defined CRAY
 void DGEMX(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void BLADMM(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   BLADMM(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined SOLARIS || P_LINUX 
 extern void dgemx_(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 { 
   extern void bladmm_(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   bladmm_(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined HPUX
 extern void dgemx (long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void bladmm(long *n,long *m,long *k,double *mat,long *lda,double *b,long * ldb, double *c,long *ldc);
   bladmm(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined PPRO_NT
 extern void __stdcall DGEMX(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void __stdcall BLADMM(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   BLADMM(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#endif
