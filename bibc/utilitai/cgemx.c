/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF CGEMX UTILITAI  DATE 28/06/2004   AUTEUR ROSE C.ROSE */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2004  EDF R&D              WWW.CODE-ASTER.ORG */
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
typedef struct { double real, imag; } Complex;
 void cgemx_(long  *n, long *m, long  *k, Complex *mat, long *lda,Complex *b, long *ldb, Complex *c,long *ldc)
             
 {
   extern void zgemm_(char *tra,char *trb,int *n,int *m,int *k,Complex *alpha,Complex *mat,int *lda,
                      Complex *b,int *ldb,Complex *beta,Complex *c,int *ldc,unsigned int *ltra,
                      unsigned int *ltrb);

   int ilda,ildb,ildc;
   int ln,lm,lk;
   Complex alpha,beta ;
   char transa[2],transb[2];
   unsigned int la,lb;

   ln = *n;
   lm = *m;
   lk = *k;
   ilda = *lda;
   ildb = *ldb;
   ildc = *ldc;
   alpha.real = -1.; alpha.imag = 0.;
   beta.real  =  0.;  beta.imag = 0.;
   la=lb=1;
   transa[0] = 'N';
   transa[1] = '\0';
   transb[0] = 'N';
   transb[1] = '\0';

   zgemm_(transa,transb,&ln,&lm,&lk,&alpha,mat,&ilda,b,&ildb,&beta,c,&ildc,&la,&lb);
 }
#elif defined CRAY
 void CGEMX(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void BLACMM(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   BLACMM(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined SOLARIS || P_LINUX 
 extern void cgemx_(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 { 
   extern void blacmm_(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   blacmm_(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined HPUX
 extern void cgemx (long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void blacmm(long *n,long *m,long *k,double *mat,long *lda,double *b,long * ldb, double *c,long *ldc);
   blacmm(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#elif defined PPRO_NT
 extern void __stdcall CGEMX(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc)
 {
   extern void __stdcall BLACMM(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc);
   BLACMM(n,m,k,mat,lda,b,ldb,c,ldc);
 }
#endif
