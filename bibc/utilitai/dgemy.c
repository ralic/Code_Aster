/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF DGEMY UTILITAI  DATE 13/01/2003   AUTEUR PABHHHH N.TARDIEU */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2002  EDF R&D              WWW.CODE-ASTER.ORG */
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

#if defined  TRU64 || SGI 
 void dgemy_(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc,double *beta,long *opta, long *optb)
             
 {
   extern void dgemm_(char *tra,char *trb,int *n,int *m,int *k,double *alpha,double *mat,int *lda,
                      double *b,int *ldb,double *beta,double *c,int *ldc,unsigned int *ltra,
                      unsigned int *ltrb);

   int ilda,ildb,ildc;
   int ln,lm,lk;
   double alpha;
   char transa[2],transb[2];
   unsigned int la,lb;

   ln = *n;
   lm = *m;
   lk = *k;
   ilda = *lda;
   ildb = *ldb;
   ildc = *ldc;
   alpha = -1.;
   /*   BETA  = 0.; PASSÈ EN PARAMËTRE 
      ZERO POUR LA FACTORISATION 1 POUR DESC/ REM*/
   la=lb=1;
   transa[0] = 'T';
   transb[0] = 'T';
   if (*opta == 1) transa[0] = 'N';
   if (*optb == 1) transb[0] = 'N';
   transa[1] = '\0';
   transb[1] = '\0';
   dgemm_(transa,transb,&ln,&lm,&lk,&alpha,mat,&ilda,b,&ildb,beta,c,&ildc,&la,&lb);
 }
#elif defined CRAY
 void DGEMY(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc,double *beta,long *opta, long *optb)
 {
   extern void BLADMN(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc,double *beta,long *opta,long *optb);
   BLADMN(n,m,k,mat,lda,b,ldb,c,ldc,beta,opta,optb);
 }
#elif defined SOLARIS || P_LINUX 
 extern void dgemy_(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc,double *beta,long *opta, long *optb)
{ 
   extern void bladmn_(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc,double *beta,long *opta,long *optb);
   bladmn_(n,m,k,mat,lda,b,ldb,c,ldc,beta,opta,optb);
 }
#elif defined HPUX
 extern void dgemy (long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc,double *beta,long *opta, long *optb)
 {
   extern void bladmn(long *n,long *m,long *k,double *mat,long *lda,double *b,long * ldb, double *c,long *ldc,double *beta, long *opta,long *optb);
   bladmn(n,m,k,mat,lda,b,ldb,c,ldc,beta,opta,optb);
 }
#elif defined PPRO_NT
 extern void __stdcall DGEMY(long  *n, long *m, long  *k, double *mat, long *lda, double *b, long *ldb, double *c,long *ldc,double *beta,long *opta, long *optb)
 {
   extern void __stdcall BLADMN(long *n,long *m,long *k,double *mat,long *lda,double *b,long *ldb, double *c,long *ldc,double *beta,long *opta,long *optb);
   BLADMN(n,m,k,mat,lda,b,ldb,c,ldc,beta,opta,optb);
 }
#endif
