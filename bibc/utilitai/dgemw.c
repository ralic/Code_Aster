/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF DGEMW UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
 void dgemw_(long *n, long  *k, double *mat, long *lda, double *trav, double *y)
 {
   extern void dgemv_(char *trans,int *n,int *k,double *alpha,double *mat,int *lda,
                      double *trav,int *incx,double *beta,double *y,int *incy,
                      unsigned int *ltr);

   int incx,incy,ilda;
   int ln,lk;
   double alpha,beta;
   char trans[2];
   unsigned int l;

   ln = *n;
   lk = *k;
   incx = 1; 
   incy = 1;
   ilda = *lda;
   alpha = -1.;
   beta  =  1.;
   l=1;
   trans[0] = 'N';
   trans[1] = '\0';

   dgemv_(&trans[0],&ln,&lk,&alpha,mat,&ilda,trav,&incx,&beta,y,&incy,&l);
 }
#elif defined CRAY
 void DGEMW(long  *n, long  *k, double *mat, long *lda, double *trav, double *y)
 {
   extern void BLADMV(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   BLADMV(n,k,mat,lda,trav,y);
 }
#elif defined SOLARIS || P_LINUX 
 extern void dgemw_(long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void bladmv_(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   bladmv_(n,k,mat,lda,trav,y);
 }
#elif defined HPUX
 extern void dgemw (long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void bladmv(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   bladmv(n,k,mat,lda,trav,y);
 }
#elif defined PPRO_NT
 extern void __stdcall DGEMW(long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void __stdcall BLADMV(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   BLADMV(n,k,mat,lda,trav,y);
 }
#endif
