/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF CGEMW UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
#ifdef IRIX 
#include <cblas.h>
#endif
#if defined IRIX || TRU64 || SOLARIS64
typedef struct { double real, imag; } Complex;
void cgemw_(long  *n, long  *k, Complex *mat, long *lda, Complex *trav, Complex *y)
 {
   extern void zgemv_(char *trans,int *n,int *k,Complex *alpha,Complex *mat,int *lda,
                      Complex *trav,int *incx,Complex *beta,Complex *y,int *incy,
                      unsigned int *ltr);
   int incx,incy,ilda;
   int ln,lk;
   Complex alpha,beta;
   char trans[2];
   unsigned int l;

   ln = *n;
   lk = *k;
   incx = 1; 
   incy = 1;
   ilda = *lda;
   alpha.real = -1.; alpha.imag = 0.;
   beta.real  =  1.;  beta.imag = 0.;
   l=1;
   trans[0] = 'N';
   trans[1] = '\0';
 
   zgemv_(trans,&ln,&lk,&alpha,mat,&ilda,trav,&incx,&beta,y,&incy,&l);
 }
#elif defined CRAY
 void CGEMW(long  *n, long  *k, double *mat, long *lda, double *trav, double *y)
 {
   extern void BLACMV(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   BLACMV(n,k,mat,lda,trav,y);
 }
#elif defined SOLARIS || P_LINUX   
 extern void cgemw_(long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void blacmv_(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   blacmv_(n,k,mat,lda,trav,y);
 }
#elif defined HPUX
 extern void cgemw (long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void blacmv(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   blacmv(n,k,mat,lda,trav,y);
 }
#elif defined PPRO_NT
extern void __stdcall CGEMW(long *n,long *k,double *mat,long *lda,double *trav,double *y)
 {
   extern void __stdcall BLACMV(long *n,long *k,double *mat,long *lda,double *trav,double *y);
   BLACMV(n,k,mat,lda,trav,y);
 }
#endif

