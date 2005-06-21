/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF FETMPI UTILITAI  DATE 20/06/2005   AUTEUR BOITEAU O.BOITEAU */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2005  EDF R&D              WWW.CODE-ASTER.ORG */
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
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64
  void fetmpi_(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
             char *k124,char *k224,char *k324,float *argr1,unsigned long lk124,unsigned long lk224,unsigned long lk324)
#elif defined HPUX
  void fetmpi(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
             char *k124,char *k224,char *k324,float *argr1,unsigned long lk124,unsigned long lk224,unsigned long lk324)
#elif defined PPRO_NT
  extern void __stdcall FETMPI(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
             char *k124,unsigned long lk124,char *k224,unsigned long lk224,char *k324,unsigned long lk324,float *argr1)
#endif
{
#if defined MPI_FETI
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64
  extern void fetam_(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
                   char *k124,char *k224,char *k324,float *argr1,unsigned long lk124,unsigned long lk224,unsigned long lk324);
  fetam_(opt,nbsd,ifm,niv,rang,nbproc,k124,k224,k324,argr1,lk124,lk224,lk324);
#elif defined HPUX
  extern void fetam(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
                   char *k124,char *k224,char *k324,float *argr1,unsigned long lk124,unsigned long lk224,unsigned long lk324);
  fetam(opt,nbsd,ifm,niv,rang,nbproc,k124,k224,k324,argr1,lk124,lk224,lk324);
#elif defined PPRO_NT
  extern void __stdcall FETAM(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc,
                   char *k124,unsigned long lk124,char *k224,unsigned long lk224,char *k324,unsigned long lk324,float *argr1);
  FETAM(opt,nbsd,ifm,niv,rang,nbproc,k124,lk124,k224,lk224,k324,lk324,argr1);
#endif
#else
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64
  extern void fetsm_(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc);
  fetsm_(opt,nbsd,ifm,niv,rang,nbproc);
#elif defined HPUX
  extern void fetsm(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc);
  fetsm(opt,nbsd,ifm,niv,rang,nbproc);
#elif defined PPRO_NT
  extern void __stdcall FETSM(long *opt,long *nbsd,long *ifm,long *niv,long *rang,long *nbproc);
  FETSM(opt,nbsd,ifm,niv,rang,nbproc);
#endif
#endif
}
