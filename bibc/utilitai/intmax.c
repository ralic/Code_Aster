/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF INTMAX UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
/* ------------------------------------------------------------------ */
#ifdef CRAY
/*    On ne fait rien : on utilise la fonction INTMAX systeme */
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
   long intmax_(long *n, long *dx, long *incx)
#elif defined HPUX
   long intmax(long *n, long *dx, long *incx)
#elif defined PPRO_NT
   extern long __stdcall INTMAX (long *n, long *dx, long *incx)
#endif
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
{
extern long blimax_(long *n, long *dx, long *incx);
long v;
v=blimax_(n,dx,incx);
return(v);
}
#elif defined HPUX
{
extern long blimax(long *n, long *dx, long *incx);
long v;
v=blimax(n,dx,incx);
return(v);
}

#elif defined PPRO_NT
{
long v;
extern long __stdcall BLIMAX(long *n, long *dx, long *incx);
v=BLIMAX(n,dx,incx);
return(v);
}
#endif
