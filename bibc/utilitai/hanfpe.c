/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF hanfpe utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
/*
**  Fonction C intermediaire pour appeler une routine FORTRAN
**  qui va faire appel a UTMESS('F',...)
**  Il n'y a pas de passage d'argument pour minimiser les problemes
**  d'interfacage FORTRAN/C et reciproquement
*/
#if defined CRAY || SOLARIS || HPUX|| IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <stdio.h>
#include <stdlib.h>
#endif

#if defined CRAY || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
   void hanfpe (int sig)
#elif defined SOLARIS
#include <siginfo.h>
#include <ucontext.h>
   void hanfpe (int sig, siginfo_t *sip, ucontext_t *uap)
#elif defined PPRO_NT
   void hanfpe (int sig)
#endif
{
void exit (int status) ;
#ifdef CRAY
   UTMFPE();
   exit(sig);
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   utmfpe_();
   exit(sig);
#elif defined HPUX
   utmfpe();
   exit(sig);
#elif defined PPRO_NT
   extern void __stdcall UTMFPE(void);
   UTMFPE();
   exit(sig);
#endif
}
