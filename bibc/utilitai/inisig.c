/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF inisig utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
** Initialisation de l'iterception de certain signaux
** Actuellement sont traites les signaux :
**    CPULIM  : plus de temps CPU
**    FPE     : Floating point exception
*/
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
extern int errno;
void hancpu (int sig);

#if defined CRAY || HPUX || P_LINUX || TRU64  || SOLARIS64
  void hanfpe (int sig);
  void stptrap ( int sig) ;
#elif defined SOLARIS
/*#include <sunmath.h>*/
#include <siginfo.h>
#include <ucontext.h>
  void hanfpe(int sig, siginfo_t *sip, ucontext_t *uap);
#elif defined IRIX 
#include <siginfo.h>
#include <sigfpe.h>
#include <limits.h>
  void stptrap ( int sig) ;
#elif defined PPRO_NT
#include <float.h>
  void  hanfpe (int sig);
  void stptrap(int sig);
#endif

#ifdef CRAY
  void INISIG( void )
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
  void inisig_( void )
#elif defined HPUX
  void inisig( void )
#elif defined PPRO_NT
  extern void __stdcall INISIG( void )
#endif
{
int ier;
#if defined CRAY || SOLARIS || IRIX || TRU64 || SOLARIS64 
  struct sigaction action_CPU_LIM;
#endif
#if defined IRIX
  extern struct sigfpe_template sigfpe_[_N_EXCEPTION_TYPES+1];
  extern int inifpe(void);
#endif
#ifdef CRAY
  struct sigaction action_FPE;
#endif
/*            */
/* CPU LIMITE */
/*            */
#if defined CRAY || SOLARIS || IRIX || TRU64 || SOLARIS64 
  action_CPU_LIM.sa_handler=hancpu;
  sigemptyset(&action_CPU_LIM.sa_mask);
  action_CPU_LIM.sa_flags=0;
#endif

#ifdef CRAY
   sigaction(SIGCPULIM,&action_CPU_LIM,NULL);
#elif defined SOLARIS || IRIX || TRU64 || SOLARIS64 
   sigaction(SIGXCPU  ,&action_CPU_LIM,NULL);
#endif

/*                          */
/* Floating point exception */
/*                          */
#ifdef CRAY
   action_FPE.sa_handler=hanfpe;
   sigemptyset(&action_FPE.sa_mask);
   action_FPE.sa_flags=0;
   sigaction(SIGFPE,&action_FPE,NULL);
#elif defined SOLARIS
   ieee_handler("set","common",hanfpe);
   ieee_handler("clear","invalid",hanfpe);
#elif defined IRIX
   ier=inifpe();
#elif defined HPUX || P_LINUX || TRU64  || SOLARIS64
   signal(SIGFPE,  hanfpe);
#elif defined PPRO_NT
#define _EXC_MASK  _EM_INEXACT + _EM_UNDERFLOW
   unsigned int  _controlfp (unsigned int new, unsigned int mask);
   _controlfp(_EXC_MASK,_MCW_EM);
   signal(SIGFPE,  hanfpe);
#endif

/*                          */
/* Arret par CRTL C         */
/*                          */
#if defined PPRO_NT || IRIX || TRU64 
   signal(SIGINT,  stptrap);
#endif
}
void stptrap (int sig)
{
  printf(" \n arret sur CTRL C \n");
  exit(1);
}
