/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF inisig utilitai  DATE 19/05/2011   AUTEUR SELLENET N.SELLENET */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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
** Initialisation de l'interception de certains signaux
** Actuellement sont traites les signaux :
**    CPULIM  : plus de temps CPU
**    FPE     : Floating point exception
*/

#include "aster.h"
#include "aster_fort.h"

#include <signal.h>

void abort();

void hancpu (int sig);

#if defined SOLARIS
#include <siginfo.h>
#include <ucontext.h>
  void hanfpe(int sig, siginfo_t *sip, ucontext_t *uap);

#elif defined IRIX 
#include <siginfo.h>
#include <sigfpe.h>
#include <limits.h>
  void stptrap(int sig);
  void interrupt_sigsegv(int sig);

#elif defined _WINDOWS
#include <float.h>
  void hanfpe(int sig);
  void stptrap(int sig);

#elif defined _POSIX
  void hanfpe(int sig);
  void stptrap(int sig);
  void stpusr1(int sig);
#endif

#if defined GNU_LINUX
#define _GNU_SOURCE 1
#include <fenv.h>
#endif


void STDCALL(INISIG, inisig)()
{
#if defined _POSIX
   struct sigaction action_CPU_LIM;
#endif
#if defined IRIX
   int ier;
   extern struct sigfpe_template sigfpe_[_N_EXCEPTION_TYPES+1];
   extern int inifpe(void);
#endif
#if defined _WINDOWS
    unsigned int cw, cwOrig;
#endif

/*            */
/* CPU LIMITE */
/*            */
#if defined _POSIX
   action_CPU_LIM.sa_handler=hancpu;
   sigemptyset(&action_CPU_LIM.sa_mask);
   action_CPU_LIM.sa_flags=0;
   sigaction(SIGXCPU  ,&action_CPU_LIM,NULL);
#endif

/*                          */
/* Floating point exception */
/*                          */
#if defined SOLARIS
   ieee_handler("set","common",hanfpe);
   ieee_handler("clear","invalid",hanfpe);
#elif defined IRIX
   ier=inifpe();

#elif defined GNU_LINUX

   /* Enable some exceptions. At startup all exceptions are masked. */
   feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);

   signal(SIGFPE,  hanfpe);

#elif defined _WINDOWS
    _clearfp();
    cw = _controlfp(0, 0);
    cw &=~( EM_OVERFLOW | EM_ZERODIVIDE );
    cwOrig = _controlfp(cw, MCW_EM);

    signal(SIGFPE, hanfpe);
#else
   signal(SIGFPE,  hanfpe);
#endif

/*                          */
/* Arret par CRTL C         */
/*                          */
   signal(SIGINT,  stptrap);

/*                          */
/* Arret par SIGUSR1        */
/*                          */
/* Note : l'arret par SIGUSR1 ne fonctionne pas sous MSVC,
   il faudra essayer de trouver autre chose... */
#if defined _POSIX
   signal(SIGUSR1,  stpusr1);
#endif
}


void stptrap (int sig)
{
  CALL_U2MESS("I", "SUPERVIS_97");
  exit(EXIT_FAILURE);
}


static INTEGER status_usr1 = 0;

INTEGER F_FUNC(ETAUSR, etausr)()
{
    /* ETAt USR1 :
     * Retourne la variable status_usr1 */
    return status_usr1;
}

void stpusr1 (int sig)
{
    /* SToP USR1 :
     * callback appelé lors de la réception du signal USR1.
     */
    CALL_U2MESS("I", "SUPERVIS_96");
    status_usr1 = (INTEGER)1;
}

void STDCALL(CLRUSR, clrusr)()
{
    /* CLeaR USR1 :
     * Réinitialise la valeur de status_usr1
     * Utile pour éviter la récursivité.
     */
   status_usr1 = (INTEGER)0;
}

