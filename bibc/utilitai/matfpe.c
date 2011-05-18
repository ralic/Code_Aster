/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF matfpe utilitai  DATE 19/05/2011   AUTEUR SELLENET N.SELLENET */
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
/*
   Cette fonction permet de désactiver temporairement la levée d'exceptions
   autour des appels des fonctions des librairies mathématiques blas et
   lapack.
      CALL MATFPE(-1) : on désactive la levée d'exceptions
      CALL MATFPE(1) : on active la levée d'exceptions
   
   Problème rencontré sur Linux ia64 avec MKL 8.0.
*/
#include "aster.h"
#include <stdio.h>

#if defined _DISABLE_MATHLIB_FPE
#include <signal.h>
#define _GNU_SOURCE 1
#include <fenv.h>
void hanfpe (int sig);

static int compteur_fpe = 1;
#endif

void DEFP(MATFPE, matfpe, INTEGER *enable)
{
#if defined _DISABLE_MATHLIB_FPE
   
   /* permet juste de vérifier où on en est si besoin ! */
   if (*enable == 0) {
      printf("#MATFPE var = %ld (compteur %d)\n", *enable, compteur_fpe);
      return;
   }
   
   compteur_fpe = compteur_fpe + *enable;
   
   if (compteur_fpe < 1) {
      fedisableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
      /* définition du handler : hanfpe appelle UTMFPE qui fait UTMESS('F') */
      signal(SIGFPE, hanfpe);
   }
   else if (compteur_fpe >= 1) {
      feenableexcept(FE_DIVBYZERO|FE_OVERFLOW|FE_INVALID);
      /* définition du handler : hanfpe appelle UTMFPE qui fait UTMESS('F') */
      signal(SIGFPE, hanfpe);
   }
      
#endif
}
