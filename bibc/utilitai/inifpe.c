/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF inifpe utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
** Initialisation de l'interception des signaux dus aux floating
** point exception
** Cette fonction est uniquement utilisee sous IRIX
*/

#if defined IRIX
#include <siginfo.h>
#include <sigfpe.h>
#include <limits.h>
#include <stdlib.h>
int inifpe( void )
{
 extern struct sigfpe_template sigfpe_[_N_EXCEPTION_TYPES+1];
if (getenv ("TRAP_FPE") == NULL ) {
/* remontee d'erreur pour chaque type */

/* sigfpe_[_UNDERFL].trace   =0; */
 sigfpe_[_OVERFL].trace    =1;
 sigfpe_[_DIVZERO].trace   =1;
 sigfpe_[_INVALID].trace   =1;
 sigfpe_[_INT_OVERFL].trace=1;

/* impression en fin de travail du nombre d'erreurs de chaque type */

/* sigfpe_[_UNDERFL].count   =INT_MAX; */
 sigfpe_[_OVERFL].count    =INT_MAX;
 sigfpe_[_DIVZERO].count   =INT_MAX;
 sigfpe_[_INVALID].count   =INT_MAX;
 sigfpe_[_INT_OVERFL].count=INT_MAX;

/* abort provoque a la ième erreur */

/* sigfpe_[_UNDERFL].abort   =INT_MAX; */
 sigfpe_[_INVALID].abort   =1;
 sigfpe_[_DIVZERO].abort   =1;
 sigfpe_[_OVERFL].abort    =1;
 sigfpe_[_INT_OVERFL].abort=1;

 handle_sigfpes(_ON,	 _EN_OVERFL|_EN_DIVZERO
	      |	_EN_INVALID | _EN_INT_OVERFL, 0, _ABORT_ON_ERROR, 0);

                  
 return(0);
} else {
 return(1);
} 
   
}
#endif
