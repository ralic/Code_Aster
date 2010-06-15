/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF modifpe utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
** Modification de l'interception des signaux dus aux floating
** point exception
** Cette fonction est uniquement utilisee sous IRIX
*/

#if defined IRIX
#include <siginfo.h>
#include <sigfpe.h>
#include <string.h>

void modfpe_( char *flag, char *opera , long *trace, long *count, long *abort, long lflag, long lopera)
{
 extern struct sigfpe_template sigfpe_[_N_EXCEPTION_TYPES+1];
 
 if (strcmp(opera,"UNDERFL") == 0) { 
   if (strcmp(flag,"ON") == 0) { 
     sigfpe_[_UNDERFL].trace  = (int) *trace; 
     sigfpe_[_UNDERFL].count  = (int) *count;
     sigfpe_[_UNDERFL].abort  = (int) *abort;
     handle_sigfpes(_ON,  _EN_UNDERFL, 0, _ABORT_ON_ERROR, 0);
   } else {
     handle_sigfpes(_OFF, _EN_UNDERFL, 0, 0, 0);    
   }
 } 

 if (strcmp(opera,"OVERFL") == 0) { 
   if (strcmp(flag,"ON") == 0) { 
     sigfpe_[_OVERFL].trace  = (int) *trace;
     sigfpe_[_OVERFL].count  = (int) *count;
     sigfpe_[_OVERFL].abort  = (int) *abort;
     handle_sigfpes(_ON,  _OVERFL, 0, _ABORT_ON_ERROR, 0);
   } else {
     handle_sigfpes(_OFF, _OVERFL, 0, 0, 0);    
   }
 } 
 
 if (strcmp(opera,"DIVZERO") == 0) { 
   if (strcmp(flag,"ON") == 0) { 
     sigfpe_[_DIVZERO].trace  = (int) *trace;
     sigfpe_[_DIVZERO].count  = (int) *count;
     sigfpe_[_DIVZERO].abort  = (int) *abort;
     handle_sigfpes(_ON,  _DIVZERO, 0, _ABORT_ON_ERROR, 0);
   } else {
     handle_sigfpes(_OFF, _DIVZERO, 0, 0, 0);    
   }
 } 
 
 if (strcmp(opera,"INVALID") == 0) { 
   if (strcmp(flag,"ON") == 0) { 
     sigfpe_[_INVALID].trace  = (int) *trace;
     sigfpe_[_INVALID].count  = (int) *count;
     sigfpe_[_INVALID].abort  = (int) *abort;
     handle_sigfpes(_ON,  _INVALID, 0, _ABORT_ON_ERROR, 0);
   } else {
     handle_sigfpes(_OFF, _INVALID, 0, 0, 0);    
   }
 } 

 if (strcmp(opera,"INT_OVERFL") == 0) { 
   if (strcmp(flag,"ON") == 0) { 
     sigfpe_[_INT_OVERFL].trace  = (int) *trace;
     sigfpe_[_INT_OVERFL].count  = (int) *count;
     sigfpe_[_INT_OVERFL].abort  = (int) *abort;
     handle_sigfpes(_ON,  _INT_OVERFL, 0, _ABORT_ON_ERROR, 0);
   } else {
     handle_sigfpes(_OFF, _INT_OVERFL, 0, 0, 0);    
   }
 } 

}
#endif
