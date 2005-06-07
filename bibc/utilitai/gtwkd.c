/* ----------------------------------------------------------------- */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF GTWKD UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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

#ifdef CRAY
#include <unistd.h>
   long GTWKD(char *rep, long *lrep)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <unistd.h>
   long gtwkd_(char *rep, long *lrep, unsigned long ll)
#elif defined HPUX
#include <unistd.h>
   long gtwkd (char *rep, long *lrep, unsigned long ll)
#elif defined PPRO_NT
#include <direct.h>
#include <stdlib.h>
   extern long __stdcall GTWKD(char *rep,unsigned long ll, long *lrep)
#endif
{
long ier,l;
#if defined CRAY
char *cier;size_t lc;
lc = *lrep;
cier = getcwd(rep,lc) ;

#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
char *cier;size_t lc;
lc = *lrep;
cier = getcwd(rep,lc) ;

#elif defined PPRO_NT
extern char *_getcwd(char *rep,int lc);
char *cier;int lc;
lc = *lrep;
cier = _getcwd(rep,lc) ;
#endif
ier = 0;
l   = 0;
while (*rep != '\0') {
       *rep++;
       l++; }
*lrep = l;
if ( cier != NULL ) { ier = 4;}
return(ier);
}
