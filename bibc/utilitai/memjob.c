/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF memjob utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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

extern char g_memory[];
#include <string.h>
#include <stdio.h>
#if defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <sys/resource.h>
#include <sys/time.h>
   long memjob_(void)
#elif defined HPUX
#include <sys/resource.h>
#include <sys/time.h>
   long memjob(void)
#elif defined CRAY
#include <sys/resource.h>
#include <sys/category.h>
   long MEMJOB(void)
#elif defined PPRO_NT
   extern long __stdcall MEMJOB(void)
#endif


/* Renvoie, en Mmots, la memoire definie pour le job */
{
#ifdef CRAY
   if (strlen(g_memory) > 0) {
      int imem;
      sscanf(g_memory,"%d",&imem);
      return((long)imem);
      }
    else {
      /* limit() renvoie un nb de blocs de 512 mots */
      return(limit(C_JOB, 0, L_MEM, -1)*512/1048576);
      }
#elif defined SOLARIS
   int memmega, imem;
   struct rlimit limit;

   if ( getrlimit(RLIMIT_DATA,&limit) < 0 ) {
      return(0);
      }
   else {

      if ( strlen(g_memory) != '\0' ) {
         sscanf(g_memory,"%d",&imem);
         return((long)imem);
         }
      else {
         memmega = limit.rlim_cur/(sizeof(float)*1048576);
         return ((long)memmega);
         }
      }
#elif defined PPRO_NT || HPUX || IRIX || P_LINUX || TRU64  || SOLARIS64
   int imem;
   if ( strlen(g_memory) != '\0' ) {
        sscanf(g_memory,"%d",&imem);
        return ((long) imem );}
   else {
        return( 16 );}
#endif
}
