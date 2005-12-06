/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTLIM UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
/* temps (sec)  maxi pour ce processus                    	*/
extern char g_tpmax[];
#include <string.h>
#include <stdio.h>
#ifdef CRAY
#include <sys/category.h>
#include <sys/resource.h>
#include <time.h>
#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <sys/time.h>
#include <sys/resource.h>
#endif


#ifdef CRAY
void UTTLIM ( float *t_lim )
{
  if (strlen(g_tpmax) > 0) {
  int itpm;
  sscanf(g_tpmax,"%d",&itpm);
  *t_lim= (float)itpm;
  }
  else {
	*t_lim = (float)limit(C_PROC, 0, L_CPU, -1)/(float)CLK_TCK;
  }
}
#elif  defined SOLARIS 
void uttlim_ ( double *t_lim )
{
  struct rlimit rlp;
  if (strlen(g_tpmax) > 0) {
    int itpm;
    sscanf(g_tpmax,"%d",&itpm);
    *t_lim= (double)itpm;
  }
  else {
	getrlimit(RLIMIT_CPU,&rlp);
 	*t_lim = (double)rlp.rlim_max;
  }
}
#elif  defined HPUX 
#include <float.h>
#include <limits.h>
void uttlim ( double *t_lim )
{
  if (strlen(g_tpmax) > 0) {
    int itpm;
    sscanf(g_tpmax,"%d",&itpm);
    *t_lim= (double)itpm;
  }
  else {
	*t_lim = (double) LONG_MAX;
  }
}
#elif  defined IRIX || P_LINUX || TRU64 || SOLARIS64 
#include <float.h>
#include <limits.h>
void uttlim_ ( double *t_lim )
{
  if (strlen(g_tpmax) > 0) {
    int itpm;
    sscanf(g_tpmax,"%d",&itpm);
    *t_lim= (double)itpm;
  }
  else {
	*t_lim = (double) LONG_MAX;
  }
}
#elif  defined PPRO_NT
#include <float.h>
#include <limits.h>
void __stdcall UTTLIM ( double *t_lim )
{
  if (strlen(g_tpmax) > 0) {
    int itpm;
    sscanf(g_tpmax,"%d",&itpm);
    *t_lim= (double)itpm;
  }
  else {
	*t_lim = (double) LONG_MAX;
  }
}
#endif
