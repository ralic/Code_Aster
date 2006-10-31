/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTLIM UTILITAI  DATE 30/10/2006   AUTEUR D6BHHJP J.P.LEFEBVRE */
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

#include <sys/time.h>
#include <sys/resource.h>
#include <float.h>
#include <limits.h>

#include "aster.h"

void DEFP(UTTLIM, uttlim, double *t_lim)
{
   int itpm;
   double tmax;
#ifdef _USE_RLIMIT
   struct rlimit rlp;
#endif
   if (strlen(g_tpmax) > 0) {
      sscanf(g_tpmax,"%d",&itpm);
      *t_lim = (double)itpm;
   }
   else {
      tmax = ((double) LONG_MAX)/2;
#ifdef _USE_RLIMIT
      getrlimit(RLIMIT_CPU,&rlp);
      *t_lim = (double)rlp.rlim_max;
#else
      *t_lim = tmax;
#endif
   }
   if (*t_lim > tmax) {
      *t_lim = tmax;
   }
}
