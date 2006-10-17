/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTCSM UTILITAI  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS */
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
#include "aster.h"

/* temps(sec) consommes user et systeme pour ce processus   	 	*/

#ifdef _POSIX
#include <sys/times.h>
#endif

#include <time.h>

/*
   Avec les versions de GCC >= 4, CLK_TCK n'est plus défini.
   Il faut dans ce cas utiliser CLOCKS_PER_SEC.
   ATTENTION car, inversement, CLOCKS_PER_SEC ne donne pas les
   bons temps avec les versions GCC < 4.
   
   Quand les deux sont définis, il y a un facteur 10000 entre
   les deux (CLOCKS_PER_SEC = 1e6, CLK_TCK = 100). A priori,
   cela semble lier à l'utilisation de times() ou getrusage()
*/
#ifndef _USE_CLK_TCK
   #define CLOCKS_PER_SEC_VALUE CLOCKS_PER_SEC
#else
   #define CLOCKS_PER_SEC_VALUE CLK_TCK
#endif

void DEFP(UTTCSM, uttcsm, double *t_csm)
{
#ifdef _POSIX
   struct tms temps;
   times (&temps);
   t_csm[0]=(double)temps.tms_utime/(double)CLOCKS_PER_SEC_VALUE;
   t_csm[1]=(double)temps.tms_stime/(double)CLOCKS_PER_SEC_VALUE;

#else
   t_csm[0]=(double)clock()/CLOCKS_PER_SEC_VALUE;
   t_csm[1]=(double)0.;

#endif
}
