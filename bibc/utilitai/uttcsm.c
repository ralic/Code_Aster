/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTCSM UTILITAI  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS */
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
#include "aster.h"

/* retourne 3 temps(sec) : consommes user et systeme pour ce processus
                           + temps elapsed (depuis epoch)
*/

#ifdef _POSIX
#include <sys/times.h>
#include <sys/time.h>
#include <unistd.h>
#endif

#include <time.h>


/*
   On trouve parfois ceci :
   "CLK_TCK is described as an obsolete name for CLOCKS_PER_SEC"

   Quand les deux sont définis, il y a un facteur 10000 entre
   les deux (CLOCKS_PER_SEC = 1e6, CLK_TCK = 100).
*/

#ifdef CLK_TCK
#define CLOCKS_PER_SEC_VALUE CLK_TCK
#else
#define CLOCKS_PER_SEC_VALUE sysconf(_SC_CLK_TCK)
#endif


void DEFP(UTTCSM, uttcsm, DOUBLE *t_csm)
{
    DOUBLE elaps;

#ifdef _POSIX
/* calcul de elaps avec gettimeofday  */
    struct timeval tv;
    struct timezone tz;
    gettimeofday(&tv,&tz);
    elaps=(DOUBLE) tv.tv_sec + (DOUBLE) tv.tv_usec / 1000000.;

#else
/* calcul de elaps : date depuis epoch en secondes
   ce nombre est stocké dans un double
   Parfois à la seconde près.
   Sous WIN, gettimeofday n'existe pas.
   Une implémentation : http://www.suacommunity.com/dictionary/gettimeofday-entry.php
*/
    time_t t1, t0, *pt1 ;
    t0=0;
    t1=time(NULL);
    elaps=difftime(t1,t0);
#endif

    t_csm[2]=elaps;


#ifdef _POSIX
   struct tms temps;
   times (&temps);
   t_csm[0]=(DOUBLE)temps.tms_utime/(DOUBLE)CLOCKS_PER_SEC_VALUE;
   t_csm[1]=(DOUBLE)temps.tms_stime/(DOUBLE)CLOCKS_PER_SEC_VALUE;

#else
   t_csm[0]=(DOUBLE)clock()/CLOCKS_PER_SEC_VALUE;
   t_csm[1]=(DOUBLE)0.;

#endif
}
