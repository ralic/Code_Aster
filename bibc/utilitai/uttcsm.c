/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTCSM UTILITAI  DATE 08/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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

/* temps(sec) consommes user et systeme pour ce processus   	 	*/

#ifdef CRAY
#elif defined  SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
#include <sys/times.h>
#include <time.h>
#elif defined  PPRO_NT
#include <time.h>
#endif

/*
   Avec les versions de GCC >= 4, CLK_TCK n'est plus défini.
   Il faut dans ce cas utiliser CLOCKS_PER_SEC.
   ATTENTION car, inversement, CLOCKS_PER_SEC ne donne pas les
   bons temps avec les versions GCC < 4.
   
   Quand les deux sont définis, il y a un facteur 10000 entre
   les deux (CLOCKS_PER_SEC = 1e6, CLK_TCK = 100). A priori,
   cela semble lier à l'utilisation de times() ou getrusage()
*/
#ifndef CLK_TCK
   #define __USED_CLOCKS_PER_SEC CLOCKS_PER_SEC
#else
   #define __USED_CLOCKS_PER_SEC CLK_TCK
#endif


#ifdef CRAY
void UTTCSM ( float *t_csm )
{
 float  t_sys,  t_sys_ini, t_sys_csm, t_usr_csm;
 void UTTSYS (long *id , float *t_sys_csm );
/* temps systeme consomme approximatif (getjtab(2))     		*/

 UTTSYS ( (long *) 1 , &t_sys_csm);

/* temps user consomme a peu pres exact (SECOND(3F))    		*/

 SECOND ( &t_usr_csm );

 t_csm[0] = t_usr_csm;
 t_csm[1] = t_sys_csm;
}
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
void uttcsm_(double *t_csm)
{
 struct tms temps;
 times (&temps);
 t_csm[0]=(double)temps.tms_utime/(double)__USED_CLOCKS_PER_SEC;
 t_csm[1]=(double)temps.tms_stime/(double)__USED_CLOCKS_PER_SEC;
}
#elif defined PPRO_NT
void __stdcall UTTCSM(double *t_csm)
{
 t_csm[0]=(double)clock()/__USED_CLOCKS_PER_SEC;
 t_csm[1]=(double)0.;
}
#elif defined HPUX
void uttcsm(double *t_csm)
{
 struct tms temps;
 times (&temps);
 t_csm[0]=(double)temps.tms_utime/(double)__USED_CLOCKS_PER_SEC;
 t_csm[1]=(double)temps.tms_stime/(double)__USED_CLOCKS_PER_SEC;
}
#elif defined PPRO_NT
void __stdcall UTTCSM(double *t_csm)
{
 t_csm[0]=(double)clock()/__USED_CLOCKS_PER_SEC;
 t_csm[1]=(double)0.;
}
#endif
