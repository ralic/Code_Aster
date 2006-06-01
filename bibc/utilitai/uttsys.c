/* -------------------------------------------------------------------- */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTSYS UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
/* -------------------------------------------------------------------- */
/* initialisation/restitution temps systeme (cray)   			*/

#ifdef CRAY
#include <sys/types.h>
#include <sys/jtab.h>
#include <time.h>
void UTTSYS ( long *id , float *t_sys_csm )
{
  struct jtab buf;
  static float t_sys_ini;
  float t_sys_abs;
  long  jid;

/* initialisation temps systeme (getjtab(2))                */

  jid = getjtab(&buf);
  t_sys_abs = (float)buf.j_scputime/(float)CLK_TCK;

  if ( *id == 0 ) {
    t_sys_ini = t_sys_abs; }

/* restitution temps systeme (static)               	    */

  else {
   *t_sys_csm = t_sys_abs - t_sys_ini;
   }
}
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
void uttsys_ ( long *id , double *t_sys_csm)
{}
#elif defined HPUX
void uttsys ( long *id , double *t_sys_csm)
{}
#elif defined PPRO_NT
void __stdcall UTTSYS ( long *id , double *t_sys_csm)
{}
#endif
