/* -------------------------------------------------------------------- */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF UTTRST UTILITAI  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
/* temps(sec) total restant pour ce processus              */

#if defined PPRO_NT
  void __stdcall UTTLIM( double *t_lim );
  void __stdcall UTTCSM( double *t_csm);
#endif


#ifdef CRAY
/* calcul du temps restants user et systeme en multitasking */
/* sur cray pour ce processus */
void UTTRST ( float *t_rst )
{
  float 	t_csm[2] , t_lim;

/* temps user + systeme consommes  pour ce processus    */

  UTTCSM (t_csm);

/* temps limite pour ce processus                       */

  UTTLIM (&t_lim);

/* temps restant a peu pres pour ce processus           */

  *t_rst = t_lim - t_csm[0] - t_csm[1];
}
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
void uttrst_ ( double *t_rst )
{
  double t_csm[2] , t_lim;
  uttcsm_ (t_csm);
  uttlim_ (&t_lim);
  *t_rst = t_lim - t_csm[0] - t_csm[1];
}
#elif defined PPRO_NT
void __stdcall UTTRST ( double *t_rst )
{
  double t_csm[2] , t_lim;
  UTTCSM (t_csm);
  UTTLIM (&t_lim);
  *t_rst = t_lim - t_csm[0] - t_csm[1];
}
#elif defined HPUX
void uttrst ( double *t_rst )
{
  double t_csm[2] , t_lim;
  uttcsm (t_csm);
  uttlim (&t_lim);
  *t_rst = t_lim - t_csm[0] - t_csm[1];
}
#elif defined PPRO_NT
void __stdcall UTTRST ( double *t_rst )
{
  double 	t_csm[2] , t_lim;
  UTTCSM (t_csm);
  UTTLIM (&t_lim);
  *t_rst = t_lim - t_csm[0] - t_csm[1];
}
#endif
