/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF GTCWD UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   long GTCWD(char *rep)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long gtcwd_(char *rep,unsigned long ll)
#elif defined HPUX
   long gtcwd(char *rep,unsigned long ll)
#elif defined PPRO_NT
   extern long __stdcall GTCWD(char *rep,unsigned long ll)
#endif
{
#if defined CRAY
extern GETCWD(char *rep);
long ier;
ier = GETCWD(rep) ;

#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
extern long getcwd_(char *rep,unsigned long ll);
long ier;
ier = getcwd_(rep,ll) ;

#elif defined HPUX
extern long getcwd(char *rep,unsigned long ll);
long ier;
ier = getcwd(rep,ll) ;

#elif defined PPRO_NT
extern long  __stdcall GETCWD(char *rep,unsigned long ll);
long ier;
ier = GETCWD(rep,ll) ;
#endif
return(ier);
}
