/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF ISINTE utilitai  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
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
#include <stdio.h>
#ifdef CRAY
   long ISINTE(long* val)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || LINUX64 || SOLARIS64 
   long isinte_(long* val)
#elif defined HPUX
   long isinte(long* val)
#elif defined PPRO_NT
   long __stdcall ISINTE(long* val)
#endif
/*
** Fonction pour positionner et interroger l'indicateur
** d'execution d'Aster en interactif
** val -> si <0  estint renvoie la valeur de l'indicateur
**        si >=0 positionne l'indicateur
*/
{
static long IND_INTERACTIF=0;

if (*val >= 0) {
   IND_INTERACTIF=*val;
   }

return(IND_INTERACTIF);
}
