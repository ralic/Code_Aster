/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MEMJVX utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
   long MEMJVX(double* val)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long memjvx_(double* val)
#elif defined HPUX
   long memjvx(double* val)
#elif defined PPRO_NT
   long __stdcall MEMJVX(double* val)
#endif
/*
** Fonction pour positionner et interroger l'indicateur
** d'allocation d'une zone de memoire exacte JEVEUX
** val -> si <  0 ISSUIV renvoie la valeur de la memoire en mots
**        si >= 0 positionne l'indicateur par une valeur en Mw
*/
{
static long MEM_JEVEUX=0;
if (*val >= 0) {
   MEM_JEVEUX=(long)((*val)*1024*1024);
   }

return(MEM_JEVEUX);
}
