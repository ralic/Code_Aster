/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MAXBAS utilitai  DATE 22/03/2011   AUTEUR COURTOIS M.COURTOIS */
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
#include <stdio.h>
#include "aster.h"

double DEFP(MAXBAS, maxbas, DOUBLE *val)
/*
** Fonction pour positionner et interroger l'indicateur
** fixant la taille maximale de base en mega-octets
** val -> si <  0 MAXBAS renvoie taille en mega-octets
**        si >= 0 positionne l'indicateur par une valeur en Mo
*/
{
   static DOUBLE TAILLE_BASE=0.0;
   if (*val >= 0.) {
      TAILLE_BASE=*val;
   }
   return TAILLE_BASE;
}
