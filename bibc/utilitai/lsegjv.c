/* -------------------------------------------------------------------- */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF LSEGJV UTILITAI  DATE 02/02/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* -------------------------------------------------------------------- */
#include <stdlib.h>
#include "aster.h"

INTEGER DEFP(LSEGJV, lsegjv, INTEGER *val)/*
** Fonction pour positionner et interroger l'indicateur de longueur
** des segments de valeurs associés au type 3 de
** parcours de la segmentation de memoire JEVEUX
** val -> si <  0 LSEGJV renvoie le type de parcourt
**        si >= 0 positionne l'indicateur
*/
{
   static INTEGER LSEG_JEVEUX=0;
   if (*val >= 0) {
      LSEG_JEVEUX=*val;
   }
   return LSEG_JEVEUX;
}
