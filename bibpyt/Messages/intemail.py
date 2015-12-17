# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {






    35: _(u"""
Utilisation de MODI_MAILLAGE / ABSC_CURV :
  Il existait déjà une carte pour les abscisses curviligne dans le
  maillage. Celle-ci sera écrasée.
 """),

    36: _(u"""
Utilisation de MODI_MAILLAGE / ABSC_CURV :
  L'ensemble des segments ne forme pas une ligne ouverte.
  Le noeud %(k1)s appartient à plus de deux segments.
 """),

    37: _(u"""
Utilisation de MODI_MAILLAGE / ABSC_CURV :
  L'ensemble des segments ne forme pas une ligne ouverte.
  Il n'y a pas deux extrémités. Il y en a %(i1)d.
 """),

    38: _(u"""
Utilisation de MODI_MAILLAGE / ABSC_CURV :
  L'ensemble des segments n'a pas pour extrémité le noeud "origine".
 """),

}
