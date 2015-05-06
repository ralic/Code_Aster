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

    1 : _(u"""
On ne sait pas calculer les déformations plastiques avec des lois prenant en compte la métallurgie.
"""),

    2 : _(u"""
On ne sait pas calculer les déformations plastiques avec de l'élasticité non-isotrope.
"""),

   3 : _(u"""
  -> Attention vous êtes en contraintes planes, et vous utilisez la loi
     de comportement %(k1)s. La composante du tenseur des déformations
     plastiques EPZZ est calculée en supposant l'incompressibilité des
     déformations plastiques : EPZZ = -(EPXX + EPYY).
  -> Risque & Conseil :
     Vérifiez que cette expression est valide avec votre loi de comportement.

"""),
}
