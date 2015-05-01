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
 Erreur de maillage :
   La maille %(k1)s de type %(k2)s est trop distordue.
   Le jacobien de la transformation géométrique n'a pas le même signe sur tous les
   points de Gauss.

 Risques & conseils :
   Le maillage a-t-il été produit par un mailleur ?
   La connectivité respecte-t-elle bien la convention Aster ?
"""),

    2: _(u"""
Pour le noeud %(k1)s de la maille %(k2)s, la coordonnée X est négative (x=%(r1)G).
"""),

    3: _(u"""
Pour une modélisation axisymétrique, la coordonnée X doit être positive, nulle ou
très faiblement négative ( > -1.d-6 * X_MAX)

 Conseils :
  * Vérifiez votre maillage.
  * Vous pouvez utiliser MODI_MAILLAGE / DEFORME pour repositionner votre maillage
    dans le demi espace  X >= 0
"""),

}
