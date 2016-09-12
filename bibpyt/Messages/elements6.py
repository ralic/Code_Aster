# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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

    5 : _(u"""
On ne peut pas utiliser le modèle 3D_SI avec un comportement élastique de type %(k1)s.
"""),

    6 : _(u"""
  -> Erreur de programmation :
  -> L argument %(k1)s est manquant ou mal renseigné dans l appel a la routine <xcalfev>
  -> Veuillez renseigner cette argument
"""),

    7 : _(u"""
  -> Erreur de programmation :
  -> L argument %(k1)s est manquant ou mal renseigné dans l appel a la routine <coor_cyl>
  -> Veuillez renseigner cette argument
"""),

    8 : _(u"""
  -> Erreur de programmation :
  -> En dimension %(i1)d, le calcul d'un tenseur de courbure dans la routine <coor_cyl>
     n'est pas autorisé
"""),

    9 : _(u"""
  -> Erreur de programmation :
  -> En dimension %(i1)d, le calcul d'une matrice de passage n'a pas de sens
     dans la routine <xbasgl>
"""),

    10 : _(u"""
  -> Erreur de programmation :
  -> Au moins un des paramètres élastiques (module d'Young ou coefficent de poisson) n'a
     pas été trouvé dans <xkamat> lors de l'évaluation des fonctions vectorielles XFEM
"""),

}
