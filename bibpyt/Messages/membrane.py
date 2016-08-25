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

      Vous utilisez un élément MEMBRANE avec STAT_NON_LINE.
      PROBLEME : Les membranes ne prennent pas en compte le COMPORTEMENT %(k1)s .
      SOLUTION : Si vous faites l'hypothèse des petites perturbations (valeur par défaut), indiquez
                 COMPORTEMENT=PETIT. Si vous êtes en grandes déformations utilisez COMPORTEMENT=GROT_GDEP.

"""),
    
        3 : _(u"""

      Vous utilisez un élément MEMBRANE en GROT_GDEP.
      PROBLEME : Les membranes en non linéaire ne fonctionnent qu'avec le choix ELAS_HYPER_MEMB sous 
                 le mot clé RELATION de COMPORTEMENT.
      SOLUTION : Changez votre choix de COMPORTEMENT par ELAS_HYPER_MEMB.

"""),
        4 : _(u"""

      Vous utilisez un élément MEMBRANE en GROT_GDEP  avec ELAS_HYPER_MEMB.
      PROBLEME : En membrane hyperélastique, seuls le module d'Young et le coefficient de poisson sont nécessaires.
      SOLUTION : Dans DEFI_MATERIAU choisissez le comportement ELAS.

"""),
        5 : _(u"""

      Vous utilisez un élément MEMBRANE en GROT_GDEP.
      PROBLEME : Vous utilisez une option qui n'est pas prise en compte pour les membranes en grandes deformations.
      SOLUTION : Changez l'option sous le mot clé 'PREDICTION'.

"""),
        6 : _(u"""

      Vous utilisez un élément MEMBRANE en GROT_GDEP avec ELAS_HYPER_MEMB.
      PROBLEME : Vous avez spécifié des angles dans ANGLE_REP.
      AVERTISSEMENT : Ces angles ne servent pas à définir une anisotropie, leur seul 
                      rôle et de définir les directions d'affichage des contraintes

"""),
        7 : _(u"""

      Vous utilisez un élément MEMBRANE en GROT_GDEP avec ELAS_HYPER_MEMB.
      PROBLEME : Vous avez spécifié une option de chargement non prise en compte.
      SOLUTION : Modifiez vos chargements. 

"""),
        8 : _(u"""

      Vous voulez post-traiter un élément MEMBRANE en GROT_GDEP avec %(k1)s.
      PROBLEME : Vous avez spécifié une option de post-traitement non prise en compte.
      SOLUTION : Modifiez vos options de post-traitement. 

"""),

}
