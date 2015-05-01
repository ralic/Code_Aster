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


    4 : _(u"""
 DEBORST non compatible avec couplage UMLV/Mazars.
 Mais le traitement analytique est réalisé, il suffit de supprimer le
 mot-clé DEBORST),

"""),
    5 : _(u"""
 pas de C_PLAN pour ENDO_ISOT_BETON
 utiliser C_PLAN_DEBORST
"""),

    7 : _(u"""
 pas d'orthotropie non linéaire
"""),

    8 : _(u"""
 loi de comportement hyper-élastique non prévue
"""),

    9 : _(u"""
 C_PLAN méthode DEBORST et grandes déformations sont incompatibles
"""),

    10 : _(u"""
 COMP1D et SIMO_MIEHE incompatibles
"""),

    12 : _(u"""
 couplage fluage/fissuration :
 nombre total de variables internes incohérent <--> erreur de programmation.
"""),



    20 : _(u"""
 le champ de déplacement DIDI n'est pas trouvé dans le concept  %(k1)s
"""),

    61 : _(u"""
 option  %(k1)s  non traitée
"""),

    63 : _(u"""
 pas existence de solution pour le saut
"""),

    64 : _(u"""
 existence d'un élément à discontinuité trop grand
 non unicité du saut
"""),

    65 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 1
"""),

    66 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 2
"""),

    67 : _(u"""
 non convergence du NEWTON pour le calcul du saut numéro 3
"""),

    68 : _(u"""
 erreur dans le calcul du saut
"""),

    69 : _(u"""
 loi %(k1)s  non implantée pour les éléments discrets
"""),





    74 : _(u"""
  valeur de D_SIGM_EPSI non trouvée
"""),

    75 : _(u"""
  valeur de SY non trouvée
"""),

    76 : _(u"""
 développement non implanté
"""),

    79 : _(u"""
 loi de comportement avec irradiation, le paramètre N doit être supérieur à 0
"""),

    80 : _(u"""
 loi de comportement avec irradiation, le paramètre PHI_ZERO doit être supérieur à 0
"""),

    81 : _(u"""
 loi de comportement avec irradiation, le paramètre phi/K.PHI_ZERO+L doit être supérieur ou égal à 0
"""),

    82 : _(u"""
 loi de comportement avec irradiation, le paramètre phi/K.PHI_ZERO+L vaut 0. Dans ces conditions le paramètre BETA doit être positif ou nul
"""),

    98 : _(u"""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage de dessiccation
 intrinsèque avec SECH comme paramètre
"""),

}
