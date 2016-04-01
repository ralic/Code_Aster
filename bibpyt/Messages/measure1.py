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

# Attention a ne pas faire de retour à la ligne !

cata_msg = {

    1  : _(u"""
  Temps CPU consommé dans ce pas de temps    : %(k1)s
"""),

    2  : _(u"""
  Temps CPU consommé dans le calcul          : %(k1)s
"""),

    3  : _(u"""    dont temps CPU "perdu" dans les découpes            : %(k1)s"""),

    6  : _(u"""    * Temps total factorisation matrice                 : %(k1)s (%(i1)d factorisations)"""),

    7  : _(u"""    * Temps total intégration comportement              : %(k1)s (%(i1)d intégrations)"""),

    8  : _(u"""    * Temps total résolution K.U=F                      : %(k1)s (%(i1)d résolutions)"""),

    9  : _(u"""    * Temps résolution contact                          : %(k1)s (%(i1)d itérations)"""),

    10 : _(u"""    * Temps appariement contact                         : %(k1)s (%(i1)d appariements)"""),

    11 : _(u"""    * Temps construction second membre                  : %(k1)s"""),

    12 : _(u"""    * Temps assemblage matrice                          : %(k1)s"""),

    13 : _(u"""    * Temps préparation données contact                 : %(k1)s (%(i1)d préparations)"""),

    14 : _(u"""    * Temps calculs élémentaires contact                : %(k1)s"""),

    15 : _(u"""    * Temps dans le post-traitement                     : %(k1)s"""),

    16 : _(u"""    * Temps dans l'archivage                            : %(k1)s"""),

    17 : _(u"""    * Temps autres opérations                           : %(k1)s"""),

    18 : _(u"""    * Nombre de liaisons de contact                     : %(i1)d"""),

    19 : _(u"""    * Nombre de liaisons de frottement adhérentes       : %(i1)d"""),

    20 : _(u"""    * Nombre de cycles de type contact/pas contact      : %(i1)d"""),

    21 : _(u"""    * Nombre de cycles de type glissement/adhérence     : %(i1)d"""),

    22 : _(u"""    * Nombre de cycles de type glissement avant/arrière : %(i1)d"""),

    23 : _(u"""    * Nombre de cycles de type point fixe contact       : %(i1)d"""),

    24 : _(u"""    * Nombre d'itérations de recherche linéaire         : %(i1)d"""),

    25 : _(u"""    * Nombre de pas de temps                            : %(i1)d"""),

    26 : _(u"""    * Nombre d'itérations de Newton                     : %(i1)d"""),

}
