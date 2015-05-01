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


    1: _(u"""
  Degré de liberté physique associé au noeud %(k1)s et à la composante %(k2)s.
"""),

    2: _(u"""
  Degré de liberté de Lagrange associé au blocage du noeud %(k1)s et de la composante %(k2)s.
"""),

    3: _(u"""
  Degré de liberté de Lagrange associé à une relation linéaire entre plusieurs degrés de liberté.
  La relation linéaire a été définie par la commande ayant produit le concept de nom %(k1)s.
  La liste des noeuds impliqués dans cette relation linéaire est la suivante:
"""),

    4: _(u"""    Noeud %(k1)s"""),

    5: _(u"""
  Degré de liberté d'un système généralisé pour le macro-élément %(k1)s et l'équation %(i1)d.
"""),

    6: _(u"""
  Degré de liberté d'un système généralisé pour la liaison %(i1)d et l'équation %(i2)d.
"""),
}
