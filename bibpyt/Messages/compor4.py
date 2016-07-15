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

    1: _(u"""
Liste des comportements
"""),

    2: _(u"""
Comportement - Occurrence  %(i1)d"""),

    4: _(u"""Affecté sur %(i1)d éléments"""),

    5: _(u"""  Relation                             : %(k1)s"""),

    6: _(u"""  Déformation                          : %(k1)s"""),

    8: _(u"""  Algorithme contraintes planes (ou 1D): Deborst"""),

    9: _(u"""  Nombre total de variables internes   : %(i1)d"""),


    12: _(u"""
  Pour les kits métallurgiques KIT_META en grandes déformations, on n'a pas d'informations sur le nom des variables internes.
"""),

    13: _(u"""
  Il y a deux types de modélisations différents pour une même affectation du comportement MFRONT.
  Chaque occurrence de COMPORTEMENT avec relation MFRONT doit contenir un seul type de modélisation.
  Par exemple, une occurrence pour les groupes de mailles contenant des éléments 3D (et comportement MFRONT),
  puis une occurrence pour les groupes de mailles contenant des éléments discrets (et comportement ELAS).

  Les types de modélisation rencontrés sont : %(k1)s et %(k2)s.
"""),


    14: _(u"""
  La modélisation %(k1)s dans le modèle n'est pas utilisable avec MFront.
"""),

    15: _(u"""
  Pour les comportements multiples (définis par DEFI_COMPOR), on n'a pas d'informations sur le nom des variables internes.
"""),

    16: _(u"""
  Pour les comportements externes (MFRONT ou UMAT), on n'a pas d'informations sur le nom des variables internes.
"""),

    20: _(u"""         V%(i1)d : %(k1)s"""),

    53: _(u"""
Comportement POLYCRISTAL
      Nombre de grains  %(i1)d : localisation %(k1)s
      Nombre d'occurrences de MONOCRISTAL différentes : %(i2)d - nombre de variables internes : %(i3)d
      Noms des variables internes: """),

    54: _(u""" A partir de la variable interne %(i1)d : pour chaque grain : """),

    55: _(u""" Dernière variable interne V%(i1)d : %(k1)s"""),

    56: _(u""" ... jusqu'à V%(i1)d """),


    62 : _(u"""
  -> Le critère de convergence pour intégrer le comportement 'RESI_INTE_RELA'
     est lâche (très supérieur à la valeur par défaut).
  -> Risque & Conseil :
     Cela peut nuire à la qualité de la solution et à la convergence.
"""),

    63 : _(u"""
La définition explicite du comportement est obligatoire.
"""),

    64 : _(u"""
Comme vous n'avez pas défini explicitement le comportement, tout le modèle est supposé élastique en petites perturbations.
"""),

    65 : _(u"""
Il y a trop d'occurrences du mot-clef facteur COMPORTEMENT. On n'affichera aucune information sur les comportements."""),

    70 : _(u"""
Le comportement s'intègre avec un algorithme de type analytique.
On ne peut donc pas utiliser le mot-clé  %(k1)s . On l'ignore.
"""),

    71 : _(u"""
La valeur propre numéro %(i1)d du module tangent local est négative et vaut %(r1)f.
L'énergie libre n'est donc pas convexe ce qui peut mener à des problèmes de convergence.
"""),

    72: _(u"""
L'occurrence %(i1)d du mot-clef COMPORTEMENT n'affecte aucune maille du modèle.
Par défaut, on affecte le comportement élastique en petites déformations sur les mailles du modèle non affectées par l'utilisateur
Conseils: vérifier que ce comportement est voulu (pas d'oubli dans AFFE_MODELE).
"""),

}
