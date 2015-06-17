# coding=utf-8

# ======================================================================
# COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: nicolas.brie at edf.fr

cata_msg = {

    1: _(u"""                 Participation du mode : %(i1)2d --> %(r1)12.5E
"""),

    2: _(u""" =======================================================================
                Calcul modal par %(k1)s

"""),

    3: _(u"""
numéro    itération      erreur              valeur propre
"""),

    4: _(u"""%(i1)4d        %(i2)4d       %(r1)10.3E      ( %(r2)9.2E, %(r3)9.2E )
"""),

    5: _(u""" Normalisation des modes : %(k1)s

"""),

    6: _(u""" La bande de fréquence est vide.
"""),

    7: _(u"""
 NUME_ORDRE                    norme
"""),

    8: _(u""" %(i1)5d        %(k1)24s
"""),

    9: _(u"""
 NUME_ORDRE             ancienne norme              nouvelle norme
"""),

    10: _(u""" %(i1)5d        %(k1)24s    %(k2)24s
"""),

    11: _(u""" On saute la valeur propre numéro %(i1)d .
"""),

    12: _(u""" ALPHA = %(r1)12.5E
 BETA = %(r2)12.5E
"""),

    13: _(u""" Erreur directe LAPACK %(r1)12.5E
"""),

    14: _(u""" Elle correspond soit à un Lagrange soit à un DDL physique bloqué.
"""),

    15: _(u""" Fréquence     = %(r1)12.5E
 Amortissement = %(r2)12.5E
"""),

    16: _(u""" LAMBDA = %(r1)12.5E
"""),

    17: _(u"""  Le nombre total de DDL est       : %(i1)10d
  Le nombre de DDL de Lagrange est : %(i2)10d
  Le nombre de DDL actifs est      : %(i3)10d
"""),

    18: _(u"""  Le nombre total de DDL est               : %(i1)10d
  Le nombre de DDL de Lagrange est         : %(i2)10d
  Le nombre de DDL bloqués cinématiquement : %(i3)10d
  Le nombre de DDL actifs est              : %(i4)10d
"""),

    19: _(u""" Nombre de valeurs propres : %(i1)d
"""),

    20: _(u""" On ne peut pas remplir la composante %(k1)s du noeud numéro %(k2)s
 pour le champ "%(k3)s"
"""),

    21: _(u""" La combinaison linéaire de "%(k1)s" et "%(k2)s" est impossible.
 Cela est dû au fait qu'il manque des degrés de liberté dans  "%(k1)s" par
 rapport à "%(k2)s".
"""),

    22: _(u"""

       ===============================================
       =                                             =
       =          Opérateur CALC_ERC_DYN             =
       =                                             =
       =        Résolution d'un problème de          =
       =      minimisation d'une fonctionnelle       =
       =    d'erreur en relation de comportement     =
       =           en dynamique linéaire.            =
       =                                             =
       =        Formulation fréquentielle.           =
       =                                             =
       ===============================================
 
 Nombre de fréquences demandées    = %(i1)d
 
 Ordres de grandeur des matrices du problème:
 (informations combinant degrés de liberté physiques et de Lagrange)
 -------------------------------------------------------------------

 Dimension des matrices de masse (M) et raideur (K)  = %(i2)d x %(i2)d
 Termes non nuls des matrices de masse et raideur    = %(i3)d
 
 Dimension de la matrice d'observation (H)           = %(i4)d x %(i2)d
 Termes non nuls de la matrice d'observation         = %(i5)d
 Dimension de la matrice norme (G)                   = %(i4)d x %(i4)d
  
 Termes non nuls du sous-bloc H^T*G*H                = %(i6)d
 
 Dimension de la matrice du problème d'ERC           = %(i7)d x %(i7)d
 Termes non nuls de la matrice du problème d'ERC     = %(i8)d

 ===============================================

"""),   
}

