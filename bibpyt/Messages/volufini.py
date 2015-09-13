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

    2 : _(u"""
  -> le sommet de numéro global %(i1)i n appartient pas
  -> a la maille %(i2)i
"""),

    3 : _(u"""
  -> Nombre de voisins %(i1)i trop grand
"""),

    4 : _(u"""
  -> Nombre de sommets communs %(i1)i trop grand
"""),

    5 : _(u"""
  -> Le nombre de mailles %(i1)i est inférieur a 1
"""),
    6 : _(u"""
  -> Le type de voisinage %(k1)s est inconnu
"""),
    7 : _(u"""
  -> Le type de voisinage %(k1)s a une longueur %(i1)i trop grande
"""),
    8 : _(u"""
  -> La loi de comportement  %(k1)s est inconnu
"""),
    9 : _(u"""
  -> Le type de modélisation volumes finis (TYPVF)  %(i1)i   est inconnu.
     Vous avez le choix entre :
                          TYPVF=1  => Schéma VF a deux points (supprimée)
                          TYPVF=2  => Schéma SUDM (supprimée)
                          TYPVF=3  => Schéma SUDA
                          TYPVF=4  => Schéma SUC (supprimée)

"""),
    10 : _(u"""
  -> Le nom de la modélisation  %(k1)s  est inconnu
"""),
    11 : _(u"""
  -> L'option  %(k1)s est inconnue
"""),
    12 : _(u"""
  -> ELREFE  %(k1)s   inconnue : En 3D et en VF on peut utiliser uniquement des hexaèdres
    a 27 DDL et des tétraèdres a 27 DDL.
"""),
    13 : _(u"""
  -> l'élément %(k1)s et la face  %(i1)i est non plane
"""),

}
