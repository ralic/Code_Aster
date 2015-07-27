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
# person_in_charge: jean-luc.flejou at edf.fr

cata_msg = {

# Messages dans OP0019
    1 : _(u"""AFFE_CARA_ELEM
Le modèle contient des noeuds tardifs, ils ne sont pas autorisés dans cette commande.

Les noeuds tardifs correspondent à des noeuds ou groupes de noeuds sur lesquels vous
avez affecté un modèle avec la commande AFFE_MODELE/AFFE/NOEUD ou GROUP_NO.

Solution : il faut créer un groupe de maille de POI1 correspondant à ces noeuds à l'aide
           de la commande CREA_MAILLAGE/CREA_POI1 et ensuite affecter sur ce groupe de maille
           un modèle avec la commande AFFE_MODELE/AFFE/GROUP_MA
"""),

    2 : _(u"""AFFE_CARA_ELEM
Aucune affectation n'est réalisée sur des GROUP_MA ou des MAILLES.

Vérifiez vos données.
"""),

# Messages dans ace_mass_rep
    10 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Les mailles dans le groupe <%(k2)s> ne sont pas toutes de même dimension topologique.

Les mailles de <%(k2)s> doivent être des segments ou des éléments de surfaces.
Il ne faut pas mélanger les dimensions topologique.
Pour information :
 - dimension topologique de la 1ère maille du groupe : %(i2)d
 - maille incriminée                                 : %(k3)s
 - dimension topologique de la maille incriminée     : %(i3)d
 - type de la maille incriminée                      : %(k4)s

Vérifiez vos données.
"""),

    11 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Une maille dans le groupe  <%(k2)s> n'a pas la bonne dimension topologique.

Les mailles de <%(k2)s> doivent être des segments ou des éléments de surfaces.
Pour information :
 - maille concernée         : %(k3)s
 - sa dimension topologique : %(i2)d
 - son type                 : %(k4)s

Vérifiez vos données.
"""),

    12 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
La maille %(k3)s est présente au moins 2 fois dans la définition de la surface.

Les mailles ne doivent être présente qu'une seule fois, en cas de doublon :
- La surface totale de répartition ne peut pas être correctement déterminée.
- La contribution de cette maille n'est pas correcte.

Vérifiez vos données.
"""),

    13 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
La maille %(k3)s présente dans <%(k2)s> n'a pas le bon nombre de noeuds.
Les mailles gérées doivent avoir %(k5)s noeuds

Pour information :
 - maille concernée              : %(k3)s
 - son type                      : %(k4)s
 - nombre de noeuds de la maille : %(i2)d

Vérifiez vos données.
"""),

    14 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Au moins un élément appartenant au GROUP_MA_POI1 <%(k2)s> n'est pas connecté aux mailles
surfaciques du GROUP_MA.

Pour pouvoir calculer la contribution de la maille, il faut que tous les noeuds de <%(k2)s> soient
en relation avec un seul noeud d'une maille surfacique du GROUP_MA.

Pour information :
 - maille détectée : %(k3)s

Vérifiez vos données.
"""),

    15 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Au moins un noeud appartenant à un élément du GROUP_MA surfacique n'est pas connecté à une maille du
GROUP_MA_POI1.

Pour pouvoir répartir la contribution des mailles surfaciques, il faut que tous les noeuds
soient en relation avec une et une seule maille de GROUP_MA_POI1.

Pour information :
 - noeud concerné : %(k2)s

Vérifiez vos données.
"""),

    16 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Des éléments appartenant au GROUP_MA_POI1 <%(k2)s> n'ont pas le bon nombre de noeuds.
Les mailles doivent être de type POI1 et posséder 1 seul noeud.

Pour information :
 - maille détectée : %(k3)s

Vérifiez vos données.
"""),

    17 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Les mailles appartenant au GROUP_MA sont surfaciques. Vous ne devez pas utiliser
TYPE=LINEIQUE mais TYPE=SURFACIQUE ou TOTALE.

Vérifiez vos données.
"""),

    18 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Les mailles appartenant au GROUP_MA sont linéiques. Vous ne devez pas utiliser
TYPE=SURFACIQUE mais TYPE=LINEIQUE ou TOTALE.

Vérifiez vos données.
"""),

19 : _(u"""AFFE_CARA_ELEM / <%(k1)s> / occurrence %(i1)d
Le noeud <%(k5)s> de la maille <%(k3)s> appartenant au GROUP_MA_POI1 <%(k2)s> est connecté
avec un noeud d'une maille surfacique du GROUP_MA qui est déjà connecté à la maille
<%(k4)s> du GROUP_MA_POI1.

Les mailles de <%(k2)s> ne doivent être connecté qu'a un seul noeud des mailles de surface.
Les noeuds des mailles de surface ne doivent être connecté qu'a un seul noeud des mailles POI1.
La relation doit être bijective.

Vérifiez vos données.
"""),

}
