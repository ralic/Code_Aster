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
La charge %(k1)s a été utilisée plus d'une fois dans EXCIT: il faut la supprimer.
"""),

    2 : _(u"""
Il n'y a aucune charge dans le mot-clef facteur EXCIT. Ce n'est pas possible avec STAT_NON_LINE.
"""),

    3 : _(u"""
La charge %(k1)s n'a pu être identifiée. Cette erreur est probablement due à l'utilisation d'un
mot-clef facteur vide dans l'opérateur AFFE_CHAR_MECA, AFFE_CHAR_THER ou AFFE_CHAR_ACOU.
"""),

    20 : _(u"""
La charge %(k1)s  n'est pas compatible avec FONC_MULT.
"""),

    21 : _(u"""
La charge %(k1)s n'est pas thermique.
"""),

    22 : _(u"""
La charge %(k1)s n'est pas mécanique.
"""),

    23 : _(u"""
La charge %(k1)s a été déclarée comme étant suiveuse alors que ce n'est pas possible.
Si votre chargement contient plusieurs types dont certains ne peuvent être suiveurs, il faut les séparer.
Certains chargements ne peuvent être suiveurs s'ils sont dépendant du temps.
"""),

    24 : _(u"""
La charge %(k1)s est de type cinématique (AFFE_CHAR_CINE):
 elle ne peut pas être différentielle.
"""),

    26 : _(u"""
La charge %(k1)s a été déclarée comme étant pilotable alors que ce n'est pas possible.
Si votre chargement contient plusieurs types dont certains ne peuvent être pilotables, il faut les séparer.
"""),


    27 : _(u"""
La charge %(k1)s est de type cinématique (AFFE_CHAR_CINE):
 elle ne peut pas être pilotée.
"""),

    28 : _(u"""
On ne peut pas piloter la charge %(k1)s car c'est une charge fonction du temps.
"""),

    29 : _(u"""
Il y a trop de chargements de type Dirichlet suiveur.
"""),

    30 : _(u"""
Erreur utilisateur :
  Le chargement contient des relations cinématiques qui sont non-linéaires
  lorsque l'on utilise EXCIT / TYPE_CHARGE='SUIV'.
  Le code ne sait pas encore traiter ces relations non linéaires.
"""),



    34 : _(u"""
La charge de type EVOL_CHAR %(k1)s  ne peut pas être pilotée.
"""),

    38 : _(u"""
La charge %(k1)s ne peut pas utiliser de fonction multiplicatrice FONC_MULT
 car elle est pilotée.
"""),

    39 : _(u"""
On ne peut pas piloter en l'absence de forces de type FIXE_PILO.
"""),

    40 : _(u"""
On ne peut piloter plus d'une charge.
"""),

    50 : _(u"""
Le chargement FORCE_SOL n'est utilisable qu'en dynamique.
"""),

    51 : _(u"""
Le chargement FORCE_SOL ne peut pas être de type suiveur
"""),

    52 : _(u"""
Le chargement FORCE_SOL ne peut pas être de type Dirichlet différentiel.
"""),

    53 : _(u"""
Le chargement FORCE_SOL ne peut pas être une fonction.
"""),

    54 : _(u"""
Le chargement FORCE_SOL ne doit pas avoir de fonction multiplicatrice.
"""),



}
