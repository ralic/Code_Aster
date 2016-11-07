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

    2: _(u"""
%(k1)s: FREQ_MAX < FREQ_MIN
"""),

    3: _(u"""
Erreur dans les indices.
"""),

    4: _(u"""
Le fichier %(k1)s est introuvable.
"""),

    5: _(u"""
La dimension DIM n est pas précisée dans le fichier lu.
"""),

    6: _(u"""
Nombre de fonctions incorrect.
"""),

    7: _(u"""
Erreur dans les données de fonctions.
"""),

    9: _(u"""
Le fichier IDEAS est vide ou ne contient pas le data set demande
"""),

    10: _(u"""
Un des data sets 58 contient une donnée qui n'est pas un interspectre
"""),

    11: _(u"""
On ne traite pas les cas ou les abscisses fréquentielles ne sont pas régulièrement espacées
"""),

    12: _(u"""
Le mot-clé format correspond au format du fichier source, qui peut être 'ASTER' ou 'IDEAS' (pour lire les DS58)
"""),

    13: _(u"""
Le calcul en multi-appuis n'est réalisable que lorsque le concept résultat renseigné sous le mot-clé RESU est RESU_GENE.
"""),

14: _(u"""
Les listes données pour AMOR_EQUIP, FREQ_EQUIP et COEF_MASS_EQUIP doivent être de même longueur
"""),

15: _(u"""
La somme des rapport de masses COEF_MASS_EQUIP doit être égale à 1
"""),

16: _(u"""
La valeur initiale du signal d'entrée dépasse le critère limite TOLE_INIT choisi %(r1)f > %(r2)f
"""),

}
