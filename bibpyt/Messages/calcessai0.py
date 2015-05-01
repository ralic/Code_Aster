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
# person_in_charge: albert.alarcon at edf.fr

cata_msg = {

    1: _(u"""
Le modèle mesuré doit être un concept de type DYNA_HARMO ou MODE_MECA.
"""),

    3: _(u"""
Calcul de MAC impossible : bases incompatibles.
"""),

    4: _(u"""
Problème inverse impossible : problème de cohérence entre les données.
"""),

    5: _(u"""
Problème de NUME_DDL dans MACRO_EXPANS : il est possible de le préciser
a l'appel de la macro-commande. Conséquence : erreur fatale possible dans les
opérations ultérieures (notamment l'opérateur MAC_MODE)
"""),

    6: _(u"""
Si vous n'avez pas sélectionné de NUME_ORDRE ou de NUME_MODE dans %(k1)s.
Il ne faut pas déclarer de concept en sortie de type %(k2)s.
Cela risque de causer une erreur fatale par la suite.
"""),

    7: _(u"""
Erreur dans MACRO_EXPANS
"""),

    8: _(u"""
Impossible de trouver le modèle associe a la base de modes %(k1)s.
Cela peut empêcher certains calculs de se dérouler normalement.
"""),

    9: _(u"""
Les mots-clés MATR_RIGI et MATR_MASS n'ont pas été renseignés dans OBSERVATION.
Sans ces matrices, certains calculs (par exemple : calcul d'expansion, de MAC, etc.)
ne seront pas possibles.
"""),

    10: _(u"""
Le modèle associé aux matrices MATR_RIGI et MATR_MASS doit être le même que MODELE_2.
"""),

    13: _(u"""
Le résultat expérimental est un DYNA_HARMO : il n'est pas possible d'en extraire
des numéros d'ordre avec MACRO_EXPANS. Le mots-clés NUME_MODE et NUME_ORDRE
sont ignorés.
"""),

    14: _(u"""
Erreur dans le calcul de MAC : le NUME_DDL associé à la base %(k1)s
n'existe pas. Si cette base a été créée avec PROJ_CHAMP, ne pas oublier
de mentionner explicitement le NUME_DDL de la structure de données résultat
avec le mot-clé NUME_DDL.
"""),


}
