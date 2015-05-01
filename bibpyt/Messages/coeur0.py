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

cata_msg = {
    1 : _(u"""
Le mot-clé MAILLAGE_N n'est pas utilisé. Le maillage considéré est celui provenant
de RESU_INIT.
"""),

    2 : _(u"""
Assemblage des %(i1)d permutations
"""),

    3 : _(u"""
Permutation de l\'assemblage %(k1)s en position %(k2)s
"""),

    4 : _(u"""
Récupération des jeux entre les assemblages
"""),

    5 : _(u"""
Récupération des jeux entre les assemblages de bord et le cloisonnement
"""),

    6 : _(u"""
Post-traitement des déformations des assemblages combustibles
"""),

    7 : _(u"""
Il faut renseigner au moins un des mots-clés MAILLAGE_N ou RESU_INIT.
"""),

}
