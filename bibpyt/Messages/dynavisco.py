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

cata_msg={

1: _(u"""
Dans le cas TYPE_RESU='MODE',
la liste de fréquences donnée sous le mot-clé FREQ ou LIST_FREQ
doit contenir exactement 2 valeurs.
"""),

2: _(u"""
Dans le cas TYPE_RESU='HARM',
la liste de fréquences donnée sous le mot-clé FREQ ou LIST_FREQ
doit contenir au moins 2 valeurs.
"""),

3: _(u"""
Les modes propres finaux sont :
"""),

4: _(u"""
numéro    fréquence (HZ)
"""),

5: _(u""" %(i1)4d      %(r1)12.5E """),

6: _(u"""
numéro    fréquence (HZ)     amortissement
"""),

7: _(u""" %(i1)4d      %(r1)12.5E       %(r2)12.5E """),

8: _(u"""
Aucune excitation au second membre n'a été trouvée.
Pour le moment, l'opérateur DYNA_VISCO supporte uniquement
un second membre de type FORCE_NODALE.
"""),

}
