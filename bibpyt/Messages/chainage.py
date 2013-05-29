# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
#
# person_in_charge: sylvie.granet at edf.fr

cata_msg = {

1 : _(u"""
Vous cherchez à faire du chaînage HM avec une modélisation Thermo-hydro-mécanique comportant de la mécanique.
Le chaînage est donc inutile !
"""),

2 : _(u"""
Le champ d'entrée div(u) est mal construit. Il manque soit l'instant actuel soit l'instant précédent de div(u).
"""),

3 : _(u"""
Vous n'êtes pas sur une modélisation autorisée pour faire du chaînage.
Le chaînage ne fonctionne pas sur la modélisation %(k1)s.

Conseil : Vérifiez que votre modélisation %(k2)s est sans mécanique
"""),

4 : _(u"""
Vous n'êtes pas sur une modélisation autorisée pour faire du chaînage.
Le chaînage ne fonctionne pas sur la modélisation %(k1)s.

Conseil : Vérifiez que votre modélisation %(k2)s est 'D_PLAN' ou '3D'
ou une modélisation THM
"""),

5 : _(u"""
Il n'est pas possible de faire du chaînage avec un coefficient d'emmagasinement
non nul.
"""),

6 : _(u"""
L'instant %(r1)e spécifié en entrée doit être supérieur au dernier
instant trouvé dans la SD résultat %(k1)s.
"""),

7 : _(u"""
  Impression du champ %(k1)s à l'instant %(r1)e sur le modèle %(k2)s
"""),

8 : _(u"""
  Les modélisations THM n'ont de sens qu'en petites déformations.
  Choisissez COMP_INCR/DEFORMATION='PETIT'.
"""),

9 : _(u"""
  Vous n'avez pas choisi une loi de comportement mécanique autorisée pour les
  modélisations THM : %(k1)s
"""),

10 : _(u"""
  Si vous faites du chaînage, il ne faut qu'un seul et unique modèle dans le résultat. Si vous en voulez plusieurs, faites une demande d'évolution.
"""),

11 : _(u"""
  On a trouvé une évolution de variable de commandes PTOT incomplète. Il manque un instant.
"""),

}
