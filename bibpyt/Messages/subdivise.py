# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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

# Pour la méthode de subdivision

cata_msg={

1:  _(u"""          On utilise la découpe manuelle."""),

2:  _(u"""          On utilise la découpe automatique."""),

3:  _(u"""          On ne peut pas découper le pas de temps car la fonctionnalité n'est pas activée.
                    Pour activer la découpe du pas de temps, utilisez la commande DEFI_LIST_INST."""),

10: _(u"""          Découpe uniforme à partir de l'instant <%(r1)19.12e> en <%(i1)d> pas de temps.
                    (soit un incrément constant de <%(r2)19.12e>)"""),

11: _(u"""          Découpe non uniforme à partir de l'instant <%(r1)19.12e> en <%(i1)d> pas de temps.
                    (avec un incrément initial de <%(r2)19.12e>, puis des incréments de <%(r3)19.12e>)"""),

12: _(u"""          Découpe non uniforme à partir de l'instant <%(r1)19.12e> en <%(i1)d> pas de temps.
                    (avec des incréments de <%(r2)19.12e>, puis un incrément final de <%(r3)19.12e>)"""),

13: _(u"""          On tente de découper les pas de temps au delà de l'instant <%(r1)19.12e>, pendant une durée de <%(r2)19.12e>."""),

14: _(u"""          On a découpé les pas de temps jusqu'à l'instant <%(r1)19.12e>."""),

15: _(u"""          On ne découpe pas les pas de temps au delà de l'instant <%(r1)19.12e>.
                    Les incréments de temps au delà de cet instant sont tous plus petits que SUBD_INST."""),

16: _(u"""          Le pas de temps minimum <%(r1)19.12e> (SUBD_PAS_MINI) est atteint."""),

17: _(u"""          Le nombre maximal <%(i1)d> de niveaux de subdivision est atteint."""),

18: _(u"""          La découpe sera maintenue au delà de l'instant <%(r1)19.12e>, pendant une durée de <%(r2)19.12e>."""),

50: _(u""" <Action><Échec> Échec dans la tentative de découper le pas de temps."""),

51: _(u""" <Action> On découpe le pas de temps."""),

52: _(u""" <Action> On ne découpe pas le pas de temps."""),

53: _(u""" <Action><Échec> Le pas de temps est devenu trop petit: %(r1)19.12e"""),

99: _(u"""Avec PREDICTION = 'DEPL_CALCULE', la subdivision du pas de temps n'est pas autorisée. """),

}
