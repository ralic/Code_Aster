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

# Pour la méthode ITER_SUPPL de DEFI_LIST_INST

cata_msg = {


    2: _(u"""          On ne peux pas tenter d'autoriser des itérations de Newton supplémentaires car on dépasse déjà le nombre
                   d'itérations maximum autorisé <%(i1)d>.
"""),

    3: _(u"""          On estime qu'on va pouvoir converger en <%(i1)d> itérations de Newton."""),

    4: _(u"""          L'extrapolation des résidus donne un nombre d'itérations inférieur à ITER_GLOB_ELAS et ITER_GLOB_MAXI.
                   Cela peut se produire si vous avez donné ITER_GLOB_ELAS inférieur à ITER_GLOB_MAXI et que l'extrapolation du
                   nombre d'itérations est faite en régime élastique."""),


    5: _(u"""          L'extrapolation des résidus donne un nombre d'itérations <%(i1)d> supérieur au maximum autorisé <%(i2)d>"""),

    6: _(u""" <Action><Échec> Échec dans la tentative d'autoriser des itérations de Newton supplémentaires."""),

    7: _(u""" <Action> On autorise des itérations de Newton supplémentaires."""),

}
