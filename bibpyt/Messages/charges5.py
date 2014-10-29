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

cata_msg = {

    1 : _(u"""
 On ne peut pas avoir simultanément une charge de type VECT_ASSE et une charge du type CHARGE.
"""),

    2 : _(u"""
 La charge de nom <%(k1)s> est en double.
"""),

    3 : _(u"""
 La charge de nom <%(k1)s> n'est pas autorisée dans la commande <%(k2)s>.
"""),

    4 : _(u"""
 La charge de nom <%(k1)s> se base sur un PHENOMENE non pris en charge dans la commande.
"""),

    5 : _(u"""
 La charge de nom <%(k1)s> se base sur un MODELE différent de la commande.
"""),

    6 : _(u"""
 Les charges n'ont pas le même MODELE.
"""),

    7 : _(u"""
 La charge de nom <%(k1)s> ne peut pas être suiveuse.
 Les charges de type Dirichlet par élimination (AFFE_CHAR_CINE) ne peuvent pas être des charges suiveuses.
"""),

    8 : _(u"""
 La charge de nom <%(k1)s> ne peut pas être DIDI.
 Les charges de type Dirichlet par élimination (AFFE_CHAR_CINE) ne peuvent pas être des charges DIDI.
"""),

    9 : _(u"""
 La charge de nom <%(k1)s> ne peut pas être pilotée.
 Les charges de type Dirichlet par élimination (AFFE_CHAR_CINE) ne peuvent pas être des charges pilotées.
"""),

    10 : _(u"""
 La charge de nom <%(k1)s> ne peut pas être suiveuse.
 Les charges de type Dirichlet ne peuvent pas être des charges suiveuses.
"""),

    11 : _(u"""
 La charge de nom <%(k1)s> ne peut pas être pilotée.
 Les charges de type EVOL_CHAR ne peuvent pas être des charges pilotées.
"""),

}
