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

    35 : _(u"""
La loi de couplage <%(k1)s> n'est pas compatible avec la modélisation choisie <%(k2)s>.
"""),

    36 : _(u"""
Il y a déjà une loi de couplage.
"""),

    37 : _(u"""
Il y a déjà une loi hydraulique.
"""),

    38 : _(u"""
Il y a déjà une loi de mécanique.
"""),

    39 : _(u"""
Il manque la loi de couplage pour définir le kit <%(k1)s> .
"""),

    40 : _(u"""
Il manque la loi hydraulique pour définir le kit <%(k1)s> .
"""),

    41 : _(u"""
Il manque la loi mécanique pour définir le kit <%(k1)s> .
"""),

    42 : _(u"""
La loi de couplage <%(k1)s> est incorrecte pour une modélisation <%(k2)s>.
"""),

    43 : _(u"""
La loi hydraulique <%(k1)s> n'est pas compatible avec la loi mécanique <%(k2)s>.
"""),

    44 : _(u"""
La loi mécanique <%(k1)s> est incorrecte pour une modélisation <%(k2)s>.
"""),

    46 : _(u"""
Il y a une loi mécanique définie dans la relation, ce n'est pas possible avec la modélisation <%(k1)s> .
"""),

    59 : _(u"""
La loi de couplage doit être LIQU_SATU ou GAZ pour une modélisation <%(k1)s>.
"""),

}
