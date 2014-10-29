# coding=utf-8
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

    1: _(u"""
 l option :  %(k1)s  est probablement composée (vieillot)
"""),

    2: _(u"""
 l option :  %(k1)s  a plusieurs paramètres de mêmes noms.
"""),

    3: _(u"""
 mode local incorrect
 pour le paramètre:  %(k1)s
 pour l'option    :  %(k2)s
 pour le type     :  %(k3)s
"""),

    4: _(u"""
 le paramètre :  %(k1)s  pour l'option :  %(k2)s
 existe pour le type :  %(k3)s mais n'existe pas dans l'option.
"""),

    5: _(u"""
 le paramètre :  %(k1)s  pour l'option :  %(k2)s  et pour le TYPE_ELEMENT :  %(k3)s
 n'est pas associe à la bonne grandeur.
"""),

    6: _(u"""
 le paramètre :  %(k1)s  pour l'option :  %(k2)s  et pour le TYPE_ELEMENT :  %(k3)s
 n'a pas le bon nombre de noeuds.
"""),

    7: _(u"""
 le paramètre :  %(k1)s  pour l option :  %(k2)s  et pour le TYPE_ELEMENT :  %(k3)s
 n'est pas du bon type:  %(k4)s
"""),

    8: _(u"""
 les grandeurs : %(k1)s  et  %(k2)s  doivent avoir exactement les mêmes composantes.
"""),

    9: _(u"""
 erreurs de cohérence dans les catalogues d'éléments finis.
"""),

    20: _(u"""
 Erreur lors de l'accès à la composante %(i1)d dans le champ de nom %(k1)s et de type %(k2)s.
 Les arguments sont hors bornes ou la composante est déjà affectée (écrasement).
 Contactez le support.
"""),

}
