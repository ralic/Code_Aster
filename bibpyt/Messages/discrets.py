#@ MODIF discrets Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

def _(x) : return x

# Messages pour les éléments discrets non-linéaires
cata_msg={

1: _("""
élément discret %(k1)s .
Il n'y a pas de rotation non-linéaire possible.
"""),

2: _("""
élément discret %(k1)s .
Il n'y a pas de comportement non-lineaire possible suivant Z
ou en rotation autour de X,Y en 2D.
"""),

3: _("""
élément discret %(k1)s .
Il n'y a pas de comportement non-linéaire possible en rotation
ou suivant Z en 2D.
"""),

4: _("""
élément discret.
Le pas de temps est devenu trop petit : %(r1)12.5E .
"""),

5: _("""
élément discret %(k1)s .
Les caractéristiques sont obligatoirement données dans le repère local du discret.
"""),

6: _("""
pour les éléments discrets il faut définir un repère dans AFFE_CARA_ELEM
maille : %(k1)s
"""),

}
