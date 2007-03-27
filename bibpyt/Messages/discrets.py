#@ MODIF discrets Messages  DATE 28/03/2007   AUTEUR PELLET J.PELLET 
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
element discret %(k1)s . Il n'y a pas de rotation non-lineaire possible.
"""),

2: _("""
element discret %(k1)s . Il n'y a pas de comportement\n
non-lineaire possible suivant z ou en rotation autour de x,y en 2d.
"""),

3: _("""
element discret %(k1)s . Il n'y a pas de comportement\n
non-lineaire possible en rotation ou suivant z en 2d.
"""),

4: _("""
element discret. Le pas de temps est devenu trop petit : %(r1)12.5E .
"""),

5: _("""
element discret %(k1)s . Les caracteristiques sont obligatoirement\n
donnees dans le repere local du discret..
"""),

6: _("""
pour les elements discrets il faut definir un repere dans affe_cara_elem\n
maille : %(k1)s
"""),

}




