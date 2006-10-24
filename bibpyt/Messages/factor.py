#@ MODIF factor Messages  DATE 23/10/2006   AUTEUR VABHHTS J.PELLET 
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

cata_msg={


10: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
"""),

11: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
"""),


20: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
  pour le noeud %(k1)s et la composante %(k2)s
"""),

21: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  pour le noeud %(k1)s et la composante %(k2)s
"""),


30: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
  pour le noeud tardif %(k1)s et la composante %(k2)s
  Il s'agit sans doute d'une relation de blocage surabondante.
  blocage concerné : %(k4)s
"""),

31: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  pour le noeud tardif %(k1)s et la composante %(k2)s
  Il s'agit sans doute d'une relation de blocage surabondante.
  blocage concerné : %(k4)s
"""),


40: _("""
Matrice non factorisable :
  pivot presque nul à la ligne : %(i1)d
  nombre de décimales perdues  : %(i2)d
  pour le noeud tardif %(k1)s et la composante %(k2)s
  Il s'agit sans doute d'une relation entre ddls surabondante.
  La relation concerne les noeuds imprimés ci-dessus dans le fichier MESSAGE.
"""),

41: _("""
Matrice non factorisable :
  pivot vraiment nul à la ligne : %(i1)d
  pour le noeud tardif %(k1)s et la composante %(k2)s
  Il s'agit sans doute d'une relation entre ddls surabondante.
  La relation concerne les noeuds imprimés ci-dessus dans le fichier MESSAGE.
"""),

51: _("""
Solveur MUMPS interdit ici.
Causes possibles :
  - contact/frottement discret
  - STAT_NON_LINE / FLAMBEMENT
"""),

52: _("""
Solveurs LDLT et MULT_FRONT seuls permis ici.
Causes possibles :
  - contact/frottement discret
  - STAT_NON_LINE / FLAMBEMENT
"""),

}
