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

cata_msg={


1: _(u"""
  Recherche des modes rigides de la matrice %(k1)s.
"""),

2: _(u"""
  Matrice symétrique
"""),

3: _(u"""
La matrice %(k1)s n'est pas symétrique.
Pour l'instant, la recherche des modes de corps rigide n'a pas été développée
pour une matrice non symétrique.
"""),

4: _(u"""
  Matrice à valeurs réelles.
"""),

5: _(u"""
La matrice %(k1)s est à valeurs complexes.
Pour l'instant, la recherche des modes de corps rigide n'a pas été développée
pour une matrice à valeurs complexes.
"""),

6: _(u"""
Pivot nul détecté à la ligne %(i1)d.
Le degré de liberté correspondant est le suivant:
"""),

7: _(u"""
%(i1)d modes de corps rigide ont été détectés.
"""),

8: _(u"""
Attention : plus de six modes de corps rigide ont été détectés.

--> Conseil :
Si vous pensez avoir une seule structure dans le modèle, cela peut provenir de noeud(s) orphelin(s). Dans ce cas, vérifiez le maillage.
"""),

9: _(u"""
  Factorisation de la matrice %(k1)s avec la méthode %(k2)s.
"""),

10: _(u"""
  Matrice non-symétrique
"""),

11: _(u"""
  Matrice à valeurs complexes.
"""),

12: _(u"""
  Résultats de la factorisation de la matrice %(k1)s.
"""),

13: _(u"""
  La matrice n'est pas définie positive et comporte %(i1)d zéros sur la diagonale.
"""),

14: _(u"""
    Nombre maximum de décimales à perdre : %(i1)d
    Nombre de décimales perdues          : %(i2)d
    Numéro de la pire équation           : %(i3)d
    Nombre de pivots négatifs            : %(i4)d
    Code arrêt                           : %(i5)d
    Code retour                          : %(i6)d
"""),

15: _(u"""
Problème lors de la factorisation de la matrice: 
    Le pivot devient très grand à la ligne %(i1)d qui correspond au degré de liberté donné ci-dessus.
    On a perdu %(i2)d décimales.
"""),
}
