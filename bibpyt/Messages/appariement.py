# coding=utf-8
#            CONFIGURATION MANAGEMENT OF EDF VERSION
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

cata_msg = {

1 : _(u"""
Aucun noeud n'est apparié.
La définition de vos surfaces ou de vos paramètres d'appariement est incorrecte.
Vérifier TOLE_PROJ_EXT et/ou la définition de vos surfaces.
Vérifier les déplacements induits par votre modélisation.
Cette alarme peut entraîner des résultats faux si elle apparaît dans la résolution du contact en Newton généralisé.
"""),

13 : _(u"""
L'algorithme de Newton a échoué lors de la projection du point de coordonnées
  (%(r1)f,%(r2)f,%(r3)f)
sur la maille %(k1)s.
Erreur de définition de la maille ou projection difficile.
"""),

14 : _(u"""
Les vecteurs tangents sont nuls au niveau du projeté du noeud %(k2)s sur la maille %(k1)s.
Erreur de définition de la maille ou projection difficile.
"""),

15 : _(u"""
Le vecteur normal est nul au niveau du noeud %(k2)s sur la maille %(k1)s.
Erreur de définition de la maille ou projection difficile.
"""),

16 : _(u"""
Le vecteur normal résultant est nul au niveau du noeud %(k1)s.
Erreur de définition de la maille ou projection difficile.
"""),

17 : _(u"""
Les vecteurs tangents résultants sont nuls au niveau du noeud %(k1)s.
Erreur de définition de la maille ou projection difficile.
"""),

34 : _(u"""
Échec de l'orthogonalisation du repère tangent construit au niveau du projeté du point de coordonnées
  (%(r1)f,%(r2)f,%(r3)f)
sur la maille %(k1)s,
Erreur de définition de la maille ou projection difficile. Contactez l'assistance dans ce dernier cas.
"""),

36 : _(u"""
La maille %(k1)s est de type 'POI1', ce n'est pas autorisé sur une maille maître dans le cas d'un appariement MAIT_ESCL.
"""),

38 : _(u"""
La maille %(k1)s est de type poutre et sa tangente est nulle.
Vérifiez votre maillage.
"""),

61 : _(u"""
La maille %(k1)s est de type poutre, elle nécessite la définition d'une base locale.
"""),

62 : _(u"""
La maille %(k1)s est de type 'POI1', elle nécessite la définition explicite de sa normale.
"""),

63 : _(u"""
La maille %(k1)s est de type 'POI1', elle nécessite la définition explicite d'une normale non nulle.
"""),

75 : _(u"""
Un élément de type POI1 ne peut pas être une maille maître.
"""),


}
