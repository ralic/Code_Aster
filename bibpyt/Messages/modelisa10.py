#@ MODIF modelisa10 Messages  DATE 13/03/2012   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
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
# RESPONSABLE DELMAS J.DELMAS

cata_msg = {

1 : _(u"""
Le vecteur définissant l'axe de rotation a une composante non nulle suivant Ox ou Oz,
ce qui induit un chargement non axisymétrique. Avec une modélisation AXIS ou AXIS_FOURIER,
l'axe de rotation doit être dirigé suivant Oy.
"""),

2 : _(u"""
Les coordonnées du centre de rotation ont au moins une composante non nulle, ce qui induit
un chargement non axisymétrique. Avec une modélisation AXIS ou AXIS_FOURIER,
le centre de rotation doit être confondu avec l'origine.
"""),

3 : _(u"""
Le vecteur définissant l'axe de rotation a une composante non nulle suivant Ox ou Oy,
ce qui induit des forces centrifuges hors plan. Avec une modélisation C_PLAN ou D_PLAN,
l'axe de rotation doit être dirigé suivant Oz.
"""),


4 : _(u"""
Les mailles affectées à la modélisation TUYAU ne semblent pas former des lignes continues.
Il y a probablement un problème dans le maillage (superposition d'éléments par exemple).
Pour obtenir le détail des mailles affectées, utilisez INFO=2.
"""),

5 : _(u"""
Le quadrangle de nom %(k1)s est dégénéré : les cotés 1-2 et 1-3 sont colinéaires.
Reprenez votre maillage.
"""),

6 : _(u"""
Le modèle est de dimension %(i1)d . ARETE_IMPO s'applique sur des arêtes d'éléments 3D,
donc un modèle de dimension 3. Pour les arêtes d'éléments 2D utiliser FACE_IMPO.
"""),
7 : _(u"""
Il faut au moins un noeud esclave.
"""),

8 : _(u"""
Le groupe d'esclaves %(k1)s est vide.
"""),

9 : _(u"""
Le groupe du noeud maitre %(k1)s contient %(i1)d noeuds alors qu'il en faut un seul.
"""),

10 : _(u"""
Arguments incompatibles : il y a %(i1)d degrés de liberté esclaves mais %(i2)d noeuds esclaves.
"""),

11 : _(u"""
Le degré de liberté  %(k1)s est invalide.
"""),

12 : _(u"""
Arguments incompatibles : il y a %(i1)d degrés de liberté esclaves mais %(i2)d coefficients esclaves.
"""),

13 : _(u"""
Arguments incompatibles : il y a %(i1)d degrés de liberté esclaves mais %(i2)d noeuds esclaves.
"""),

14 : _("""
Pour un spectre de type SPEC_CORR_CONV_3, il faut donner le nom du
MODELE_INTERFACE dans PROJ_SPEC_BASE
"""),

}
