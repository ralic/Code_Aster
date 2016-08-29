# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
 On ne peut pas affecter des moments répartis sur des éléments de type %(k1)s.
"""),

    2 : _(u"""

 L'option %(k1)s n'est pas développée avec le comportement %(k2)s pour les éléments %(k3)s.

"""),

    3 : _(u"""

 L'option MASS_FLUI_STRU n'est pas disponible pour les POU_D_TGM en multi-matériaux.
 Vous ne pouvez définir qu'un seul groupe de fibres.

"""),

    10 : _(u"""
 on ne peut pas affecter la modélisation "AXIS_DIAG" aux éléments de l'axe
"""),

    11 : _(u"""
  -> Attention vous avez une loi de comportement inélastique et vous êtes
     en contraintes planes, la composante du tenseur de déformations EPZZ que
     vous allez calculer n'est valable que tant que vous restez dans le
     domaine élastique. Les autres composantes EPXX, EPYY, EPXY sont correctes.
  -> Risque & Conseil :
     Si le comportement est effectivement non linéaire, il ne faut pas utiliser
     la valeur de EPZZ calculée par cette option.
"""),

    13 : _(u"""
  Les composantes SIXZ et SIYZ du champs de contraintes sont nulles pour les
  éléments DKT et TUYAU. Le calcul des composantes EPXZ et EPYZ du champs de déformations
  anélastiques donnerait des valeurs fausses. Ces valeurs sont donc mises
  à zéro et ne doivent pas être prises en compte.
"""),

    16 : _(u"""
 Comportement: %(k1)s non implanté
"""),

    17 : _(u"""
 Le matériau  %(k1)s  n'est pas connu
 Seuls sont admis les matériaux  'THER' et 'THER_COQUE' pour les coques thermiques
"""),

    18 : _(u"""
 Le matériau  %(k1)s  n'est pas connu
 Seuls sont admis les matériaux  'THER' et 'THER_COQUE' pour le calcul des flux pour les coques thermiques
"""),

    26 : _(u"""
 Mauvaise définition des caractéristiques de la section
"""),

    30 : _(u"""
 Section non tubulaire pour MASS_FLUI_STRU
"""),

    31 : _(u"""
 Pas de valeur utilisateur pour RHO
"""),

    34 : _(u"""
 Seules les forces suiveuses de type vent définies par un EVOL_CHAR sont autorisées
"""),

    35 : _(u"""
 Un champ de vitesse de vent est imposé sans donner un Cx dépendant de la vitesse sur une des barres
"""),

    36 : _(u"""
 COMPORTEMENT non valide
"""),

    37 : _(u"""
  Relation :  %(k1)s  non implantée sur les câbles
"""),

    38 : _(u"""
  Déformation :  %(k1)s  non implantée sur les câbles
"""),

    39 : _(u"""
 un champ de vitesse de vent est impose sans donner un Cx dépendant de la vitesse sur un des câbles.
"""),

    40 : _(u"""
 DEFORMATION %(k1)s non programmée.
 Seules les déformations PETIT et GROT_GDEP sont autorisées avec les
 éléments de type %(k2)s.
"""),










    49 : _(u"""
 anisotropie non prévue pour coque1d
"""),

    50 : _(u"""
 nombre de couches limite a 30 pour les coques 1d
"""),

    51 : _(u"""
 Le nombre de couches défini dans DEFI_COMPOSITE et dans AFFE_CARA_ELEM dans n'est pas cohérent.
 Nombre de couches dans DEFI_COMPOSITE: %(i1)d
 Nombre de couches dans AFFE_CARA_ELEM: %(i2)d
"""),

    52 : _(u"""
 L'épaisseur totale des couches définie dans DEFI_COMPOSITE et celle définie dans AFFE_CARA_ELEM ne sont pas cohérentes.
 Épaisseur totale des couches dans DEFI_COMPOSITE: %(r1)f
 Épaisseur dans AFFE_CARA_ELEM: %(r2)f
"""),

    54 : _(u"""
  la réactualisation de la géométrie (déformation : PETIT_REAC) est déconseillée pour les éléments de coque_1d.
"""),

    55 : _(u"""
 nombre de couches limite à 10 pour les coques 1d
"""),

    56 : _(u"""
 valeur utilisateur de RHO nulle
"""),

    59 : _(u"""
  le coefficient de poisson est non constant. la programmation actuelle n en tient pas compte.
"""),

    61 : _(u"""
 loi  %(k1)s  indisponible pour les POU_D_E/d_t
"""),

    62 : _(u"""
 Noeuds confondus pour un élément de barre
"""),

    63 : _(u"""
 ne pas utiliser THER_LINEAIRE avec des éléments de Fourier mais les commandes développées
"""),

    67 : _(u"""
 Élément dégénéré :
 revoir le maillage
"""),

    74 : _(u"""
 pour l'option "RICE_TRACEY", la relation " %(k1)s " n'est pas admise
"""),

    76 : _(u"""
Couplage fluage/fissuration :
La loi BETON_DOUBLE_DP ne peut être couplée qu'avec une loi de fluage de GRANGER
"""),

    80 : _(u"""
 situation de contact impossible
"""),

    85 : _(u"""
La relation %(k1)s  non implantée sur les éléments "POU_D_T_GD"
"""),

    86 : _(u"""
  déformation :  %(k1)s  non implantée sur les éléments "POU_D_T_GD"
"""),

    87 : _(u"""
On ne trouve pas RHO, qui est nécessaire en dynamique
"""),

    91 : _(u"""
  calcul de la masse non implanté pour les éléments COQUE_3D en grandes rotations, déformation : GROT_GDEP
"""),

    93 : _(u"""
  déformation :  %(k1)s  non implantée sur les éléments COQUE_3D en grandes rotations
  déformation : GROT_GDEP obligatoirement
"""),

    94 : _(u"""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC') est déconseillée pour les éléments COQUE_3D.
  -> Risque & Conseil :
     Le calcul des déformations à l'aide de PETIT_REAC n'est qu'une
     approximation des hypothèses des grands déplacements. Elle nécessite
     d'effectuer de très petits incréments de chargement. Pour prendre en
     compte correctement les grands déplacements et surtout les grandes
     rotations, il est recommandé d'utiliser DEFORMATION='GROT_GDEP'.

"""),

    98 : _(u"""
 comportement coeur homogénéise inexistant
"""),

    99 : _(u"""
Seules les poutres à section constante sont admises !
"""),

}
