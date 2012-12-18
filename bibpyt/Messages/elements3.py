#@ MODIF elements3 Messages  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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

12 : _(u"""
 Calcul de G bilinéaire
 E, NU, ALPHA dépendent de la température
 Les champs de température (TGU et TGV) sont différents
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

19 : _(u"""
 L'option  %(k1)s  n'est disponible qu'avec des éléments TETRA ou HEXA en 3D et
 des éléments TRIA ou QUAD en 2D.
 Or, la maille  %(k2)s  est de type  %(k3)s .
"""),

20 : _(u"""
 La maille  %(k1)s  ne répond pas au critère géométrique sur les mailles HEXA :
 Les cotés opposés doivent être parallèles
"""),

26 : _(u"""
 Mauvaise définition des caractéristiques de la section
"""),

28 : _(u"""
 Rigidité géométrique non définie pour les éléments courbes
"""),

29 : _(u"""
 Force élémentaire électrique non définie pour les éléments courbes
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
 COMP_INCR non valide
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

46 : _(u"""
 le paramètre "pnosym" n'existe pas dans le catalogue de l'élément  %(k1)s  .
"""),

47 : _(u"""
 la taille de la matrice non-symétrique en entrée est fausse.
"""),

48 : _(u"""
 la taille de la matrice symétrique en sortie est fausse.
"""),

49 : _(u"""
 anisotropie non prévue pour coque1d
"""),

50 : _(u"""
 nombre de couches limite a 30 pour les coques 1d
"""),

51 : _(u"""
 Le nombre de couches défini dans DEFI_COQU_MULT et dans AFFE_CARA_ELEM dans n'est pas cohérent.
 Nombre de couches dans DEFI_COQU_MULT: %(i1)d
 Nombre de couches dans AFFE_CARA_ELEM: %(i2)d
"""),

52 : _(u"""
 L'épaisseur totale des couches définie dans DEFI_COQU_MULT et celle définie dans AFFE_CARA_ELEM ne sont pas cohérentes.
 Épaisseur totale des couches dans DEFI_COQU_MULT: %(r1)f
 Épaisseur dans AFFE_CARA_ELEM: %(r2)f
"""),

54 : _(u"""
  la réactualisation de la géométrie (déformation : PETIT_REAC sous le mot clé COMP_INCR) est déconseillée pour les éléments de coque_1d.
"""),

55 : _(u"""
 nombre de couches limite a 10 pour les coques 1d
"""),

56 : _(u"""
 valeurs utilisateurs de RHO ou de rof nulles
"""),

58 : _(u"""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments POU_D_T et POU_D_E.
  -> Risque & Conseil :
     En présence de grands déplacements et grandes rotations, avec une loi de comportement
     non linéaire, il est préférable  d'utiliser la modélisation POU_D_TGM
     (poutre multifibre) avec DEFORMATION=GROT_GDEP. Si le comportement reste
     élastique, il est également possible d'utiliser la modélisation POU_D_T_GD avec
     DEFORMATION='GROT_GDEP'.
"""),

59 : _(u"""
  le coefficient de poisson est non constant. la programmation actuelle n en tient pas compte.
"""),

60 : _(u"""
 Noeuds confondus pour un élément de poutre
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

75 : _(u"""
 le matériau %(k1)s  n'est pas autorisé pour calculer les déformations plastiques :
 seuls les matériaux isotropes sont traités en plasticité
"""),

76 : _(u"""
 couplage fluage/fissuration :
 la loi BETON_DOUBLE_DP ne peut être couplée qu'avec une loi de fluage de GRANGER
"""),

77 : _(u"""
  -> Attention vous êtes en contraintes planes, et vous utilisez la loi
     de comportement %(k1)s. La composante du tenseur des déformations
     plastiques EPZZ est calculée en supposant l'incompréhensibilité des
     déformations plastiques : EPZZ = -(EPXX + EPYY).
  -> Risque & Conseil :
     Vérifiez que cette expression est valide avec votre loi de comportement.

"""),

78 : _(u"""
  la réactualisation de la géométrie (déformation : PETIT_REAC sous le mot clé COMP_INCR) est déconseillée pour les éléments POU_D_TG
"""),

80 : _(u"""
 situation de contact impossible
"""),

85 : _(u"""
  relation :  %(k1)s  non implantée sur les éléments "POU_D_T_GD"
"""),

86 : _(u"""
  déformation :  %(k1)s  non implantée sur les éléments "POU_D_T_GD"
"""),

87 : _(u"""
 RCVALA ne trouve pas RHO, qui est nécessaire en dynamique
"""),

91 : _(u"""
  calcul de la masse non implanté pour les éléments COQUE_3D en grandes rotations, déformation : GROT_GDEP
"""),

92 : _(u"""
 les comportements élastiques de type COMP_ELAS ne sont pas disponibles pour la modélisation DKTG.
"""),

93 : _(u"""
  déformation :  %(k1)s  non implantée sur les éléments COQUE_3D en grandes rotations
  déformation : GROT_GDEP obligatoirement
"""),

94 : _(u"""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments COQUE_3D.
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
  : seule les poutres à sections constantes sont admises !
"""),

}
