#@ MODIF elements3 Messages  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

10 : _("""
 on ne peut pas affecter la modelisation "axis_diag" aux elements de l'axe
"""),

11 : _("""
  -> Attention vous avez une loi de comportement inélastique et vous etes
     en contraintes planes, la composante du tenseur de déformations EPZZ que
     vous allez calculer n'est valable que tant que vous restez dans le
     domaine élastique. Les autres composantes EPXX, EPYY, EPXY sont correctes.
  -> Risque & Conseil :
     Si le comportement est effectivement non linéaire, il ne faut pas utiliser
     la valeur de EPZZ calculée par cette option.
"""),

12 : _("""
 Calcul de G bilinéaire
 E, NU, ALPHA dépendent de la temperature
 Les champs de température (TGU et TGV) sont différents
"""),

16 : _("""
 Comportement: %(k1)s non implanté
"""),

17 : _("""
 Le matériau  %(k1)s  n'est pas connu
 Seuls sont admis les matériaux  'THER' et 'THER_COQUE' pour les coques thermiques
"""),

18 : _("""
 Le matériau  %(k1)s  n'est pas connu
 Seuls sont admis les materiaux  'THER' et 'THER_COQUE' pour le calcul des flux pour les coques thermiques
"""),

19 : _("""
 L'option  %(k1)s  n'est disponible qu'avec des éléments TETRA ou HEXA
 Or, la maille  %(k2)s  est de type  %(k3)s .
"""),

20 : _("""
 La maille  %(k1)s  ne répond pas au critère géometrique sur les mailles HEXA :
 Les cotés opposés doivent être parallèles
"""),

25 : _("""
 Calcul de sensibilité :
 Actuellement, on ne dérive que les POU_D_E
"""),

26 : _("""
 Mauvaise définition des caractéristiques de la section
"""),

28 : _("""
 Rigidité géométrique non définie pour les éléments courbes
"""),

29 : _("""
 Force élémentaire électrique non définie pour les éléments courbes
"""),

30 : _("""
 Section non tubulaire pour MASS_FLUI_STRU
"""),

31 : _("""
 Pas de valeur utilisateur pour RHO
"""),

34 : _("""
 Seules les forces suiveuses de type vent définies par un evol_char sont autorisées
"""),

35 : _("""
 Un champ de vitesse de vent est imposé sans donner un CX dépendant de la vitesse sur une des barres
"""),

36 : _("""
 comp_incr non valide
"""),

37 : _("""
  Relation :  %(k1)s  non implantée sur les cables
"""),

38 : _("""
  Déformation :  %(k1)s  non implantée sur les cables
"""),

39 : _("""
 un champ de vitesse de vent est impose sans donner un cx dependant de la vitesse sur un des cables.
"""),

46 : _("""
 le parametre "pnosym" n'existe pas dans le catalogue de l'element  %(k1)s  .
"""),

47 : _("""
 la taille de la matrice non-symetrique en entree est fausse.
"""),

48 : _("""
 la taille de la matrice symetrique en sortie est fausse.
"""),

49 : _("""
 anisotropie non prevue pour coque1d
"""),

50 : _("""
 nombre de couches limite a 30 pour les coques 1d
"""),

51 : _("""
 Le nombre de couches défini dans DEFI_COQU_MULT et dans AFFE_CARA_ELEM dans n'est pas cohérent.
 Nombre de couches dans DEFI_COQU_MULT: %(i1)d
 Nombre de couches dans AFFE_CARA_ELEM: %(i2)d
"""),

52 : _("""
 L'épaisseur totale des couches definie dans DEFI_COQU_MULT et celle définie dans AFFE_CARA_ELEM ne sont pas cohérentes.
 Epaisseur totale des couches dans DEFI_COQU_MULT: %(r1)f
 Epaisseur dans AFFE_CARA_ELEM: %(r2)f
"""),

54 : _("""
  la reactualisation de la geometrie (deformation : petit_reac sous le mot cle comp_incr) est deconseillee pour les elements de coque_1d.
"""),

55 : _("""
 nombre de couches limite a 10 pour les coques 1d
"""),

56 : _("""
 valeurs utilisateurs de rho ou de rof nulles
"""),

57 : _("""
 pas d elements lumpes pourhydratation 
"""),

58 : _("""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments POU_D_T et POU_D_E.
  -> Risque & Conseil :
     En présence de grands déplacements et grandes rotations, avec une loi de comportement
     non linéaire, il est préférable  d'utiliser la modélisation POU_D_TGM
     (poutre multi-fibres) avec DEFORMATION=REAC_GEOM. Si le comportement reste
     élastique, il est également possible d'utiliser la modélisation POU_D_T_GD avec
     DEFORMATION='GREEN_GR'.
"""),

59 : _("""
  le coefficient de poisson est non constant. la programmation actuelle n en tient pas compte.
"""),

60 : _("""
 Noeuds confondus pour un élément de poutre
"""),

61 : _("""
 loi  %(k1)s  indisponible pour les pou_d_e/d_t
"""),

62 : _("""
 Noeuds confondus pour un élément de barre
"""),

63 : _("""
 ne pas utiliser THER_LINEAIRE avec des éléments de fourier mais les cmdes developpees
"""),

67 : _("""
 Elément dégénéré : 
 revoir le maillage
"""),

74 : _("""
 pour l'option "RICE_TRACEY", la relation " %(k1)s " n'est pas admise
"""),

75 : _("""
 le matériau %(k1)s  n'est pas autorisé pour calculer les deformations plastiques :
 seuls les matériaux isotropes sont traités en plasticité
"""),

76 : _("""
 couplage fluage/fissuration :
 la loi BETON_DOUBLE_DP ne peut etre couplée qu'avec une loi de fluage de GRANGER
"""),

77 : _("""
  -> Attention vous etes en contraintes planes, et vous utilisez la loi
     de comportement %(k1)s. La composante du tenseur des déformations
     plastiques EPZZ est calculée en supposant l'incompressibilité des
     déformations plastiques : EPZZ = -(EPXX + EPYY).
  -> Risque & Conseil :
     Vérifiez que cette expression est valide avec votre loi de comportement.

"""),

78 : _("""
  la réactualisation de la géometrie (déformation : PETIT_REAC sous le mot cle COMP_INCR) est deconseillée pour les éléments POU_D_TG
"""),

80 : _("""
 situation de contact impossible
"""),

84 : _("""
 type de maille inconnu
"""),

85 : _("""
  relation :  %(k1)s  non implantee sur les elements "pou_d_t_gd"
"""),

86 : _("""
  deformation :  %(k1)s  non implantee sur les elements "pou_d_t_gd"
"""),

87 : _("""
 RCVALA ne trouve pas RHO, qui est nécessaire en dynamique
"""),

91 : _("""
  calcul de la masse non implanté pour les éléments COQUE_3D en grandes rotations, deformation : GREEN_GR
"""),

92 : _("""
 les comportements elastiques de type comp_elas ne sont pas disponibles pour la modelisation dktg.
"""),

93 : _("""
  déformation :  %(k1)s  non implantée sur les éléments COQUE_3D en grandes rotations
  déformation : GREEN_GR obligatoirement 
"""),

94 : _("""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments COQUE_3D.
  -> Risque & Conseil :
     Le calcul des déformations à l'aide de PETIT_REAC n'est qu'une
     approximation des hypothèses des grands déplacements. Elle nécessite
     d'effectuer de très petits incréments de chargement. Pour prendre en
     compte correctement les grands déplacements et surtout les grandes
     rotations, il est recommandé d'utiliser DEFORMATION='GREEN_GR'.

"""),

95 : _("""
  nume_couche incorrect
"""),

98 : _("""
 comportement coeur homogénéise inexistant
"""),

99 : _("""
  : seule les poutres à sections constantes sont admises !
"""),

}
