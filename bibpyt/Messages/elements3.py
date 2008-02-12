#@ MODIF elements3 Messages  DATE 11/02/2008   AUTEUR MACOCCO K.MACOCCO 
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

1: _("""
 erreur calcul de sigm1
"""),

2: _("""
 erreur calcul de sigmn
"""),

3: _("""
 erreur calcul de eps1
"""),

4: _("""
 erreur calcul de epsn
"""),

5: _("""
 erreur calcul de tpf1
"""),

6: _("""
 erreur calcul de tpfn
"""),

8: _("""
 erreur calcul de coenp1
"""),

9: _("""
 erreur calcul de coen
"""),

10: _("""
 on ne peut pas affecter la modelisation "axis_diag" aux elements de l'axe
"""),

11: _("""
  -> Attention vous avez une loi de comportement inélastique et vous etes
     en contraintes planes, la composante du tenseur de déformations EPZZ que
     vous allez calculer n'est valable que tant que vous restez dans le
     domaine élastique. Les autres composantes EPXX, EPYY, EPXY sont correctes.
  -> Risque & Conseil :
     Si le comportement est effectivement non linéaire, il ne faut pas utiliser
     la valeur de EPZZ calculée par cette option.
"""),

12: _("""
 e, nu, alpha dependent de la temperature,                         tgu differente de tgv
"""),

13: _("""
 le calcul de dg n'a pas ete etendu a la plasticite !
"""),

16: _("""
 comportement: %(k1)s non implante
"""),

17: _("""
 le materiau  %(k1)s  n'est pas connu. seuls sont admis les materiaux  'ther', 'ther_coqmu' et 'ther_coque' pour les coques thermiques .
"""),

18: _("""
 le materiau  %(k1)s  n'est pas connu. seuls sont admis les materiaux  'ther' et 'ther_coqmu' pour le calcul des flux pour les coques thermiques .
"""),

19: _("""
 l'option  %(k1)s  n'est disponible qu'avec des elements tetra ou hexa. or, la maille  %(k2)s  est de type  %(k3)s .
"""),

20: _("""
 la maille  %(k1)s  ne repond pas au critere geometrique sur les mailles hexa : les cotes opposes doivent etre paralleles
"""),

21: _("""
 erreur lors de l appel a fointe
"""),

22: _("""
 erreur dans le calcul de coef_f
"""),

25: _("""
 calcul de sensibilite :  actuellement, on ne derive que les : pou_d_e
"""),

26: _("""
 mauvaise definition des caracteristiques de la section
"""),

27: _("""
 l'option  " %(k1)s "  n'est pas programmee
"""),

28: _("""
 rigidite geometrique non definie pour les elements courbes.
"""),

29: _("""
 force elementaire electrique non definie pourles elements courbes.
"""),

30: _("""
 section non tubulaire pour mass_flui_stru
"""),

31: _("""
 pas de valeur utilisateur pour rho
"""),

32: _("""
 " %(k1)s "  nom d'option inconnu.
"""),

33: _("""
 option non disponible
"""),

34: _("""
 seules les forces suiveuses de type vent definies par un evol_char sont autorisees
"""),

35: _("""
 un champ de vitesse de vent est impose sans donner un cx dependant de la vitesse sur une des barres.
"""),

36: _("""
 comp_incr non valide
"""),

37: _("""
  relation :  %(k1)s  non implantee sur les cables
"""),

38: _("""
  deformation :  %(k1)s  non implantee sur les cables
"""),

39: _("""
 un champ de vitesse de vent est impose sans donner un cx dependant de la vitesse sur un des cables.
"""),

46: _("""
 le parametre "pnosym" n'existe pas dans le catalogue de l'element  %(k1)s  .
"""),

47: _("""
 la taille de la matrice non-symetrique en entree est fausse.
"""),

48: _("""
 la taille de la matrice symetrique en sortie est fausse.
"""),

49: _("""
 anisotropie non prevue pour coque1d
"""),

50: _("""
 nombre de couches limite a 30 pour les coques 1d
"""),

51: _("""
 Le nombre de couches défini dans DEFI_COQU_MULT et dans AFFE_CARA_ELEM dans n'est pas cohérent.
 Nombre de couches dans DEFI_COQU_MULT: %(i1)d
 Nombre de couches dans AFFE_CARA_ELEM: %(i2)d
"""),

52: _("""
 L'épaisseur totale des couches definie dans DEFI_COQU_MULT et celle définie dans AFFE_CARA_ELEM ne sont pas cohérentes.
 Epaisseur totale des couches dans DEFI_COQU_MULT: %(r1)f
 Epaisseur dans AFFE_CARA_ELEM: %(r2)f
"""),






54: _("""
  la reactualisation de la geometrie (deformation : petit_reac sous le mot cle comp_incr) est deconseillee pour les elements de coque_1d.
"""),

55: _("""
 nombre de couches limite a 10 pour les coques 1d
"""),

56: _("""
 valeurs utilisateurs de rho ou de rof nulles
"""),

57: _("""
 pas d elements lumpes pourhydratation 
"""),

58: _("""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments POU_D_T et POU_D_E.
  -> Risque & Conseil :
     En présence de grands déplacements et grandes rotations, avec une loi de comportement
     non linéaire, il est préférable  d'utiliser la modélisation POU_D_TGM
     (poutre multi-fibres) avec DEFORMATION=REAC_GEOM. Si le comportement reste
     élastique, il est également possible d'utiliser la modélisation POU_D_T_GD avec
     DEFORMATION='GREEN_GR'.
"""),

59: _("""
  le coefficient de poisson est non constant. la programmation actuelle n en tient pas compte.
"""),

60: _("""
 noeuds confondus pour un element de poutre
"""),

61: _("""
 loi  %(k1)s  indisponible pour les pou_d_e/d_t
"""),

62: _("""
 noeuds confondus pour un element de barre
"""),

63: _("""
 ne pas utiliser ther_lineaire avec des elements de fourier mais les cmdes developpees
"""),

64: _("""
 erreur dans le calcul de coeh_f
"""),

65: _("""
 chargements non nuls sur l'axe
"""),

66: _("""
 option  %(k1)s  inattendue
"""),

67: _("""
 element degenere:revoir le maillage
"""),

68: _("""
 L'option CALC_K_G est incompatible avec les comportements incrémentaux, avec les comportement non
 linéaires et avec la deformation GREEN.
"""),

70: _("""
 il faut affecter les elements de  bord (e et nu) pour le calcul des fic
"""),

73: _("""
 option de calcul non valide
"""),

74: _("""
 pour l'option "rice_tracey", la relation " %(k1)s " n'est pas admise
"""),

75: _("""
 le materiau  %(k1)s  n'est pas autorise pour calculer les deformations plastiques : seuls les materiaux isotropes sont traites en plasticite.
"""),

76: _("""
 couplage fluage/fissuration : la loi beton_double_dp ne peut etre couplee qu avec une loi de fluage de granger.
"""),

77: _("""
  -> Attention vous etes en contraintes planes, et vous utilisez la loi
     de comportement %(k1)s. La composante du tenseur des déformations
     plastiques EPZZ est calculée en supposant l'incompressibilité des
     déformations plastiques : EPZZ = -(EPXX + EPYY).
  -> Risque & Conseil :
     Vérifiez que cette expression est valide avec votre loi de comportement.

"""),

78: _("""
  la reactualisation de la geometrie (deformation : petit_reac sous le mot cle comp_incr) est deconseillee pour les elements pou_d_tg  .
"""),

79: _("""
 tableau sous dimensionne (dvlp)
"""),

80: _("""
 situation de contact impossible
"""),

82: _("""
 vecteur sous dimensionne (dvlp)
"""),

83: _("""
 dimension incorrecte (dvlp)
"""),

84: _("""
 type maille inconnu
"""),

85: _("""
  relation :  %(k1)s  non implantee sur les elements "pou_d_t_gd"
"""),

86: _("""
  deformation :  %(k1)s  non implantee sur les elements "pou_d_t_gd"
"""),

87: _("""
 rcvala ne trouve pas rho, qui est necessaire en dynamique
"""),

88: _("""
 la masse volumique rho n'a pas ete definie
"""),

89: _("""
 developpement non realise
"""),

90: _("""
 option:  %(k1)s  non implante
"""),

91: _("""
  calcul de la masse non implante pour les elements coque_3d en grandes rotations, deformation : green_gr
"""),

92: _("""
 les comportements elastiques de type comp_elas ne sont pas disponibles pour la modelisation dktg.
"""),

93: _("""
  deformation :  %(k1)s  non implantee sur les elements coque_3d en grandes rotations.   deformation : green_gr obligatoirement 
"""),

94: _("""
  -> La réactualisation de la géométrie (DEFORMATION='PETIT_REAC' sous
     le mot clé COMP_INCR) est déconseillée pour les éléments COQUE_3D.
  -> Risque & Conseil :
     Le calcul des déformations à l'aide de PETIT_REAC n'est qu'une
     approximation des hypothèses des grands déplacements. Elle nécessite
     d'effectuer de très petits incréments de chargement. Pour prendre en
     compte correctement les grands déplacements et surtout les grandes
     rotations, il est recommandé d'utiliser DEFORMATION='GREEN_GR'.

"""),

95: _("""
  nume_couche incorrect
"""),

96: _("""
 l'option est incompatible avec les comportements incrementaux ainsi que la deformation green
"""),

98: _("""
 comportement coeur homogeneise inexistant
"""),

99: _("""
  : seule les poutres a sections constantes sont admises !
"""),
}
