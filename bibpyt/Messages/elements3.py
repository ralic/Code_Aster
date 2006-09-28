#@ MODIF elements3 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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

7: _("""
 pb determination sensibilite de rayonnement
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
 attention vous avez une loi de comportement inelastique et vous etes en contraintes planes, la composante du tenseur de deformations epzz que vous allez calculer n'est valable que tant que vous restez dans le domaine elastique.
"""),

12: _("""
 e, nu, alpha dependent de la temperature,                         tgu differente de tgv
"""),

13: _("""
 le calcul de dg n'a pas ete etendu a la plasticite !
"""),

14: _("""
 option invalide
"""),

15: _("""
 nom d'element illicite
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

23: _("""
 erreur lors de l'appel a fointe
"""),

24: _("""
 matns mal dimensionnee
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

40: _("""
 seule une loi de  comportement elastique isotrope est valide pour le calcul de la derivee de g par rapport a e !
"""),

41: _("""
 seule une loi de  comportement elastique isotrope est valide pour le calcul de la derivee de g par rapport au chargement !
"""),

42: _("""
 en thermoelasticite le calcul des derivees de g est pour le moment incorrect
"""),

43: _("""
 avec un chargement en  deformations (ou contraintes) initiales ,le calcul des derivees de g est pour le moment incorrect
"""),

44: _("""
 seule une loi de  comportement elastique isotrope est valide pour le calcul de la derivee de g par rapport a e
"""),

45: _("""
 le calcul de derivee n'a pas ete etendu a la plasticite !
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
 option sensibilite non developpee en hydratation
"""),

52: _("""
 pb determination sensibilite materiau ther_nl
"""),

53: _("""
 ce calcul est dedie au tria3
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
  la reactualisation de la geometrie (deformation : petit_reac sous le mot cle comp_incr) est deconseillee pour les elements pou_d_t et pou_d_e .
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
 calc_k_g est incompatible avec              les comportements incrementaux ainsi que la deformation                  green
"""),

69: _("""
 on a pas pu calculer les derivees des fonctions singulieres car on se trouve sur le fond de fissure
"""),

70: _("""
 il faut affecter les elements de  bord (e et nu) pour le calcul des fic
"""),

71: _("""
 la vitesse de convection ne doit pas etre nulle.
"""),

72: _("""
 erreur: weibull pas de champ thermique
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
 attention vous etes en contraintes planes, et vous utilisez la loi de comportement  %(k1)s . la  composante du tenseur des deformations plastiques  epzz est calculee avec epzz = -(epxx + epyy) . verifiez que cette expression est vraie avec votre loi de comportement.
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

81: _("""
 option inconnue 
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
  la reactualisation de la geometrie (deformation : petit_reac sous le mot cle comp_incr) est deconseillee pour les elements de coque_3d.
"""),

95: _("""
  nume_couche incorrect
"""),

96: _("""
 l'option est incompatible avec                                  les comportements incrementaux ainsi que la deformation           green
"""),

97: _("""
 type element inconnu
"""),

98: _("""
 comportement coeur homogeneise inexistant
"""),

99: _("""
  : seule les poutres a sections constantes sont admises !
"""),
}
