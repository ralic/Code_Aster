#@ MODIF calculel3 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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
 manque les accelerations
"""),

2: _("""
 pour une sd resultat de type  dyna_trans, seuls les mots_cles fonc_mult et coef_mult sont autorises
"""),

3: _("""
 pour un sd resultat de type  evol_elas,seul le mot-cle fonc_mult est  autorise
"""),

4: _("""
 l'utilisation d mot-cle fonc_mult n'est licite que pour les sd resultats:  evol_elas, dyna_trans, dyna_harmo
"""),

5: _("""
  pour calculer  %(k1)s  il faut sief_elno_elga ou efge_elno_depl
"""),

6: _("""
  option  %(k1)s  non traitee pour un resultat de type  %(k2)s 
"""),

7: _("""
 calcul de  %(k1)s  impossible.
"""),

8: _("""
 attention : les champs sief_elga_depl, sief_elga, sigm_elno_coqu etsigm_elno_depl sont absents : on ne peut pas calculer l'option %(k1)s  avec la sd de type  %(k2)s 
"""),

9: _("""
 attention : le champ  sigm_elno_depl est absent :  on ne peut pas calculer l'option %(k1)s  avec la sd de type  %(k2)s 
"""),

10: _("""
 attention : le champ  sigm_elno_coqu est absent :  on ne peut pas calculer l'option %(k1)s  avec la sd de type  %(k2)s 
"""),

11: _("""
 le resultat  %(k1)s  doit comporter un champ de deplacement au numero d'ordre  %(k2)s  .
"""),

12: _("""
 le mot cle prec_err est obligatoire avec l'option sing_elem
"""),

13: _("""
 le mot cle prec_err doit etre strictement superieur a zero et inferieur ou egal a 1
"""),

14: _("""
 pas d indicateur d erreur- on ne calcule pas l'option sing_elem
"""),

15: _("""
 par defaut on utilise erre_elem_sigm
"""),

16: _("""
 par defaut on utilise erz2_elem_sigm
"""),

17: _("""
 le resultat  %(k1)s  doit comporter un champ de contraintes au numero d'ordre  %(k2)s  .
"""),

18: _("""
 pas de champ de contraintes pour calculer  %(k1)s 
"""),

19: _("""
 probleme a l'appel de alchml pour  %(k1)s 
"""),

20: _("""
 pas de champ endommagement pour calculer  %(k1)s 
"""),

21: _("""
 le calcul avec l'option endo_elno_elga necessite au prealable un calcul avec l'option endo_elga
"""),

22: _("""
  option inexistante: %(k1)s 
"""),

23: _("""
 option :  %(k1)s 
"""),

25: _("""
 calcul non disponible
"""),

27: _("""
 type :  %(k1)s  incompatible avec l'option :  %(k2)s 
"""),

28: _("""
 type de champ inconnue
"""),

29: _("""
 erreur jacot 1
"""),

30: _("""
 il faut un modele ou des charges.
"""),

31: _("""
 la masse du macr_elem : %(k1)s  n'a pas encore ete calculee.
"""),

32: _("""
 il manque des masses.
"""),

33: _("""
 la rigidite du macr_elem : %(k1)s  n'a pas encore ete calculee.
"""),

34: _("""
 il manque des rigidites.
"""),

35: _("""
 le modele doit contenir des elements finis ou des sous-structures.
"""),











38: _("""
 on ne traite pas le type_scalaire: %(k1)s 
"""),

39: _("""
 le modele contient des elements de structure. il faut probablement utiliser le mot-cle cara_elem.
"""),

40: _("""
  -> Le modèle a probablement besoin d'un champ de matériau (mot-clé CHAM_MATER).

  -> Risque & Conseil :
     Ce message peut aider à comprendre un éventuel problème ultérieur lors de calculs élémentaires
     nécessitant des caractéristiques matérielles.
     Vérifier si votre modélisation nécessite un CHAM_MATER.
"""),

41: _("""
 les charges ne s'appuient pas toutes sur le meme modele.
"""),

42: _("""
 les charges ne s'apuient pas sur le modele donne en argument.
"""),

43: _("""
 les charges sont de type different.
"""),

44: _("""
 les charges ne s'appuient pas toutes sur le meme modele
"""),

45: _("""
 donnees incorrectes.
"""),

46: _("""
 calc_k_g : champ initial impossible
"""),

47: _("""
 le fond de fissure doit contenir un noeud et un seul
"""),

48: _("""
 il faut definir la normale au fond de fissure
"""),

49: _("""
 on ne trouve pas le .nomo pour: %(k1)s 
"""),

50: _("""
  il faut un modele
"""),

51: _("""
 il manque le modele
"""),

52: _("""
 le champ doit etre un cham_elem.
"""),

53: _("""
 ne traite qu'un cham_elem reel
"""),

54: _("""
 longueurs des modes locaux imcompatibles entre eux.
"""),

55: _("""
 la longueur:long est trop petite.
"""),

56: _("""
 il n'y a pas autant de composantes
"""),

57: _("""
 on ne sait pas moyenner cette composante negative
"""),

58: _("""
 champs sur modeles differents
"""),

59: _("""
  %(k1)s  doit etre un cham_elem.
"""),

60: _("""
 longueurs des modes locaux champ1 imcompatibles entre eux.
"""),

61: _("""
 longueurs des modes locaux champ2 imcompatibles entre eux.
"""),

62: _("""
 composante non definie
"""),

63: _("""
 champ de geometrie non trouve
"""),

64: _("""
 l'instant du calcul est pris  arbitrairement a 0.0 
"""),

65: _("""
  on n'accepte un instant arbitraire que si le concept deformations anelastiques n'a qu'1 champ.
"""),

66: _("""
  le concept evol_noli :  %(k1)s  ne contient aucun champ de deformations anelastiques.
"""),

67: _("""
 pour calculer l'option  %(k1)s  les parametres suivants sont obligatoires: "pgeomer" et "pcontrr".
"""),








69: _("""
 il n y a ni elements ni sous-struc
"""),






71: _("""
 il faut 1 chargement de rotation et un seul. 
"""),

72: _("""
  il ne faut pas definir plus d"un champ de vitesse 
"""),

73: _("""
 le champ:  %(k1)s  n'est ni un cham_elem ni un resuelem
"""),

74: _("""
 type scalaire interdit : %(k1)s 
"""),

75: _("""
  on n'accepte un instant arbitraire que si le concept temperature n'a qu'1 champ.
"""),

76: _("""
  le concept evol_ther :  %(k1)s  ne contient aucun champ de temperature.
"""),

77: _("""
 le champ de temperature utilise est independant du temps.
"""),

78: _("""
 temperature de reference a probleme.
"""),

79: _("""
 la matrice a est singuliere
"""),

81: _("""
 cette fonction ne marche que pour des modes locaux de type chno, vect, ou mat
"""),

82: _("""
 le mode local est de type matrice non_carree
"""),

83: _("""
 4
"""),

84: _("""
 il n y a pas de parametre  %(k1)s  associe a la grandeur: %(k2)s  dans l option: %(k3)s 
"""),

85: _("""
 il y a plusieurs parametres  %(k1)s  associes a la grandeur: %(k2)s  dans l option: %(k3)s 
"""),

86: _("""
  %(k1)s  non prevu
"""),

87: _("""
 elrefe inconnu  %(k1)s 
"""),

88: _("""
 les charges sont incoherentes avec le modele.
"""),

89: _("""
 erreur: les charges ne s appuient pas toutes sur le meme modele.
"""),

90: _("""
 le champ de theta est inexistant dans la structure de donnees  %(k1)s  de type theta_geom .
"""),

91: _("""
 erreur: une des charges n'est pas mecanique
"""),

92: _("""
 erreur: une des charges n'est pas thermique
"""),

93: _("""
 erreur: une des charges n'est pas acoustique
"""),

94: _("""
 erreur: le champ doit etre un cham_elem aux points de gauss
"""),

95: _("""
 avec un cham_elem calcule sur une liste de maille, il faut utiliser le mot cle "modele:"
"""),

96: _("""
 pour prendre en compte les termes d'inertie il est preferable d'utiliser la commande "calc_elem". le mot cle "acce" n'est pas traite et les resultats risquent d'etre faux.
"""),

97: _("""
 le champ de nom symbolique theta existe deja dans la s.d. resultat  %(k1)s 
"""),

98: _("""
 le champ de nom symbolique grad_noeu_theta existe deja dans la s.d. resultat  %(k1)s 
"""),

99: _("""
 il faut donner 3 composantes de la direction
"""),
}
