#@ MODIF calculel4 Messages  DATE 14/11/2006   AUTEUR DESROCHES X.DESROCHES 
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
 option non operationnelle: seule l'option couronne est a utiliserdans le cas ou on emploie le mot cle theta_3d .
"""),

2: _("""
 il faut donner 3 composantes de la direction,la 3-eme nulle
"""),

3: _("""
 il faut donner la direction de propagation en 2d    la direction par defaut n'existe plus
"""),

4: _("""
 option non operationnelle: seule l'option couronne est a utiliserdans le cas ou on emploie le mot cle theta_2d .
"""),

5: _("""
 option inexistante: seule l'option bande est a utiliser dans le cas ou on emploie le mot cle theta_bande .
"""),

6: _("""
 la normale n'est pas orthogonale a la tangente a l'origine
"""),

7: _("""
 la normale n'est pas orthogonale a la tangente a l'extremite
"""),

8: _("""
 le resultat  %(k1)s  n'existe pas
"""),

9: _("""
 creation de la table  %(k1)s 
"""),

10: _("""
 si le mot-cle depl est present alors le mot-cle modele est obligatoire.
"""),

11: _("""
 si le mot cle depl est present alors le mot-cle cham_mater est obligatoire.
"""),

12: _("""
 la liste d'instants ne doitcomporter qu'un seul instant avec le mot-cle depl
"""),

13: _("""
 probleme a la recuperation d'un champ
"""),

14: _("""
 dans le cas d'une sd resultat de type dyna_trans, le mot-cle modele est obligatoire.
"""),

15: _("""
 dans le cas d'une sd resultat de type dyna_trans, le mot-cle cham_mater est obligatoire.
"""),

16: _("""
 dans le cas d'une sd resultat de type dyna_trans, le mot-cle excit est obligatoire.
"""),

17: _("""
 pour un resultat de type mode_meca l option de calcul doit etre k_g_moda.
"""),

18: _("""
 les charges ne s'appuient pas sur le modele donne en argument
"""),

19: _("""
 le mot cle 'fissure' est obligatoire avec l'option  %(k1)s 
"""),

20: _("""
 le mot cle 'fond_fiss' est obligatoire avec l'option  %(k1)s 
"""),

21: _("""
 mot cle fond_fiss obligatoire pour l option  %(k1)s 
"""),

22: _("""
 champ theta calcule automatiquement
"""),

23: _("""
 pour l option  %(k1)s (3d local) utiliser le mot cle theta_lagr
"""),

24: _("""
 il faut donner 3 composantes de la direction %(k1)s 
"""),

25: _("""
 mot cle propagation utilise seulement pour le calcul de g avec propagation lagrangienne
"""),

26: _("""
 cette combinaison de lissage n'est pas programmee pour l'option :  %(k1)s .
"""),

27: _("""
 le degre des polynomes de legendre doit etre inferieur ou egal au nombre de noeuds du fond de fissure avec la methode theta-lagrange
"""),

28: _("""
 le lissage de g doit etre de type legendre si le lissage de theta est de type legendre
"""),

29: _("""
 l'option  %(k1)s  n'est pas permise avec le lissage 'lagrange_regu'
"""),

30: _("""
 si la methode 'lagrange_regu' est utilisee pour le lissage, alors le lissage de g et de theta doivent etre de type 'lagrange_regu'.
"""),

31: _("""
 champ theta obligatoire avec  %(k1)s . utiliser le mot cle theta_lagr.
"""),

32: _("""
 fond obligatoire avec option calc_k_g
"""),

33: _("""
 derivation de g : un seul parametre sensible par appel a calc_g 
"""),

34: _("""
 on ne sait pas traiter le type de sensibilite associe au parametre sensible  %(k1)s 
"""),

35: _("""
 sensibilite au parametre  %(k1)s 
"""),

36: _("""
 l usage des polynomes de legendre dans le cas d un fond de fissure clos est interdit.
"""),

37: _("""
 acces impossible au deplacement
"""),

38: _("""
 mot-clef <bornes> obligatoire avec l option  %(k1)s  !
"""),

39: _("""
 acces impossible au mode propre
"""),

40: _("""
 option non dispo actuellement
"""),

41: _("""
 erreur_01
"""),

42: _("""
 erreur_02
"""),

43: _("""
 le nom_para n'existe pas
"""),

44: _("""
 0 ligne trouvee pour le nom_para
"""),

45: _("""
 plusieurs lignes trouvees
"""),

46: _("""
 code retour de "tbliva" inconnu
"""),

47: _("""
 type_resu inconnu:  %(k1)s 
"""),

48: _("""
 erreur calcul alpha0 :champ depl elastique non trouve
"""),

49: _("""
 erreur : le champ depl elastique n'existe pas
"""),

50: _("""
 erreur: le champ sief_elga_depl n'existe pas
"""),

51: _("""
 methode zac : accommodation et chargement non radial --> methode non appliquable
"""),

52: _("""
 composante non definie dans la grandeur.
"""),

53: _("""
 longueurs des modes locaux incompatibles entre eux.
"""),

54: _("""
 aucuns noeuds sur lesquels projeter.
"""),

55: _("""
 pas de mailles a projeter.
"""),

56: _("""
  %(k1)s  pas trouve.
"""),

57: _("""
 il n'y a pas de mailles a projeter.
"""),

58: _("""
 les maillages a projeter sont ponctuels.
"""),

59: _("""
 maillages 1 differents.
"""),

60: _("""
 maillages 2 differents.
"""),

61: _("""
 probleme dans l'examen de  %(k1)s 
"""),

62: _("""
 aucun numero d'ordre dans  %(k1)s 
"""),

63: _("""
 on ne sait pas encore projeter les champs  %(k1)s 
"""),

64: _("""
 aucun champ projete.
"""),

65: _("""
  maillages non identiques :  %(k1)s  et  %(k2)s 
"""),

66: _("""
 pas de chmate
"""),

67: _("""
 erreur dans etanca pour le probleme primal
"""),

68: _("""
 erreur dans etanca pour le probleme dual
"""),

69: _("""
 pas de variable:  %(k1)s  pour la maille:  %(k2)s 
"""),

70: _("""
 le calcul de l indicateur d erreur ne sait pas traiter les charges du type de  %(k1)s 
"""),

71: _("""
 le choix  %(k1)s  apparait au moins dans 2 charges.
"""),

72: _("""
 probleme sur les charges. consulter la documentation
"""),

73: _("""
 ! ligrel incompatible avec modele !
"""),

74: _("""
 ! pas de chgeom !
"""),

75: _("""
 ! pb acces simultane carth/t !
"""),

76: _("""
 ! pb etenca cartf !
"""),

77: _("""
 ! pb etenca carth !
"""),

78: _("""
 ! pb etenca cartt !
"""),

79: _("""
 la grandeur :  %(k1)s  n'existe pas au catalogue.
"""),

80: _("""
 le champ de grandeur  %(k1)s  ne respecte pas le format xxxx_c
"""),

81: _("""
 probleme dans le catalogue des grandeurs simples, la grandeur %(k1)s  ne possede pas le meme nombre de champ que son homologue reel %(k2)s 
"""),

82: _("""
 probleme dans le catalogue des grandeurs simples, la grandeur %(k1)s  ne possede pas les memes champs que son homologue reelle %(k2)s 
"""),

83: _("""
 erreur: le calcul des contraintes ne fonctionne que pour le phenomene mecanique
"""),

84: _("""
 erreur numeros des noeuds bords
"""),

85: _("""
 erreur: les elements supportes sont tria3 ou tria6
"""),

86: _("""
 erreur: les elements supportes sont quad4 ou quad8 ou quad9
"""),

87: _("""
 maillage mixte tria-quad non supporte pour l estimateur zz2
"""),

88: _("""
 erreur: les mailles supportees sont tria ou quad
"""),

89: _("""
 erreur: un element du maillage possede tous ses sommets sur une frontiere. il faut au moins un sommet interne.
Pour pouvoir utiliser ZZ2 il faut remailler le coin de telle facon que tous les trg aient au moins un sommet interieur.
"""),

90: _("""
 erreur programmeur
"""),

91: _("""
 on ne trouve pas de routine te0npq npq doit etre compris entre 1 et 600 ici : npq = %(k1)s 
"""),

92: _("""
  relation :  %(k1)s  non implantee sur les poulies
"""),

93: _("""
  deformation :  %(k1)s  non implantee sur les poulies
"""),

94: _("""
 l'attribut:  %(k1)s  n'existe pas pour le type:  %(k2)s 
"""),

95: _("""
 erreur de programmation :on ne trouve pas dans les arguments de la routine calcul de champ a associer au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

96: _("""
 erreur de programmation :on n'a pas pu extraire toutes les cmps voulues du champ global associe au parametre: %(k1)s  (option: %(k2)s  type_element: %(k3)s )
"""),

97: _("""
 ! tout = oui obligatoire avec  %(k1)s !
"""),

98: _("""
 attention : on n'a pas pu recuperer le parametre theta dans le resultat  %(k1)s , valeur prise pour theta: 0.57 
"""),

99: _("""
 attention : recuperation d'une valeur de theta illicite dans le resultat  %(k1)s valeur prise pour theta: 1. 
"""),
}
