#@ MODIF algorith10 Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg = {

1 : _("""
 la variable  %(k1)s  n'existe pas dans la loi  %(k2)s 
"""),

2 : _("""
 tailles de matrices incompatibles
"""),

10 : _("""
 taille produit matrice-vecteur incompatible
"""),

11 : _("""
 le champ de déplacement n'a pas été calculé
"""),

12 : _("""
 le champ de vitesse n'a pas été calculé
"""),

13 : _("""
 le champ d'accélération n'a pas ete calcule.
"""),

14 : _("""
 développement non prévu pour le MULT_APPUI ou CORR_STAT.
"""),

15 : _("""
 développement non prévu pour la sous-structuration.
"""),

16 : _("""
 le champ  %(k1)s  n'a pas été calculé dans le MODE_MECA  %(k2)s 
"""),

17 : _("""
 l'option  %(k1)s  s'applique sur toute la structure
"""),

20 : _("""
  le comportement :  %(k1)s  n'a pas etet defini
"""),

21 : _("""
 DIST_REFE est obligatoire à la première occurence de RECO_GLOBAL
"""),

31 : _("""
 la bande de fréquence retenue ne comporte pas de modes propres
"""),

32 : _("""
 vous avez demandé des modes qui ne sont pas calculés
"""),

33 : _("""
 il n y a pas de mode statique calculé pour le couple noeud-cmp ci dessus
"""),

35 : _("""
 redécoupage excessif du pas de temps interne
 réduisez votre pas de temps ou augmenter abs(ITER_INTE_PAS)
 redecoupage global.
"""),

36 : _("""
 il manque SIGM_REFE
"""),

37 : _("""
 il manque RESI_HYD1_REFE
"""),

38 : _("""
 il manque RESI_HYD2_REFE
"""),

39 : _("""
 il manque RESI_THER_REFE
"""),

40 : _("""
 vecteur nul entrainant une division par zéro dans NMCONV
"""),

41 : _("""
 incohérence de A ou H
"""),

42 : _("""
 incohérence de données
"""),

43 : _("""
 incohérence de C, PHI ou A
"""),

44 : _("""
 champ 'DEPL' non calculé
"""),

45 : _("""
 champ 'VITE' non calculé
"""),

46 : _("""
 champ 'ACCE' non calculé
"""),

47 : _("""
 lecture des instants erronée
"""),

48 : _("""
 axe de rotation indéfini.
"""),

49 : _("""
 la porosité initiale F0 ne peut etre nulle ou négative
"""),

50 : _("""
 la porosité initiale F0 ne peut etre supérieure ou égale à 1.
"""),

51 : _("""
 comportement de Rousselier version PETIT_REAC non implanté en contraintes planes
"""),

52 : _("""
 la porosité initiale F0 ne peut etre négative
"""),

53 : _("""
 pb2, variables de pilotages
"""),

54 : _("""
 erreur d'intégration dans Runge-Kutta
 trop d'itération.
"""),

55 : _("""
 erreur d integration dans Runge-Kutta
"""),

56 : _("""
 on ne sait pas post-traiter le champ de type:  %(k1)s 
"""),

60 : _("""
 taille vecteurs incompatible
"""),

61 : _("""
 il faut definir une BANDE ou un NUME_ORDRE
"""),

62 : _("""
 il faut definir une "BANDE" ou une liste de "NUME_ORDRE"
"""),

63 : _("""
 dimension spectrale fausse
"""),

64 : _("""
 l'interspectre modal est de type "ACCE"
 on ne peut que restituer une accélération
"""),

65 : _("""
 l'interspectre modal est de type "VITE"
 on ne peut que restituer une vitesse
"""),

66 : _("""
 l'interspectre modal est de type "DEPL"
 on ne peut pas restituer une accélération
"""),

67 : _("""
 l'interspectre modal est de type "DEPL"
 on ne peut pas restituer une vitesse
"""),

68 : _("""
 il faut autant de "NOEUD" que de "NOM_CMP"
"""),

69 : _("""
 il faut autant de "MAILLE" que de "NOEUD"
"""),

72 : _("""
 il faut définir une liste de mailles pour post-traiter un CHAM_ELEM
"""),

73 : _("""
 la composante  %(k1)s  du noeud  %(k2)s  pour la maille  %(k3)s  n'existe pas.
"""),

74 : _("""
 on ne traite pas la maille "POI1"
"""),

75 : _("""
 type de maille non traitée
"""),

76 : _("""
 mot-clé nb_bloc inopérant on prend 1 bloc
"""),

77 : _("""
 élément dégénéré
"""),

82 : _("""
 pas de suivi attaché à la demande d'affichage
"""),

83 : _("""
 trop de lignes dans le titre
"""),

87 : _("""
 vecteur de norme trop petite
"""),

88 : _("""
 COMP_ELAS non implanté
"""),

90 : _("""
 la définition de la température est obligatoire
 pour une loi de couplage de type  %(k1)s 
"""),

91 : _("""
 problème dans la définition de la saturation
"""),

93 : _("""
 il faut un nom de champ
"""),

94 : _("""
 pas de champ autre que DEPL ou VITE ou ACCE
"""),

95 : _("""
 pour interpoler il faut fournir une liste de fréquences ou instants.
"""),

96 : _("""
 calcul du transitoire: pas de solution trouvée
 utiliser l'option ETAT_STAT = NON
"""),

97 : _("""
 durée de l'excitation trop courte pour le calcul du transitoire.
"""),

98 : _("""
 pivot nul
"""),

99 : _("""
 on ne sait pas encore traiter la sous structuration en axisymétrique
"""),

}
