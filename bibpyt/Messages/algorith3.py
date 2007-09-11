#@ MODIF algorith3 Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
 certains pas de temps de la liste (LISTE_INST) sont plus petits
 que le pas de temps minimal renseigné (SUBD_PAS_MINI)
"""),

2: _("""
 il faut donner SUBD_NIVEAU et/ou SUBD_PAS_MINI
"""),

3: _("""
 le nombre de subdivisions du pas de temps doit etre plus grand que 1 (SUBD_PAS)
"""),

4: _("""
 option inconnue < %(k1)s > incohérence catalogue
"""),

5: _("""
 méthode de subdivision inconnue < %(k1)s > incohérence catalogue
"""),

6: _("""
 valeur de SUBD_ITER_IGNO incoherent avec ITER_GLOB_MAXI.
 augmentez ITER_GLOB_MAXI
"""),

7: _("""
 valeur de SUBD_ITER_FIN incoherent avec ITER_GLOB_MAXI.
 augmentez ITER_GLOB_MAXI
"""),

8: _("""
 élément non traité
"""),

9: _("""
 utiliser la formulation en déplacement avec un schéma NEWMARK, HHT ou THETA_METHODE,
 ou la formulation en accéleration avec un schéma DIFF_CENT ou TCHAMWA.
"""),

10: _("""
  -> Contact avec DYNA_TRAN_MODAL : Il y a interpénétration d'une valeur supérieure à (DIST_MAIT + DIST_ESCL).
  -> Risque & Conseil :
     DIST_MAIT et DIST_ESCL permettent de tenir compte d'une épaisseur de matériau non représentée dans le maillage
     (rayon d'une poutre, épaisseur d'une coque ou simplement une bosse). Une trop forte interpénétration peut venir 
     d'une erreur dans le fichier de commande : RIGI_NOR trop faible ; noeuds de contact qui ne sont en vis à vis ; 
     OBSTACLE et NORM_OBSTACLE incohérents. Dans le cas de deux poutres aux fibres neutres confondues, elle peut 
     générer des erreurs dans l'orientation des forces de contact.
"""),

11: _("""
 méthode à pas adaptatif : la donnée du pas est obligatoire 
"""),

12: _("""
 méthode à pas adaptatif : le pas ne peut pas etre nul  
"""),

13: _("""
 les matrices de masse élémentaires doivent obligatoirement avoir été calculées
 avec l'option MASS_MECA_DIAG
"""),

14: _("""
 on archive au moins un champ.
"""),

15: _("""
 champ " %(k1)s " déjà existant
"""),

16: _("""
A l'instant %(r1)f, l'erreur vaut %(r2)f
Cette erreur est supérieure à 1.
Le pas de temps vaut %(r3)f
On arrete de le réduire, car le nombre de réductions a atteint %(i1)d, qui est le maximum possible.
"""),

17: _("""
 méthode à pas adaptatif : pas de temps minimal atteint
"""),

18: _("""
 methode des différences centrees:  la donnee du pas est obligatoire 
"""),

19: _("""
 methode des différences centrees:  le pas ne peut pas etre nul  
"""),

20: _("""
 le chargement de type dirichlet nécessite la résolution par le schema de NEWMARK
"""),

21: _("""
Nombre de pas de calcul : %(i1)d
Nombre d'itérations     : %(i2)d
"""),

23: _("""
 vous calculez une impédance absorbante
"""),

24: _("""
 on n'a pas pu trouver le dernier instant sauvé.
"""),

25: _("""
 le champ "DEPL" n'est pas trouvé dans le concept DYNA_TRANS  %(k1)s 
"""),

26: _("""
 le champ "VITE" n'est pas trouvé dans le concept DYNA_TRANS  %(k1)s 
"""),

27: _("""
 le champ "acce" n'est pas trouve dans le concept dyna_trans  %(k1)s 
"""),

28: _("""
 déplacements initiaux nuls.
"""),

29: _("""
 vitesses initiales nulles.
"""),

30: _("""
 NUME_INIT: on n'a pas trouvé le NUME_INIT dans le résultat  %(k1)s 
"""),

31: _("""
 methode de NEWMARK ou WILSON: la donnée de LIST_INST ou FONC_INST est obligatoire 
"""),

32: _("""
 FONC_INST: on attend une fonction.
"""),

33: _("""
 fonc_inst: il faut une fonction à pas constant.
"""),

34: _("""
 fonc_inst: temps de reprise supérieur à la fonction.
"""),

35: _("""
 on n'a pas pu trouver le dernier instant sauvé.
"""),

36: _("""
 NUME_INIT: on n'a pas trouvé le NUME_INIT dans le résultat  %(k1)s 
"""),

37: _("""
 incohérence sur H, ALPHA, ELAS
"""),

40: _("""
 le nom_cham  %(k1)s n'appartient pas à la sd
"""),

41: _("""
 erreur(s) dans les données
"""),

42: _("""
 critère inconnu :  %(k1)s 
"""),

45: _("""
 le champ absolu n'est accessible qu'en présence de modes statiques
"""),

47: _("""
 débordement tableau
"""),

48: _("""
 absence de LIST_INST dans INCREMENT
"""),

49: _("""
 il faut definir "LIST_ARCH" ou "LIST_INST" ou "INST" ou "PAS_OBSE"
 au premier mot cle facteur "OBSERVATION"
"""),

50: _("""
 seule la valeur de "LIST_ARCH" ou "LIST_INST" ou "INST" ou "PAS_OBSE"
 du premier mot cle facteur "OBSERVATION" est prise en compte
"""),

51: _("""
 pas de modélisation C_PLAN pour la plasticité à gradient
"""),

52: _("""
 pas de modélisation C_PLAN pour Rousselier à gradient
"""),

53: _("""
 comportement non prévu pour un algorithme de lagrangien augmenté
"""),

54: _("""
 le modèle explose - découpez votre pas de temps
"""),

55: _("""
 ITER_INTE_MAXI insuffisant
"""),

56: _("""
 la durée du transitoire est limitée par les possibilités de la transformée de Fourier rapide 
"""),

57: _("""
 la durée de la simulation temporelle est insuffisante pour le passage du transitoire
"""),

58: _("""
 changement de signe de la vitesse --> on prend VITG0(I)
"""),

59: _("""
 la matrice est triangulaire superieur-inversion indice
"""),

60: _("""
 pivot nul.
"""),

61: _("""
 option non prévue !
"""),

62: _("""
 pb 1 test spectre fi par ARPACK
"""),

63: _("""
 pb 2 test spectre fi par ARPACK
"""),

64: _("""
 valeur de STOGI incoherente
"""),

65: _("""
 en parallèle STOGI=OUI obligatoire pour l'instant
"""),

66: _("""
 option de calcul incohérente
"""),

67: _("""
 pb division par zéro dans la construction du BETA
"""),

68: _("""
 incoherence 1 .lili et .flin
"""),

69: _("""
 incoherence 2 .lili et .flin
"""),

70: _("""
 incoherence 3 .lili et .flin
"""),

71: _("""
 incoherence .lili et .fel3
"""),

72: _("""
 donnée erronnée, multiplicité nulle
"""),

73: _("""
 erreur logarithme négatif ou nul
"""),

74: _("""
 ERREUR_GAMDEV : ALPHA < 1
"""),

75: _("""
  UNIF < 0 
"""),

76: _("""
 le type de concept: TABLE_SDASTER doit etre associé au mot clé NUME_VITE_FLUI
"""),

77: _("""
 y a un bug 4
"""),

78: _("""
 pas de discrétisation de l'interspectre non constant.
"""),

79: _("""
 discrétisations differentes selon les fonctions de l'interspectre
"""),

80: _("""
 "NB_POIN" n est pas une puissance de 2
 on prend la puissance de 2 supérieure
"""),

81: _("""
 coefficient de dispersion trop grand
 consulter la documentation d'utilisation
"""),

82: _("""
 matrice moyenne non définie positive
"""),

83: _("""
 le pas tend vers 0 ...
"""),

84: _("""
 erreur dans la récuperation du NUME.PRNO .
"""),

86: _("""
 pas d'interpolation possible pour les fréquences.
"""),

87: _("""
 derivée de F nulle
"""),

88: _("""
 GM négatif
"""),

89: _("""
 valeurs propres non ordonnées :
 %(k1)s  %(k2)s  %(k3)s 
"""),

90: _("""
 coefficients paraboliques pas compatibles
"""),

91: _("""
 coefficients paraboliques pas compatibles 2
"""),

92: _("""
 modelisations C_PLAN et 1D pas autorisées
"""),

93: _("""
 zero elongation for hyperelastic material
"""),

94: _("""
 T1*C33-T3*C33 is zero for hyperelastic material
"""),

95: _("""
 error of elongation for hyperelastic material
"""),

96: _("""
 no material data for hyperelastic
"""),

97: _("""
 model not supported for hyperelastic material
"""),

98: _("""
 check your poisson ratio
"""),

99: _("""
 erreur numero colonne (dvlp)
"""),
}
