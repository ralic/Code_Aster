#@ MODIF algorith3 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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
 certains pas de temps de la liste (liste_inst) sont plus petits que le pas de temps minimal renseigne (subd_pas_mini)
"""),

2: _("""
 il faut donner subd_niveau et/ou subd_pas_mini
"""),

3: _("""
 le nombre de subdivisions du pas de temps doit etre plus grand que 1 (subd_pas)
"""),

4: _("""
 option inconnue < %(k1)s > incoherence catalogue
"""),

5: _("""
 methode de subdivision inconnue < %(k1)s > incoherence catalogue
"""),

6: _("""
 valeur de subd_iter_igno incoherent avec iter_glob_maxi. augmentez iter_glob_maxi
"""),

7: _("""
 valeur de subd_iter_fin incoherent avec iter_glob_maxi. augmentez iter_glob_maxi
"""),

8: _("""
 element non traite
"""),

9: _("""
 utiliser la formulation en deplacement avec un schema newmark, hht ou theta_methode,
 ou la formulation en acceleration avec un schema diff_cent ou tchamwa.
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
 methode a pas adaptatif  la donnee du pas est obligatoire 
"""),

12: _("""
 methode a pas adaptatif le pas ne peut pas etre nul  
"""),

13: _("""
 les matrices de masse elementaires doivent obligatoirement avoir ete calculees avec l'option mass_meca_diag
"""),

14: _("""
 on archive au moins un champ.
"""),

15: _("""
 champ " %(k1)s " deja existant
"""),








17: _("""
 methode adapt pas de temps minimal atteint
"""),

18: _("""
 methode des differences centrees:  la donnee du pas est obligatoire 
"""),

19: _("""
 methode des differences centrees:  le pas ne peut pas etre nul  
"""),

20: _("""
 le chargement de type dirichlet necessite la resolution par le schema de newmark
"""),

23: _("""
 vous calculez une impedance absorbante
"""),

24: _("""
 on n'a pas pu trouve le dernier instant sauve.
"""),

25: _("""
 le champ "depl" n'est pas trouve dans le concept dyna_trans  %(k1)s 
"""),

26: _("""
 le champ "vite" n'est pas trouve dans le concept dyna_trans  %(k1)s 
"""),

27: _("""
 le champ "acce" n'est pas trouve dans le concept dyna_trans  %(k1)s 
"""),

28: _("""
 deplacements initiaux nuls.
"""),

29: _("""
 vitesses initiales nulles.
"""),

30: _("""
 nume_init: on n'a pas trouver le nume_init dans le resultat  %(k1)s 
"""),

31: _("""
 methode de newmark ou wilson: la donnee de list_inst ou fonc_inst est obligatoire 
"""),

32: _("""
 fonc_inst: on attend une fonction.
"""),

33: _("""
 fonc_inst: il faut une fonction a pas constant.
"""),

34: _("""
 fonc_inst: temps de reprise superieur a la fonction.
"""),

35: _("""
 on n'a pas pu trouver le dernier instant sauve.
"""),

36: _("""
 nume_init: on n'a pas trouve le nume_init dans le resultat  %(k1)s 
"""),

37: _("""
 incoherence sur h, alpha, elas
"""),

40: _("""
 le nom_cham  %(k1)s n'appartient pas a la sd
"""),

41: _("""
 erreur(s) dans les donnees
"""),

42: _("""
 critere inconnu :  %(k1)s 
"""),

45: _("""
 le champ absolu n'est accessible qu'en presence de modes statiques
"""),








47: _("""
 debordement tableau
"""),

48: _("""
 pas de list_inst dans increment !?!?!
"""),

49: _("""
 il faut definir "list_arch" ou "list_inst" ou "inst" ou "pas_obse" au premier mot cle facteur "observation"
"""),

50: _("""
 seule la valeur de "list_arch" ou "list_inst" ou "inst" ou "pas_obse" du premier mot cle facteur "observation" est prise en compte
"""),

51: _("""
 pas de modeilisation c_plan pour la plasticite a gradient
"""),

52: _("""
 pas de modelisation c_plan pour rousselier a gradient
"""),

53: _("""
 comportement non prevu pour un algo de lagrangien augmente
"""),

54: _("""
 le modele explose - decouper votre pas de temps
"""),

55: _("""
 iter_inte_maxi insuffisant
"""),

56: _("""
 duree du transitoire limitee par les possibilites de la transformee de fourier rapide 
"""),

57: _("""
 duree de la simulation temporelle insuffisante pour passage du transitoire
"""),

58: _("""
 changement de signe de la vitesse --> on prend vitg0(i)
"""),

59: _("""
 la matrice est triangulaire superieur-inversion indice
"""),

60: _("""
 pivot nul.
"""),

61: _("""
 option non prevue !
"""),

62: _("""
 pb 1 test spectre fi par arpack !
"""),

63: _("""
 pb 2 test spectre fi par arpack !
"""),

64: _("""
 valeur de stogi incoherente !
"""),

65: _("""
 en parallele stogi=oui obligatoire pour l'instant !
"""),

66: _("""
 option de calcul incoherente !
"""),

67: _("""
 pb division par zerodans la construction du beta !
"""),

68: _("""
 incoherence 1 .lili et .flin !
"""),

69: _("""
 incoherence 2 .lili et .flin !
"""),

70: _("""
 incoherence 3 .lili et .flin !
"""),

71: _("""
 incoherence .lili et .fel3 !
"""),

72: _("""
 donnee erronnee, multiplicite nulle !
"""),

73: _("""
 erreur log negatif ou nul
"""),

74: _("""
 erreur_gamdev : alpha < 1
"""),

75: _("""
  unif < 0 
"""),

76: _("""
 le type de concept: table_sdaster doit etre associe au mot cle nume_vite_flui
"""),

77: _("""
 y a un bug 4
"""),

78: _("""
 pas de discretisation de l'interspectre non constant.
"""),

79: _("""
 discretisations differentesselon les fonctions de l'interspectre
"""),

80: _("""
 "nb_poin" n est pas une puissance de 2,on prend la puissance de 2 superieure
"""),

81: _("""
 coefficient de dispersion trop grand, cf. doc. u
"""),

82: _("""
 matrice moyenne non definie positive
"""),

83: _("""
 le pas tend vers 0 ...
"""),

84: _("""
 erreur dans la recuperation du nume.prno .
"""),

85: _("""
 lameq n'a pas converge
"""),

86: _("""
 pas d'interpolation possible pour  les frequences.
"""),

87: _("""
 derivee de f nulle
"""),

88: _("""
 gm negatif
"""),

89: _("""
 valeurs propres non//                              ordonnees %(k1)s  %(k2)s  %(k3)s 
"""),

90: _("""
 coef paraboliques pas compatibles
"""),

91: _("""
 coef paraboliques pas compatibles 2
"""),

92: _("""
 modelisations c_plan et 1d pas autorisees
"""),

93: _("""
 zero elongation for hyperelastic material
"""),

94: _("""
 t1*c33-t3*c33 is zero for hyperelastic material
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
