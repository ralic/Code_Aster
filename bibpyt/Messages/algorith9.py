#@ MODIF algorith9 Messages  DATE 30/04/2007   AUTEUR ABBAS M.ABBAS 
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
 le champ de temperature : temp_init(num_init) n'existe pas.
"""),

2: _("""
 cham_no invalide
"""),

3: _("""
 le mot cle "nume_init" est obligatoire dans ce cas.
"""),

4: _("""
 valeur de theta illicite
"""),

5: _("""
 la charge  %(k1)s  n'est pas thermique
"""),

6: _("""
 la charge cine  %(k1)s  n"a pas de .defi
"""),

7: _("""
 la charge  %(k1)s  n'est pas compatible avec fonc_mult
"""),

9: _("""
 composante non trouvee dans la numerotation
"""),

10: _("""
 nombre de vecteurs demande trop grand on prend tous les modes du concept mode_meca 
"""),

12: _("""
 borne inf incorrect
"""),

13: _("""
 probleme avec courbe de traction
"""),

14: _("""
 pente non trouvee
"""),

15: _("""
 la norme du vecteur vect_orie est nulle
"""),

16: _("""
 le pas est nul
"""),

17: _("""
 le nombre de pas est negatif
"""),

18: _("""
 les matrices assemblees generalisees doivent avoir un stockage plein (cf. nume_ddl_gene)
"""),

19: _("""
 coeff_var_amor non nul et amortissement non present
"""),





24: _("""
 charge de contact non traitee
"""),

25: _("""
 votre chargement contient plus d'une charge repartie. le calcul n'est paspossible pour les modeles de poutre
"""),

26: _("""
 le modele est obligatoire
"""),

27: _("""
 impossible de combiner les mots cles charge et vect_asse en dehors des ondes planes
"""),

28: _("""
 concept reentrant : "resultat" doit porter le meme nom que la sortie
"""),

29: _("""
 concept reentrant : "resultat" est d'un type different
"""),

30: _("""
 argument en double pour "nom_cham"
"""),

31: _("""
 pour l'instant, on ne peut pas deriver sur base modale dans dyna_line_harm 
"""),

34: _("""
 les matrices ne possedent pas toutes la meme numerotation 
"""),

35: _("""
  un vect_asse n'est ni a valeurs reelles, ni a valeurs complexes.
"""),

36: _("""
 erreur dans decoupe initiale des pas
"""),

37: _("""
 attention, arret=non donc poursuite du calcul sans avoir eu convergence
"""),

39: _("""
 base modale et matr_asse avec numerotations differentes
"""),

40: _("""
  type de matrice inconnu:  %(k1)s 
"""),

41: _("""
 base modale et vect_asse avec  numerotations differentes
"""),

42: _("""
 la base constituee ne forme pas une famille libre 
"""),

43: _("""
 le nombre de valeurs doit etre pair.
"""),

44: _("""
 trop d'arguments pour "nom_cham"
"""),

45: _("""
 pour calculer une acce_absolu, il faut "acce_mono_appui"
"""),

46: _("""
 pour restituer sur un squelette, il faut "mode_meca"
"""),

47: _("""
 mots-cles'sous_struc' et'squelette'interdits
"""),

48: _("""
 mots-cle'mode_meca' doit etre present
"""),

49: _("""
 l'instant de recuperation est en dehors du domaine de calcul.
"""),

50: _("""
 pas de mailles fournies
"""),

51: _("""
 aucune maille enrichie
"""),

55: _("""
 mauvaise definition de l'interspectre.
"""),

56: _("""
 le "nomb_pts" doit etre une puissance de 2.
"""),

57: _("""
 si les mots-cles <nume_ordre> et <amor_reduit> sont utilises, il faut autant d arguments pour l un et l autre
"""),

58: _("""
 le concept mode_meca d entree doit etre celui correspondant a la base modale initiale pour le calcul de couplage fluide-structure
"""),

59: _("""
 numero de vitesse d ecoulement du fluide non valide
"""),

60: _("""
 tous les modes non couples etant retenus, le nombre d arguments valide pour le mot-cle <amor_reduit> est la difference entre le nombre de modes de la base modale initiale et le nombre de modes pris en compte pour le couplage fluide-structure
"""),

61: _("""
 les numeros d ordre fournis ne correspondent pas a des modes non perturbes
"""),

62: _("""
 option symetrie : la dimension de point et axe_1 doit etre identique.
"""),

63: _("""
 option symetrie : axe_2 est inutile en 2d, il est ignore.
"""),

64: _("""
 option symetrie : la dimension de point et axe_2 doit etre identique.
"""),

65: _("""
 methode: elem autorisee seulement pour les resultats evol_xxx.
"""),

66: _("""
 methode: nuage_deg__* autorisee seulement pour les champs.
"""),

69: _("""
 on ne sait pas traiter le champ de type:  %(k1)s  champ :  %(k2)s 
"""),

70: _("""
 le nom du champ         de la variable de commande  pour decoupage obligatoire
"""),

71: _("""
 le nom du cmp           de la variable de commande  pour decoupage obligatoire
"""),

72: _("""
 critere pour            decoupage obligatoire
"""),

73: _("""
 erreur : itran = 0 ou 1
"""),

74: _("""
 attention, mode suramorti
"""),

75: _("""
 attention, mode instable
"""),










80: _("""
 pour utiliser le comportement "hydr" il faut surcharger le code en "mode developpement" avec les routines "permea" et "satura".
"""),

81: _("""
 le vecteur directeur est nul.
"""),

82: _("""
 erreur dvp
"""),

83: _("""
 nombre max iterations atteint
"""),

84: _("""
 precision machine depassee
"""),

85: _("""
 probleme pilo : 3 solutions ou plus
"""),

86: _("""
 matrice mat non inversible
"""),

87: _("""
 probleme pilo
"""),

88: _("""
 ldc non dispo pour pilotage
"""),

89: _("""
 le pilotage pred_elas necessite eta_pilo_min et eta_pilo_max pour la loi endo_isot_beton
"""),

90: _("""
 le pilotage pred_elas necessite eta_pilo_min et eta_pilo_max pour la loi endo_orth_beton
"""),

91: _("""
 nb noeuds mesure doit etre inf a nb noeuds modele
"""),

92: _("""
 maille seg2 non trouvee
"""),

93: _("""
 integration elastoplastique de loi beton_double_dp : pas de convergence lors de la projection au sommet des cones de traction et de compression --> utiliser le redecoupage automatique du pas de temps.
"""),

94: _("""
 integration elastoplastique de loi beton_double_dp : pas de convergence lors de la resolution pour nseuil =  %(k1)s  --> utiliser le redecoupage automatique du pas de temps.
"""),

95: _("""
 non convergence a la maille:  %(k1)s 
"""),

96: _("""
 la saturation n'est pas une variable interne pour la loi de couplage  %(k1)s 
"""),

97: _("""
 la pression de vapeur n est pas une variable interne pour la loi de couplage  %(k1)s 
"""),


99: _("""
 la variable  %(k1)s  n existe pas dans la loi cjs en 2d
"""),
}
