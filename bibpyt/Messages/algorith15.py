#@ MODIF algorith15 Messages  DATE 25/06/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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

cata_msg={
1: _("""
 arret sur nombres de ddl interface non identiques 
 nombre de ddl interface droite:  %(i1)d 
 nombre de ddl interface gauche:  %(i2)d 
"""),

2: _("""
 arret sur dimension matrice teta incorrecte dimension effective:  %(i1)d 
 dimension en argument:  %(i2)d 
"""),

3: _("""
  erreur  de repetitivite cyclique
"""),

4: _("""
  il manque un ddl sur un noeud  gauche type du ddl -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),

5: _("""
  erreur  de repetitivite cyclique
"""),

6: _("""
  il manque un ddl sur un noeud droite type du ddl -->  %(k1)s 
  nom du noeud -->  %(k2)s 
"""),

7: _("""
 arret sur probleme de repetitivite cyclique
"""),

8: _("""
 cmp est une composante indefiniecmp= %(k1)s 
"""),

9: _("""
 
"""),

10: _("""
 arret sur type de ddl non defini
"""),

11: _("""
 "nb_poin" est inferieur au nombre de points de l'interspectre.
 le spectre est tronquee a la frequence :  %(r1)f 
"""),

12: _("""
 le "nb_poin" donne est modifie (en une puissance de 2 compatible avec l'interspectre). 
 le "nb_poin" retenu  est :   %(i1)d 
"""),

13: _("""
 la duree est trop grande ou nb_poin et trop petit par rapport a la frequence max (th. de shannon).
 on choisit nbpoin =  %(i1)d 
"""),

14: _("""
 la duree est petite par rapport au pas de discretisation de l'interspectre.
 choisir plutot : duree >  %(r1)f 
"""),

15: _("""
 "nb_poin" est petit par rapport au pas de discretisation de l'interspectre.
 nb_poin =  %(i1)d 
 il faudrait un nbre superieur a :  %(r1)f 
"""),

16: _("""
 on n'as pas trouve le ddl   pour le noeud :  %(k1)s 
"""),

17: _("""
    de la sous-structure :  %(k1)s 
"""),

18: _("""
    et sa composante :  %(k1)s 
"""),

19: _("""
  il manque le seuil  pour la fonction interpretee  %(k1)s 
"""),

20: _("""
 l'abscisse lineaire est nulle   pour la courbe :  %(k1)s 
          abscisse :  %(r1)f 
"""),

21: _("""
 on n'as pas trouve le ddl   pour le noeud :  %(k1)s 
"""),

22: _("""
    de la sous-structure :  %(k1)s 
"""),

23: _("""
    et sa composante :  %(k1)s 
"""),

24: _("""
 au moins un terme de alpha est negatif a l'abscisse :  %(i1)d 
"""),

25: _("""
 alpha est nul et le nombre de mesures est strictement inferieur au nombre de modes : risque de matrice singuliere
"""),

26: _("""
 calcul moindre norme 
"""),

27: _("""
  pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

28: _("""
  matrice (phi)t*phi + alpha n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

29: _("""
  pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

30: _("""
  matrice (phi)t*phi + alpha  n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

31: _("""
 au moins un terme de alpha est negatif a l'abscisse :  %(i1)d 
"""),

32: _("""
 alpha est nul et le nombre de mesures est strictement inferieur au nombre de modes : risque de matrice singuliere
"""),

33: _("""
 calcul moindre norme 
"""),

34: _("""
 pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

35: _("""
  matrice (phi)t*phi + alpha n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

36: _("""
  pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

37: _("""
  matrice (phi)t*phi + alpha  n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

38: _("""
 au moins un terme de alpha est negatif a l'abscisse :  %(i1)d 
"""),

39: _("""
 alpha est nul et le nombre de mesures est strictement inferieur au nombre de modes : risque de matrice singuliere
"""),

40: _("""
 calcul moindre norme 
"""),

41: _("""
 pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

42: _("""
  matrice (phi)t*phi + alpha n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

43: _("""
  pb calcul valeurs singulieres pas =   %(i1)d  abscisse =    %(r1)f 
"""),

44: _("""
  matrice (phi)t*phi + alpha  n est pas inversible  pas =   %(i1)d 
  abscisse =    %(r1)f 
"""),

45: _("""
  on ne trouve pas dpmax 
"""),

46: _("""
  nombre d iterations insuffisant 
"""),

47: _("""
  f(xmin) > 0 
"""),

48: _("""
  maille :  %(k1)s  nombre d iterations =  %(i1)d  iter_inte_maxi =  %(i2)d 
"""),

49: _("""
  dp   actuel =  %(r1)f  f(dp) actuel =  %(r2)f 
"""),

50: _("""
  dp  initial =  %(r1)f  f(dp) init   =  %(r2)f 
"""),

51: _("""
  dp  maximum =  %(r1)f f(dp) maxi   =  %(r2)f 
"""),

52: _("""
  allure de la fonction. nb points :  %(i1)d 
"""),

53: _("""
  dp  =  %(r1)f f(dp)  =  %(r2)f 
"""),

54: _("""
 
"""),

55: _("""
  incoherence detectee 
"""),

56: _("""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s 
  n appartient pas la sous-structure:  %(k3)s 
"""),

57: _("""
  incoherence detectee 
"""),

58: _("""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s 
  n est pas correctement reference dans le squelette :  %(k3)s 
"""),

59: _("""
  incoherence detectee 
"""),

60: _("""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s 
  n appartient pas la sous-structure:  %(k3)s 
"""),

61: _("""
  incoherence detectee 
"""),

62: _("""
  le noeud :  %(k1)s  de l interface dynamique :  %(k2)s 
  n est pas correctement reference dans le squelette :  %(k3)s 
"""),

63: _("""
  conflit mot cles tout et group_no dans reco_global 
"""),

64: _("""
  erreur de nom  la sous-structure :  %(k1)s  n a pas ete trouvee 
"""),

65: _("""
  incoherence de nom  l interface dynamique :  %(k1)s 
  de la sous-structure :  %(k2)s 
  a pour groupe de noeud :  %(k3)s 
  or group_no_1 =  %(k4)s 
"""),

66: _("""
  erreur de nom  la sous-structure :  %(k1)s  n a pas ete trouvee 
"""),

67: _("""
  incoherence de nom  l interface dynamique :  %(k1)s 
  de la sous-structure :  %(k2)s 
  a pour groupe de noeud :  %(k3)s 
  or group_no_2 =  %(k4)s 
"""),

68: _("""
  --------------------------------nombre de points pas periode:  %(i1)d 
 coefficient de remontee du pas de temps:  %(r1)f 
 coefficient de division du pas de temps:  %(r2)f 
 coeff determinant dt min (=dt init*coeff):  %(r3)f 
 nombre maximal de reductions du pas:  %(i2)d 
 vitesse minimale variable:  %(k1)s 
"""),

69: _("""
 arret : nombre incorrect de sous-structuresil vaut :  %(i1)d 
 alors que le nombre total de sous-structures vaut :  %(i2)d 
"""),

70: _("""
 arret : nombre incorrect de sous-structurespour le chargement numero : %(i1)d 
 il en faut exactement :  %(i2)d 
 vous en avez :  %(i3)d 
"""),

71: _("""
 arret : nombre incorrect de vecteurs chargementspour le chargement numero : %(i1)d 
 il en faut exactement :  %(i2)d 
 vous en avez :  %(i3)d 
"""),

72: _("""
 arret : un prof_chno n'est pas definiil manque pour le chargement : %(k1)s 
"""),

73: _("""
 on doit avoir le meme type de forces pour  un meme chargement global
 or, la grandeur vaut :  %(i1)d 
 pour la sous-structure  %(k1)s 
 et elle vaut :  %(i2)d 
 pour la sous-structure  %(k2)s 
"""),

74: _("""
 une des bases modales a un type incorrect
 elle est associee a la sous-structure  %(k1)s 
"""),

75: _("""
 les numerotations ne coincident paspour la sous-structure : %(k1)s 
 le prof_chno pour la base modale est :  %(k2)s 
 et celui pour le second membre :  %(k3)s 
"""),

76: _("""
  Les deux interfaces n'ont pas le meme nombre de noeuds
     Nombre de noeuds interface droite --> %(i1)d 
     Nombre de noeuds interface gauche --> %(i2)d 
"""),

77: _("""
     Conflit dans les VIS_A_VIS des noeuds :
       Le noeud %(k1)s est le VIS-A-VIS des noeuds %(k2)s et %(k3)s
"""),

78: _("""
     Axe de symétrie cyclique différent de OZ
        Numéro du couple de noeud : %(i1)d 
        Noeud droite --> %(k1)s
        Noeud gauche --> %(k2)s
"""),
79: _("""
     Problème de rayon droite-gauche différents:
        Numéro du couple de noeud : %(i1)d 
        Noeud droite --> %(k1)s
        Noeud gauche --> %(k2)s
"""),
80: _("""
     Problème signe angle entre droite et gauche:
        Numéro du couple de noeud : %(i1)d 
        Noeud droite --> %(k1)s
        Noeud gauche --> %(k2)s
"""),
81: _("""
     Problème valeur angle repetitivité cyclique:
        Numéro du couple de noeud : %(i1)d 
        Noeud droite --> %(k1)s
        Noeud gauche --> %(k2)s
"""),

82: _("""
     Vérification répétitivité: aucune erreur détectée
"""),

83: _("""
     Les noeuds des interfaces ne sont pas alignes en VIS_A_VIS
     Les noeuds ont été réordonnés.
"""),
84: _("""
     Arret sur problème répétitivité cyclique
     Tentative de diagnostique %(k1)s
 """),

85: _("""
  l''interface de droite  n existe pasinterface de nom %(k1)s 
"""),

86: _("""
  l''interface de gauche  n existe pasinterface de nom %(k1)s 
"""),

87: _("""
  l''interface axe  n existe pasinterface de nom %(k1)s 
"""),

88: _("""
 arret sur probleme interfaces de type differents
"""),

89: _("""
 arret sur probleme de type interface non supportetype interface -->  %(k1)s 
"""),

90: _("""
 le nombre d'amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d 
 et le nombre de coefficients  :  %(i2)d 
 on ne garde donc que les  %(i3)d premiers coefficients
"""),

91: _("""
 le nombre d'amortissements reduits est insuffisant il en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d 
 on rajoute %(i3)d coefficients avec la valeur du dernier coefficient.
"""),

92: _("""
 Nombre de modes propres calculés insuffisant.
"""),

93: _("""
 MODE_MECA : %(k1)s 
"""),

94: _("""
 Nombre de modes propres limités à : %(i1)d 
"""),

95: _("""
 l'entree d'amortissements reduits est incompatible 
 avec des matrices de type  %(k1)s 
 Il faut des matrices de type MATR_ASSE_GENE_*
 
"""),

96: _("""
 le nombre d''amortissements reduits est trop grand
 le nombre de modes propres vaut  %(i1)d 
 et le nombre de coefficients :  %(i2)d 
 on ne garde donc que les  %(i3)d premiers coefficients 
 
"""),

97: _("""
 le nombre d'amortissements reduits est insuffisant il en manque :  %(i1)d 
 car le nombre de modes vaut :  %(i2)d  on rajoute  %(i3)d 
 amortissement reduits avec la valeur du dernier mode propre.
"""),

98: _("""
 
 incoherance dans le dataset 58 : le nombre de valeurs fournies ne correspond pas au nombre de valeurs attendues
 mesure concernee :  %(i1)d 
 
"""),

99: _("""
  ! le nb max d''iterations  %(i1)d ! est atteint sans converger 
 ! le residu relatif final est  : %(r1)f 
 ! l instant de calcul vaut : %(r2)f 
 
"""),

}
