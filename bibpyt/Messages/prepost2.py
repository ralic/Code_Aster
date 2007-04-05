#@ MODIF prepost2 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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

2: _("""
 modele inconnu, pas d'impression du champ  %(k1)s 
"""),

3: _("""
 on ne sait pas ecrire des  champs par element aux points de gauss au format castem
"""),

4: _("""
 erreur programmation
"""),

5: _("""
 on ne sait pas faire avec existc = 1
"""),

6: _("""
 med: erreur efchac numero  %(k1)s 
"""),

7: _("""
 veritable ecriture des tableaux de valeurs
"""),

8: _("""
 med: erreur efchre numero  %(k1)s 
"""),

9: _("""
 med: erreur efouvr numero  %(k1)s 
"""),

10: _("""
 med: erreur efnpro numero  %(k1)s 
"""),

11: _("""
 probleme dans la lecture du nom du profil.
"""),

12: _("""
 probleme dans la lecture des valeurs du profil.
"""),

13: _("""
 med: erreur efferm numero  %(k1)s 
"""),

14: _("""
 med: erreur efpfle numero  %(k1)s 
"""),

15: _("""
 1.3. type de champ inconnu :  %(k1)s 
"""),

16: _("""
 creation des tableaux de valeurs a ecrire avec :
"""),

17: _("""
 renumerotation impossible avec plus d'un sous-point
"""),

18: _("""
 la programmation ne permet pas plus de 150 cmps.
"""),

19: _("""
 le le nombre de cmps de la grandeur du champ depasse 500 augmenter la dimension de 4 tableaux statiques
"""),

20: _("""
  impr_resu format ensight : la composante  %(k1)s  du cham_gd  %(k2)s  n'est portee par aucun noeud du maillage et ne sera pas imprimee
"""),

21: _("""
  impr_resu format ensight : la composante  %(k1)s  du champ  %(k2)s  du concept  %(k3)s  n'est portee par aucun noeud du maillage et ne sera pas imprimee
"""),

22: _("""
  impr_resu format ensight : les trois composantes  %(k1)s   %(k2)s   %(k3)s  du cham_gd  %(k4)s  ne sont portees par aucun noeud du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

23: _("""
  impr_resu format ensight : les trois composantes  %(k1)s   %(k2)s   %(k3)s  du champ  %(k4)s  du concept  %(k5)s  ne sont portees par aucun noeud du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

24: _("""
 impression de la composante  %(k1)s  du champ  %(k2)s  sous forme d'une variable scalaire ensight  %(k3)s . la composante  %(k4)s  n'est pas portee par certains noeuds du maillage et des valeurs nulles sont affectees a ces noeuds dans les fichiers  %(k5)s 
"""),

25: _("""
 impression des 3 composantes  %(k1)s   %(k2)s  et  %(k3)s  du champ  %(k4)s  sous forme d'une variable vectorielle ensight  %(k5)s . une ou plusieurs de ces composantes ne sont pas portees par certains noeuds du maillage et des valeurs nulles sont affectees a ces noeuds pour ces composantes dans les fichiers  %(k6)s 
"""),

26: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie reelle du cham_gd  %(k2)s 
"""),

27: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie relle du champ  %(k2)s  du concept %(k3)s 
"""),

28: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie imaginaire du cham_gd  %(k2)s 
"""),

29: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression de la partie imaginaire du champ  %(k2)s  du concept %(k3)s 
"""),

30: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s .r : ce fichier existe deja !! l'impression du meme champ ouconcept resultat est demandee deux fois ou une meme composante est selectionnee 2 fois sous le mot-cle nom_cmp
"""),

31: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression du cham_gd  %(k2)s 
"""),

32: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  pour l'impression du champ  %(k2)s  du concept  %(k3)s 
"""),

33: _("""
 probleme a l'ouverture du fichier de valeurs ensight  %(k1)s  : ce fichier existe deja !! l'impression du meme champ ouconcept resultat est demandee deux fois ou une meme composante est selectionnee 2 fois sous le mot-cle nom_cmp
"""),








35: _("""
   desole on ne sait pas ecrire les champs aux noeuds de representation constante et a valeurs complexes au format  %(k1)s 
"""),

36: _("""
   desole on ne sait pas ecrire le champ aux noeuds  %(k1)s  au format  %(k2)s 
"""),

37: _("""
 le maillage  %(k1)s  associe au cham_gd  %(k2)s  est different du maillage  %(k3)s  en operande de la commande !!!! attention !!!!
"""),

38: _("""
 le maillage  %(k1)s  associe au champ  %(k2)s  du concept  %(k3)s  est different du maillage  %(k4)s  en operande de la commande !!!! attention !!!!
"""),

39: _("""
 probleme a l'ouverture du fichier d'avertissement  %(k1)s 
"""),

40: _("""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du cham_gd  %(k1)s  n'est presente dans la grandeur  %(k2)s 
"""),

41: _("""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du champ  %(k1)s  du concept  %(k2)s  n'est presente dans la grandeur  %(k3)s 
"""),

42: _("""
  impr_resu format ensight : la composante  %(k1)s  du cham_gd  %(k2)s  n'est pas portee par les noeuds du maillage et ne sera pas imprimee
"""),

43: _("""
  impr_resu format ensight : la composante  %(k1)s  du champ  %(k2)s  du concept  %(k3)s  n'est pas portee par les noeuds du maillage et ne sera pas imprimee
"""),

44: _("""
  impr_resu format ensight : aucune des trois composantes  %(k1)s   %(k2)s   %(k3)s  du cham_gd  %(k4)s  n'est portee par les noeuds du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

45: _("""
  impr_resu format ensight : aucune des trois composantes  %(k1)s   %(k2)s   %(k3)s  du champ  %(k4)s  du concept  %(k5)s  n'est portee par les noeuds du maillage et le vecteur ensight correspondant ne sera pas imprime
"""),

46: _("""
  numero d'ordre  %(k1)s  non licite 
"""),

47: _("""
  le numero d'ordre suivant est desormais considere comme le premier numero d'ordre demande
"""),

48: _("""
  pour certains numeros d'ordre le champ  %(k1)s  n'est pas present dans la sd_resultat  %(k2)s ==> des fichiers de valeurs vides seront generes afin de respecter le format ensight.
"""),

49: _("""
 le nombre de cmps de la grandeur du champ depasse 500 augmenter la dimension du tableau ivari
"""),

50: _("""
 une meme composante est donnee 2 fois sous le mot-cle nom_cmp lors de l'impression au format ensight par impr_resu
"""),

51: _("""
 type de structure non traite:  %(k1)s 
"""),

52: _("""
 on imprime que des champs elno
"""),

53: _("""
 nbcmp different
"""),

54: _("""
 composante inconnue %(k1)s 
"""),

55: _("""
 attention, il faut specifier les noms des composantes du tenseur pour pouvoir les visualiser separement avec gmsh
"""),

56: _("""
 on imprime que des champs "elga" ou "elem"
"""),

57: _("""
 nbsp different de 1
"""),

58: _("""
 pas de correspondance
"""),

59: _("""
 aucun champ trouve, pas d'impression au format "gmsh"
"""),

60: _("""
 on ne sait pas imprimer au format "gmsh" ce type de champ:  %(k1)s 
"""),

61: _("""
 erreur de programmation : nbcmp different de 1 ou 3.
"""),

62: _("""
 on ne peut pas traiter des elements a plus de 99 noeuds !
"""),

63: _("""
 erreur de programation
"""),

64: _("""
 seg4 element inexistant dans castem, converti en seg2
"""),

65: _("""
 tria7 element inexistant dans castem, converti en tria6
"""),

66: _("""
 quad9 element inexistant dans castem, converti en quad8
"""),

67: _("""
 les champs n'ont pas la meme grandeur
"""),

68: _("""
 les champs n'ont pas la meme numerotation
"""),

69: _("""
 les champs n'ont pas le meme type de valeurs
"""),

70: _("""
   desole on ne sait pas ecrire les champs aux noeuds de representation constante au format ideas
"""),

71: _("""
 element noeud non disponible dans ensight
"""),

72: _("""
 element  %(k1)s  non disponible dans ensight
"""),

73: _("""
 les elements tria7 seront reduits a des tria6
"""),

74: _("""
 les elements quad9 seront reduits a des quad8
"""),

75: _("""
 les elements penta15 seront reduits a des penta6
"""),

76: _("""
 il y a des groupes de noeuds dans le maillage  %(k1)s  qui n'apparaitront pas dans le fichier geometrie ensight: seuls des groupes de mailles peuvent y etre integres
"""),

77: _("""
 la dimension du probleme est invalide : il faut : 1d, 2d, 3d.
"""),

78: _("""
 hexa27 element inexistant dans ideas, converti en hexa20
"""),

79: _("""
 tria7 element inexistant dans ideas, converti en tria6
"""),

80: _("""
 quad9 element inexistant dans ideas, converti en quad8
"""),

81: _("""
 seg4 element inexistant dans ideas, converti en seg2
"""),

82: _("""
 element pyram5 non disponible dans ideas
"""),

83: _("""
 element pyram13 non disponible dans ideas
"""),

84: _("""
 code invalide : icod2
"""),

85: _("""
 description du fichier med :  %(k1)s 
"""),

86: _("""
 med: erreur effide numero  %(k1)s 
"""),

87: _("""
 ce maillage est deja present dans le fichier.
"""),

88: _("""
 ouverture du fichier med en mode  %(k1)s  %(k2)s 
"""),

89: _("""
 med: erreur efmaac numero  %(k1)s 
"""),

90: _("""
 med: erreur effamc numero  %(k1)s 
"""),

91: _("""
 med: erreur effame numero  %(k1)s 
"""),

92: _("""
 med: famille de  %(k1)s , erreur effame numero  %(k2)s 
"""),

93: _("""
 on ne sait pas ecrire les mailles de type  %(k1)s 
"""),

94: _("""
 med: erreur efcone numero  %(k1)s 
"""),

95: _("""
 med: erreur efnome numero  %(k1)s 
"""),

96: _("""
 med: erreur efnume numero  %(k1)s 
"""),

97: _("""
 med: erreur efcooe numero  %(k1)s 
"""),

98: _("""
 med: erreur efngau numero  %(k1)s 
"""),

99: _("""
 med: erreur efgaui numero  %(k1)s 
"""),
}
