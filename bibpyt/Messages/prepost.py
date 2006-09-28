#@ MODIF prepost Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 group_no :  %(k1)s  inconnu dans le maillage
"""),

2: _("""
 pour calculer le dommage il faut definir le comportement "fatigue" dans defi_materiau
"""),

3: _("""
 hors bornes  definies dans cescre.
"""),

4: _("""
 methode  %(k1)s  illicite
"""),

5: _("""
 la longueur du defaut n est pas en accord avec les tables definies cote revetement et cote metal de base
"""),

6: _("""
 incoherence catalogue - fortran
"""),

7: _("""
 erreur_dvp
"""),

8: _("""
 prolongement a gauche exclu
"""),

9: _("""
 prolongement a droite exclu
"""),

10: _("""
 phenomene non valide
"""),

11: _("""
 nous ne pouvons pas recuperer la valeur du module d'young : e.
"""),

12: _("""
 nous ne pouvons pas recuperer la valeur du coefficient de poisson : nu.
"""),

13: _("""
  le type d entite  %(k1)s  est inconnu.
"""),

14: _("""
 ipoi1 et ipoi4 non traite
"""),

15: _("""
 sprim negative bizarre
"""),

16: _("""
 airtot ou ener nul
"""),

17: _("""
 do102=0 
"""),

18: _("""
 probleme intersection cylindre tetraedre
"""),

19: _("""
 voli negative bizarre
"""),

20: _("""
 voltot ou ener nul
"""),

21: _("""
 pas inters
"""),

22: _("""
 delta negatif pb lors de recherche intersection
"""),

23: _("""
 pb n2+n3
"""),

24: _("""
 cas idiot
"""),

25: _("""
 pb en dehors des 2 points
"""),

26: _("""
  probleme n2+n3
"""),

27: _("""
 volume negatif
"""),

28: _("""
 volume superieur a 1.d6
"""),

29: _("""
 tbelzo depassement
"""),

30: _("""
 tbnozo depassement
"""),

31: _("""
 sd resultat inconnue  %(k1)s 
"""),

32: _("""
  l'impression de la sd_resultat  %(k1)s  a deja ete effectuee avec une liste de numeros d'ordre dont le premier numero etait le meme que celui de la liste actuelle. on arrete l'impression afin d'eviter l'ecrasement des fichiers ecrits.
"""),

33: _("""
 probleme a l'ouverture du fichier resultat ensight  %(k1)s  pour l'impression du cham_gd  %(k2)s 
"""),

34: _("""
 probleme a l'ouverture du fichier resultat ensight  %(k1)s  pour l'impression du concept  %(k2)s 
"""),

35: _("""
 code  %(k1)s  inconnu
"""),

36: _("""
 le champ de:  %(k1)s  a des elements ayant des sous-points. ces elements ne seront pas traites.
"""),

37: _("""
 vraiment desole
"""),

38: _("""
 le vecteur est "axe_z" nul.
"""),

39: _("""
 le coefficient de goodman n'est pas calculable
"""),

40: _("""
 le coefficient de gerber n'est pas calculable
"""),

41: _("""
 pour calculer le dommage de lemaitre-sermage il faut definir  le comportement domma_lemaitre dans defi_materiau
"""),

42: _("""
 pour calculer le dommage de lemaitre_sermage il faut definir  le comportement elas_fo dans defi_materiau
"""),

43: _("""
 le materiau est obligatoire pour le calcul du dommage par taheri_manson
"""),

44: _("""
 une fonction doit etre introduite sous le mot cle taheri_fonc
"""),

45: _("""
 une nappe doit etre introduite sous le mot cle taheri_nappe
"""),

46: _("""
 pour calculer le dommage il faut definir le comportement fatigue dans defi_materiau
"""),

47: _("""
 la courbe de manson_coffin est necessaire pour le calcul du dommage taheri_manson_coffin
"""),

48: _("""
 le materiau est obligatoire pour le calcul du dommage par taheri_mixte
"""),

49: _("""
 la courbe de manson_coffin est necessaire pour le calcul du dommage taheri_mixte
"""),

50: _("""
 la courbe de wohler est necessaire pour le calcul du dommage taheri_mixte
"""),

51: _("""
 l'option de calcul " %(k1)s " n'existe pas dans la structure de donnees %(k2)s 
"""),

52: _("""
 le champ " %(k1)s " pour l'option de calcul " %(k2)s ", n'a pas ete notee dans la structure de donnees %(k3)s 
"""),

53: _("""
 la dimension du probleme est invalide : il faut : 1d,2d ou 3d.
"""),

54: _("""
 nombre de noeuds superieur au maxi autorise : 27.
"""),

55: _("""
 objet &&gilire.indirect inexistant. probleme a la lecture des points 
"""),

56: _("""
 type de maille :  %(k1)s  inconnu de pre_gibi.
"""),

57: _("""
 nombre d objets superieur au maximum autorise : 99999.
"""),

58: _("""
 bug !!!
"""),

59: _("""
 le maillage gibi est  peut etre errone :  il est ecrit : "niveau rreur n_err"  ou n_err est >0 .on continue quand meme,  mais si vous avez des problemes plus loin ...
"""),

60: _("""
 arret sur erreur(s)
"""),

61: _("""
 erreur dans la recuperation du nume.prno
"""),

62: _("""
 probleme dans la recuperation du numero de bloc auquel appartient la ligne courante.
"""),

63: _("""
 les seuls types de valeurs acceptes pour les resu_elem sont les reels et les complexes, le descripteur de type  %(k1)s  est inadequat.
"""),

64: _("""
 les seuls types de s.d. autorises sont "matr_asse" , "matr_elem" "vect_elem" et "resu_elem", le type donne  %(k1)s  n'est pas reconnu .
"""),

65: _("""
 la valeur du grain d'impression est  %(k1)s  alors que les seules valeurs possibles sont "noeud" ou "valeur"  ou "maille".
"""),

66: _("""
 la valeur du grain d'impression est  %(k1)s  alors que les seules valeurs possibles sont "noeud" et "valeur" .
"""),

67: _("""
 probleme dans le descripteur de la matrice: l'indicateur de symetrie ne correspond ni a une matrice  symetrique, ni a une matrice  non-symetrique . 
"""),

68: _("""
 probleme dans le descripteur de la matrice: l'indicateur type de valeur de la matrice ne correspond ni a une matrice reelle, ni a une matrice  complexe . 
"""),

69: _("""
 probleme a l ouverture du fichier
"""),

70: _("""
 probleme a la fermeture du fichier
"""),

71: _("""
 maillage introuvable ?
"""),

72: _("""
 med:erreur mdnoma numero  %(k1)s 
"""),

73: _("""
 le champ est ecrit dans le fichier !
"""),

74: _("""
 la variable  %(k1)s  n'existe pas
"""),

75: _("""
 pas d'impression du champ
"""),

76: _("""
 on ne peut pas traiter plus de 999 variables internes
"""),

77: _("""
 incompatibilite entre les grels
"""),

78: _("""
 nec trop grand
"""),

79: _("""
  nombre de couches > 1 
"""),

80: _("""
 on traite les tria7 quad9 en oubliant le noeud centre
"""),

81: _("""
  : inoa=0
"""),

82: _("""
 ecriture impossible pour  %(k1)s  au format med
"""),

83: _("""
 certaines composantes selectionnees ne font pas parties du ligrel
"""),

84: _("""
 element pyran5 non disponible dans ideas
"""),

85: _("""
 element pyran13 non disponible dans ideas
"""),

86: _("""
 on traite les tria7 quad9 hexa27 en oubliant le noeud centre
"""),

87: _("""
 on ne sait pas imprimer le champ de type:  %(k1)s  champ :  %(k2)s 
"""),

88: _("""
  on ne sait pas imprimer au format ensight le champ  %(k1)s  correspondant a la grandeur : %(k2)s . il faut imprimer des champs aux noeuds a ce format.
"""),

89: _("""
 1  %(k1)s 
"""),

90: _("""
 on ne sait pas imprimer le champ  %(k1)s  au format  %(k2)s 
"""),

91: _("""
 debut de l'ecriture med de  %(k1)s 
"""),

92: _("""
 impossible de determiner un nom de champ med.
"""),

93: _("""
 pas d ecriture pour  %(k1)s 
"""),

94: _("""
 issu de  %(k1)s 
"""),

95: _("""
 type  %(k1)s  inconnu pour med.
"""),

96: _("""
 fin de l'ecriture med de  %(k1)s 
"""),

97: _("""
 on ne sait pas imprimer les champs de type " %(k1)s "   on est vraiment desole.
"""),

98: _("""
 le champ:  %(k1)s  a des elements ayant des sous-points. il est ecrit avec un format different.
"""),

99: _("""
 le champ:  %(k1)s  a des elements ayant des sous-points. ces elements ne seront pas ecrits.
"""),
}
