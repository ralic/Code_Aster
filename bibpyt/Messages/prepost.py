#@ MODIF prepost Messages  DATE 18/09/2007   AUTEUR DURAND C.DURAND 
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
 GROUP_NO :  %(k1)s  inconnu dans le maillage
"""),

3: _("""
 hors bornes définies dans CESCRE.
"""),

4: _("""
 méthode  %(k1)s  illicite
"""),

5: _("""
 la longueur du défaut n'est pas en accord avec les tables définies
 coté revetement et coté métal de base
"""),

8: _("""
 prolongement à gauche exclu
"""),

9: _("""
 prolongement à droite exclu
"""),

10: _("""
 phénomène non valide
"""),

11: _("""
 nous ne pouvons pas récupérer la valeur du module d'Young : E.
"""),

12: _("""
 nous ne pouvons pas récupérer la valeur du coefficient de Poisson : NU.
"""),

13: _("""
  le type d entité  %(k1)s  est inconnu.
"""),

21: _("""
 Intersection Droite / Cercle
 pas d'intersection trouvée
"""),

22: _("""
 DELTA négatif
 problème lors de la recherche d'intersection
"""),

23: _("""
 pb N2+N3
"""),

25: _("""
 problème en dehors des 2 points
"""),

26: _("""
 problème N2+N3
"""),

27: _("""
 volume négatif
"""),

28: _("""
 volume supérieur à 1.d6
"""),

29: _("""
 TBELZO dépassement
"""),

30: _("""
 TBNOZO dépassement
"""),

31: _("""
 structure de données RESULTAT inconnue  %(k1)s 
"""),

32: _("""
  l'impression de la SD_RESULTAT  %(k1)s  a déjà été effectuée
  avec une liste de numéros d'ordre dont le premier numéro etait
  le meme que celui de la liste actuelle.
  on arrete l'impression afin d'éviter l'écrasement des fichiers écrits.
"""),

33: _("""
 problème à l'ouverture du fichier résultat ENSIGHT  %(k1)s
 pour l'impression du CHAM_GD  %(k2)s 
"""),

34: _("""
 probleme à l'ouverture du fichier résultat ENSIGHT  %(k1)s
 pour l'impression du CONCEPT  %(k2)s 
"""),

35: _("""
 code  %(k1)s  inconnu
"""),

36: _("""
 le champ de:  %(k1)s  a des éléments ayant des sous-points.
 ces éléments ne seront pas traités.
"""),

37: _("""
 vraiment desolé
"""),

38: _("""
 le vecteur est "AXE_Z" nul.
"""),

46: _("""
 erreur dans la création du fichier de maillage au format GIBI.
 Celui-ci ne contient pas d'objet de type maillage.
 
 Risque & Conseil:
 Assurez vous que votre procédure gibi sauvegarde bien les objets du maillage (pile 32 dans le formalisme GIBI)
"""),

51: _("""
 l'option de calcul " %(k1)s " n'existe pas dans la structure de données %(k2)s 
"""),

52: _("""
 le champ " %(k1)s " pour l'option de calcul " %(k2)s ", n'a pas été notée
 dans la structure de données %(k3)s 
"""),

53: _("""
 la dimension du problème est invalide : il faut : 1d, 2d ou 3d.
"""),

54: _("""
 nombre de noeuds supérieur au maximum autorisé : 27.
"""),

55: _("""
 objet &&GILIRE.INDIRECT inexistant
 problème à la lecture des points 
"""),

56: _("""
 type de maille :  %(k1)s  inconnu de pre_gibi.
"""),

57: _("""
 nombre d'objets supérieur au maximum autorisé : 99999.
"""),

58: _("""
 bug !!!
"""),

59: _("""
 le maillage GIBI est peut etre erroné :
 il est écrit : "NIVEAU RREUR N_ERR"  avec N_ERR est >0 .
 on continue quand meme, mais si vous avez des problèmes plus loin ...
"""),

60: _("""
 arret sur erreur(s)
"""),

61: _("""
 erreur dans la récupération du NUME.PRNO
"""),

62: _("""
 problème dans la récupération du numéro de bloc auquel appartient la ligne courante.
"""),

63: _("""
 les seuls types de valeurs acceptés pour les RESU_ELEM sont les réels et les complexes
 le descripteur de type  %(k1)s  est inadéquat.
"""),

64: _("""
 les seuls types de structures de données autorisés sont :
 MATR_ASSE , MATR_ELEM , VECT_ELEM , RESU_ELEM
 le type donné  %(k1)s  n'est pas reconnu .
"""),

65: _("""
 la valeur du grain d'impression est  %(k1)s  alors que
 les seules valeurs possibles sont "NOEUD" ou "VALEUR" ou "MAILLE".
"""),

66: _("""
 la valeur du grain d'impression est  %(k1)s  alors que
 les seules valeurs possibles sont "NOEUD" et "VALEUR".
"""),

67: _("""
 probleme dans le descripteur de la matrice :
 l'indicateur de symétrie ne correspond ni à une matrice symétrique,
 ni à une matrice non-symétrique . 
"""),

68: _("""
 problème dans le descripteur de la matrice :
 l'indicateur type de valeur de la matrice ne correspond ni à une matrice réelle,
 ni à une matrice complexe . 
"""),

69: _("""
 problème à l'ouverture du fichier
"""),

70: _("""
 problème à la fermeture du fichier
"""),

71: _("""
 maillage introuvable
"""),

72: _("""
 MED : erreur MDNOMA numéro  %(k1)s 
"""),

73: _("""
 le champ est écrit dans le fichier
"""),

74: _("""
 la variable  %(k1)s  n'existe pas
"""),

75: _("""
 pas d'impression du champ
"""),

76: _("""
  -> Il y a des groupes de noeuds dans le maillage %(k1)s.
     Ils  n'apparaitront pas dans le fichier géométrie ENSIGHT.
     Seuls des groupes de mailles peuvent y etre intégrés.
"""),

77: _("""
  incompatibilité entre les GREL
"""),

78: _("""
 NEC trop grand
"""),

79: _("""
  nombre de couches > 1 
"""),

80: _("""
 on traite les TRIA7 QUAD9 en oubliant le noeud centre
"""),

82: _("""
 écriture impossible pour  %(k1)s  au format MED
"""),

83: _("""
 certaines composantes selectionnées ne font pas partie du LIGREL
"""),

84: _("""
 élément PYRA5 non disponible dans IDEAS
"""),

85: _("""
 élément PYRA13 non disponible dans IDEAS
"""),

86: _("""
 on traite les TRIA7 QUAD9 HEXA27 en oubliant le noeud centre
"""),

87: _("""
 on ne sait pas imprimer le champ de type:  %(k1)s 
 champ :  %(k2)s 
"""),

88: _("""
  on ne sait pas imprimer au format ENSIGHT le champ  %(k1)s
  correspondant à la grandeur : %(k2)s
  il faut imprimer des champs aux noeuds à ce format.
"""),

89: _("""
 1  %(k1)s 
"""),

90: _("""
 on ne sait pas imprimer le champ  %(k1)s  au format  %(k2)s 
"""),

91: _("""
 debut de l'écriture MED de  %(k1)s 
"""),

92: _("""
 impossible de déterminer un nom de champ MED.
"""),

93: _("""
 pas d'écriture pour  %(k1)s 
"""),

94: _("""
 issu de  %(k1)s 
"""),

95: _("""
 type  %(k1)s  inconnu pour med.
"""),

96: _("""
 fin de l'écriture MED de  %(k1)s 
"""),

97: _("""
 on ne sait pas imprimer les champs de type " %(k1)s "
"""),

98: _("""
 le champ:  %(k1)s  a des éléments ayant des sous-points.
 il est écrit avec un format différent.
"""),

99: _("""
 le champ:  %(k1)s  a des éléments ayant des sous-points.
 ces éléments ne seront pas écrits.
"""),
}
