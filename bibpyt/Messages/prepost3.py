#@ MODIF prepost3 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 med: erreur efgaul numero  %(k1)s 
"""),

2: _("""
 med: erreur efgaue numero  %(k1)s 
"""),

3: _("""
 ecriture des localisations des points de gauss
"""),

4: _("""
  le nombre de noeuds selectionnes est superieur au nombre de noeuds du maillage. on va tronquer la liste.
"""),

5: _("""
 chaine de caracteres trop longues : imprimer moins de champs
"""),

6: _("""
 type inconnu" %(k1)s "
"""),

7: _("""
 le maillage  %(k1)s  a deja ete ecrit au format ensight: le contenu du fichier  %(k2)s  sera ecrase.
"""),

8: _("""
 probleme a l'ouverture du fichier " %(k1)s " pour impression du maillage  %(k2)s  au format ensight
"""),

9: _("""
 type de base inconnu:  %(k1)s 
"""),

10: _("""
 soit le fichier n'existe pas, soit c'est une mauvaise version de hdf (utilise par med).
"""),

11: _("""
 pas de maillage dans  %(k1)s 
"""),

12: _("""
 maillage  %(k1)s  inconnu dans  %(k2)s 
"""),

13: _("""
 instant inconnu pour ce champ et ces supports dans le fichier.
"""),

14: _("""
 champ inconnu.
"""),

15: _("""
 il manque des composantes.
"""),

16: _("""
 aucune valeur a cet instant.
"""),

17: _("""
 aucune valeur a ce nro d ordre.
"""),

18: _("""
 mauvais nombre de valeurs.
"""),

19: _("""
 lecture impossible.
"""),

20: _("""
 absence de numerotation des mailles  %(k1)s  dans le fichier med
"""),

21: _("""
 grandeur inconnue
"""),

22: _("""
 composante inconnue pour la grandeur
"""),

23: _("""
 trop de composantes pour la grandeur
"""),

24: _("""
 lecture impossible pour  %(k1)s  au format med
"""),

25: _("""
 modele obligatoire pour lire un cham_elem
"""),

26: _("""
 med: erreur efchrl numero  %(k1)s 
"""),

27: _("""
 nom de composante tronquee a 8 caracteres ( %(k1)s  >>>  %(k2)s )
"""),

28: _("""
 impossible de trouver la composante aster associee a  %(k1)s 
"""),

29: _("""
 pour lire_resu / format=ideas le maillage doit normalement avoir ete lu au format ideas.
"""),

30: _("""
 on a trouve plusieurs champs pour le meme dataset
"""),

31: _("""
 on n'a pas trouve le numero d'ordre a l'adresse indiquee
"""),

32: _("""
 on n'a pas trouve l'instant a l'adresse indiquee
"""),

33: _("""
 on n'a pas trouve la frequence a l'adresse indiquee
"""),

34: _("""
 on n'a pas trouve dans le fichier unv le type de champ
"""),

35: _("""
 on n'pas trouve dans le fichier unv le nombre de composantes a lire
"""),

36: _("""
 on n'pas trouve dans le fichier unv la nature du champ (reel ou complexe)
"""),

37: _("""
 le type de champ demande est different du type de champ a lire
"""),

38: _("""
 le champ demande n'est pas dememe nature que le champ a lire (reel/complexe)
"""),

39: _("""
 le mot cle modele est obligatoire pour un cham_elem
"""),

40: _("""
 pb correspondance noeud ideas
"""),

41: _("""
 le champ de type elga n'est pas  supporte
"""),

42: _("""
 probleme dans la lecture du nombre de champs
"""),

43: _("""
 probleme dans la lecture du nombre de composantes
"""),

44: _("""
 probleme dans la lecture du nom du champ et des ses composantes
"""),

45: _("""
 le champ  %(k1)s n existe pas dans le fichier med
"""),

46: _("""
 med: erreur efnpdt numero  %(k1)s 
"""),

47: _("""
 med: erreur efpdti numero  %(k1)s 
"""),

48: _("""
 med: on ne traite pas les maillages distants
"""),

49: _("""
 probleme a la fermeture
"""),

50: _("""
 probleme dans le diagnostic.
"""),

51: _("""
 med: erreur efnval numero  %(k1)s 
"""),

52: _("""
 probleme dans la lecture du nombre de maillages
"""),

53: _("""
 probleme dans la lecture du nom du maillage.
"""),

54: _("""
 attention le maillage n'est pas de type non structure
"""),

55: _("""
 le maillage ' %(k1)s ' est inconnu dans le fichier.
"""),

56: _("""
 attention il s'agit d'un maillage structure
"""),

57: _("""
 ==> transfert impossible.
"""),

58: _("""
 mauvaise definition de noresu.
"""),

59: _("""
 mauvaise definition de nomsym.
"""),

60: _("""
 mauvaise definition de nopase.
"""),

61: _("""
 mauvais dimensionnement de nomast.
"""),

62: _("""
 impossible de determiner un nom de maillage med
"""),

63: _("""
 on attend 10 ou 12 secteurs
"""),

64: _("""
 ******* percement tube *******
"""),

65: _("""
 pour la variable d'acces "noeud_cmp", il faut un nombre pair de valeurs.
"""),

66: _("""
 le modele et le maillage introduits ne sont pas coherents
"""),

67: _("""
 il faut donner le maillage pour une impression au format "castem".
"""),

68: _("""
 vous voulez imprimer           sur un meme fichier le maillage et un champ ce qui est            incompatible avec le format gmsh
"""),

69: _("""
 le mot cle  'partie' est obligatoire.
"""),

70: _("""
 le mot cle "info_maillage" est reserve au format med
"""),

71: _("""
 le mot cle "info_resu" est reserve au format resultat
"""),

72: _("""
 l'impression avec selection sur des entites topologiques n'a pas de sens au format ensight : les valeurs de tous les noeuds du maillage seront donc imprimees.
"""),

73: _("""
 l'impression avec selection sur des entites topologiques n'a pas de sens au format castem  : toutes les valeurs sur tout le maillage seront donc imprimees.
"""),

74: _("""
 le noeud mentionne est invalide
"""),

75: _("""
 fichier gibi cree par sort format non supporte dans cette version
"""),

76: _("""
 version de gibi non supporte, la lecture peut echouer !
"""),

77: _("""
 fichier gibi errone
"""),

78: _("""
 le fichier maillage gibi est vide
"""),

79: _("""
 cette commande ne fait que completer un resultat_compose deja existant. il faut donc que le resultat de la commande :  %(k1)s  soit identique a l'argument "resultat" :  %(k2)s 
"""),

80: _("""
 pour un resultat de type " %(k1)s ", on ne traite que l'option ..._noeu_...
"""),

81: _("""
 lmat =0
"""),

82: _("""
 option inconnue :  %(k1)s 
"""),

83: _("""
 noeud  %(k1)s ne fait pas partie du maillage :  %(k2)s 
"""),

84: _("""
 il faut autant de cmp en i et j
"""),

85: _("""
 il faut autant de cmp que de noeud
"""),

86: _("""
 il faut autant d indices en i et j
"""),

87: _("""
 y a un bug 5
"""),

88: _("""
 y a un bug 6
"""),

89: _("""
 y a un bug 7
"""),

90: _("""
 y a un bug 8
"""),

91: _("""
 y a un bug 9
"""),

92: _("""
 mot cle "test_nook" non valide avec le mot cle facteur "inte_spec".
"""),

93: _("""
 la fonction n'existe pas.
"""),

94: _("""
 il faut definir deux parametres pour une nappe.
"""),

95: _("""
 pour le parametre donne on n'a pas trouve la fonction.
"""),

96: _("""
 la methode 'wohler' ne peut pas etre utilisee avec l'option %(k1)s 
"""),

97: _("""
 une courbe de wohler doit etre definie dans defi_materiau
"""),

98: _("""
 la methode  'manson_coffin' ne peut pas etre utilisee avec l'option %(k1)s 
"""),

99: _("""
 une courbe de manson_coffin doit etre definie dans defi_materiau
"""),
}
