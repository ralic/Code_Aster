#@ MODIF soustruc Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 les 2 maillages ne sont pas du meme type : 2d (ou 3d).
"""),

2: _("""
 la (super)maille :  %(k1)s  est en double.
"""),

3: _("""
 la maille :  %(k1)s  est en double.
"""),

4: _("""
 le group_ma :  %(k1)s  est en double. on ignore le second.
"""),

5: _("""
 le group_no :  %(k1)s  est en double. on ignore le second.
"""),

6: _("""
 le noeud: %(k1)s  n'a pas les memes coordonnees dans les maillages:  %(k2)s  et  %(k3)s 
"""),

7: _("""
 matrice de rigidite non inversible (modes rigides ???) - attention : criteres de choc non calcules
"""),

8: _("""
 matrice de rigidite : pivot quasi-nul (modes rigides ???) - attention : criteres de choc non calcules
"""),

9: _("""
 mot-clef "definition" interdit :le macr_elem:  %(k1)s  est deja defini.
"""),

10: _("""
 mot-clef "rigi_meca" interdit :il est deja calcule.
"""),

11: _("""
 mot-clef "rigi_meca" interdit :le resultat :  %(k1)s  existe deja.
"""),

12: _("""
 mot-clef "mass_meca" interdit :il faut avoir fait "definition" et "rigi_meca".
"""),

13: _("""
 mot-clef "mass_meca" interdit :il est deja calcule.
"""),

14: _("""
 mot-clef "cas_charge" interdit :il faut avoir fait "definition" et "rigi_meca".
"""),

15: _("""
 cet operateur modifie un maillage existant. le resultat doit etre identique au concept donne dans l'argument maillage.
"""),

16: _("""
 maillages avec super mailles : utiliser operation : sous-stru
"""),

17: _("""
 groupe de noeuds inexistant
"""),

18: _("""
 groupes incompatibles
"""),

19: _("""
 il ne faut que des points
"""),

20: _("""
 vecteur nul pour l'orientation
"""),

21: _("""
 critere inconnu
"""),

22: _("""
 noeud eloignes a la normale au segment
"""),

23: _("""
 noeuds confondus
"""),

24: _("""
 mot clef "sous_struc" interdit pour ce modele sans sous_structures.
"""),

25: _("""
 liste de mailles plus longue que la liste des sous_structures du modele.
"""),

26: _("""
 la maille :  %(k1)s  n existe pas dans le maillage :  %(k2)s 
"""),

27: _("""
 la maille :  %(k1)s  n'est pas active dans le modele
"""),

28: _("""
 la maille :  %(k1)s  ne connait pas le chargement :  %(k2)s 
"""),

29: _("""
 arret suite aux erreurs detectees.
"""),

30: _("""
 mot clef "affe_sous_struc" interdit pour ce maillage sans (super)mailles.
"""),

31: _("""
 la maille :  %(k1)s  n'appartient pas au maillage
"""),

32: _("""
 maille en double :  %(k1)s  dans le group_ma:  %(k2)s 
"""),

33: _("""
 l'indice final est inferieur a l'indice initial 
"""),

34: _("""
 l'indice final est superieur a la taille du groupe 
"""),

35: _("""
 le group_ma :  %(k1)s  n'appartient pas au maillage
"""),

36: _("""
 le group_ma : %(k1)s est vide. on ne le cree pas.
"""),

37: _("""
 le group_no : %(k1)s  existe deja, on ne le modifie pas.
"""),

38: _("""
 le group_no : %(k1)s  est vide, on ne le cree pas.
"""),

39: _("""
 noeud en double :  %(k1)s  dans le group_no:  %(k2)s 
"""),

40: _("""
 liste de charges trop longue
"""),

41: _("""
 l'exterieur du macr_elem_stat contient des noeuds qui ne portent aucun ddl, ces noeuds sont elimines.
"""),

42: _("""
 l'exterieur du macr_elem_stat contient des noeuds en double. ils sont elimines.
"""),

43: _("""
 grandeur:  %(k1)s  interdite. (seule autorisee: depl_r)
"""),

44: _("""
 la maille :  %(k1)s  n existe pas dans le maillage : //mag
"""),

45: _("""
 num. equation incorrect
"""),

46: _("""
 cas de charge :  %(k1)s  inexistant sur le macr_elem_stat :  %(k2)s 
"""),

47: _("""
 erreur programmeur 1
"""),

48: _("""
 noeud :  %(k1)s  inexistant dans le maillage :  %(k2)s 
"""),

49: _("""
 group_no :  %(k1)s  inexistant dans le maillage :  %(k2)s 
"""),

50: _("""
 liste de mailles trop longue.
"""),

51: _("""
 la liste des mailles est plus longue que la liste des macr_elem_stat.
"""),

52: _("""
 trop de reels pour le mot clef "tran" 
"""),

53: _("""
 trop de reels pour le mot clef "angl_naut" 
"""),

54: _("""
 trop de reels pour le mot clef "centre" 
"""),

55: _("""
 melange de maillages 2d et 3d
"""),

56: _("""
  le macr_elem_stat : %(k1)s  n'existe pas.
"""),

57: _("""
 les arguments "prefixe" et "index" conduisent a des noms de noeuds trop longs (8 caracteres maxi).
"""),

58: _("""
 il faut : "tout" ou "maille" pour "defi_noeud".
"""),

59: _("""
  le noeud :  %(k1)s  n'appartient pas a la maille : %(k2)s 
"""),

60: _("""
  le noeud :  %(k1)s  de la maille :  %(k2)s  a ete elimine (recollement). on ne peut donc le renommer.
"""),

61: _("""
 les arguments "prefixe" et "index" conduisent a des noms de group_no trop longs (8 caracteres maxi).
"""),

62: _("""
 le group_no :  %(k1)s  est vide. on ne le cree pas.
"""),

63: _("""
 liste trop longue
"""),

64: _("""
 la liste de mailles est plus longue que le nombre total de mailles.
"""),

65: _("""
 la liste de mailles  n'a pas la meme longueur que la liste de group_no.
"""),

66: _("""
 la liste de mailles  doit etre de dimension au moins 2 pour le recollement
"""),

67: _("""
 les group_no a recoller :  %(k1)s  et  %(k2)s  n'ont pas le meme nombre de noeuds.
"""),

68: _("""
 pour le recollement geometrique des group_no :  %(k1)s  et  %(k2)s  certains noeuds ne sont pas apparies
"""),

69: _("""
 amor_meca non implante.
"""),

70: _("""
 la sous-structuration n'est possible qu'en mecanique
"""),

71: _("""
 nombre de noeuds internes : 0
"""),

72: _("""
 erreur programmeur 2
"""),

73: _("""
 la grandeur "depl_r" doit avoir lescomposantes (1 a 6) : dx,dy, ..., drz
"""),

74: _("""
 la grandeur "depl_r" doit avoir lacomposante "lagr".
"""),

75: _("""
 autorise : "lg" ou "gl" pas: %(k1)s 
"""),

76: _("""
 ddl non prevu. on attend: dx 
"""),

77: _("""
 ddls non prevus
"""),

78: _("""
 erreur dans le maillage
"""),
}
