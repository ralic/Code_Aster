#@ MODIF utilitai5 Messages  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
 nombre en dehors de (-1,1)
"""),

2: _("""
 asin/acos svp
"""),

3: _("""
 selection de ddl : choix < %(k1)s > inconnu
"""),

4: _("""
 argument d'appel invalide :  typf =  %(k1)s 
"""),

5: _("""
 argument d'appel invalide :  acces =  %(k1)s 
"""),

6: _("""
 argument d'appel invalide :  autor =  %(k1)s 
"""),

7: _("""
 redefinition de l'unite logique  %(k1)s  non autorisee
"""),

8: _("""
 nombre maximum d'unites logiques ouvertes atteint  %(k1)s 
"""),

9: _("""
 argument d'appel invalide :  unit =  %(k1)s 
"""),

10: _("""
 aucun numero d'unite logiquedisponible
"""),

11: _("""
 unite logique  %(k1)s  associee au nom  %(k2)s  et au fichier  %(k3)s 
"""),

12: _("""
 vous devez d'abord le fermer pour l'associer au nom  %(k1)s 
"""),

13: _("""
 unite logique  %(k1)s  deja utilisee en acces  %(k2)s  par le fichier  %(k3)s 
"""),

14: _("""
 vous devez d'abord le fermer
"""),

15: _("""
 unite logique  %(k1)s  deja utilisee en mode binaire par le fichier  %(k2)s 
"""),

16: _("""
 vous devez d'abord fermer le fichier associe
"""),

17: _("""
 unite logique  %(k1)s  deja utilisee par le fichier  %(k2)s  associee au nom  %(k3)s 
"""),

18: _("""
 unite logique  %(k1)s , probleme lors de l'open  %(k2)s 
"""),

19: _("""
 unite logique  %(k1)s , probleme lors du positionnement
"""),

20: _("""
 unite logique  %(k1)s , probleme lors de l'inquire
"""),

21: _("""
 nombre d'unites logiques ouvertes superieur a //k4b
"""),

22: _("""
 unite logique  %(k1)s , probleme lors du close de la reservation.
"""),

23: _("""
 la redefinition de l'unite logique  %(k1)s  n'est pas autorisee
"""),

24: _("""
 type d'acces inconnu " %(k1)s ", unite  %(k2)s 
"""),

25: _("""
 fichier non nomme, unite  %(k1)s 
"""),

26: _("""
 fichier non ouvert, unite  %(k1)s 
"""),

27: _("""
 rewind impossible, unite  %(k1)s 
"""),

28: _("""
 positionnement inconnu " %(k1)s ", unite  %(k2)s 
"""),

29: _("""
 les champs de type " %(k1)s " sont interdits.(a faire ...)
"""),

30: _("""
 composante  %(k1)s inexistante pour la grandeur  %(k2)s 
"""),

31: _("""
 la maille: %(k1)s n'appartient pas au maillage: %(k2)s 
"""),

32: _("""
 le champ: %(k1)s n'est pas un champ par elements aux noeuds.
"""),

33: _("""
 le noeud: %(k1)s n'appartient pas au maillage: %(k2)s 
"""),

34: _("""
 la maille: %(k1)s n'est pas affectee dans le ligrel: %(k2)s 
"""),

35: _("""
 la maille:  %(k1)s  possede un type d'element ignorant le cham_elem teste.
"""),

36: _("""
 num. de sous-point > max
"""),

37: _("""
 num. de point > max
"""),

38: _("""
 l'element n'admet pas la composante  %(k1)s 
"""),

39: _("""
 determination de la localisation des points de gauss
"""),

40: _("""
 type de donnees inconnu :  %(k1)s 
"""),

41: _("""
 xous :  %(k1)s  non prevu.
"""),

42: _("""
 chaine sch1 trop longue >24
"""),

43: _("""
 ipos hors de l intervalle (0 24)
"""),

44: _("""
 longueur totale > 24 
"""),

45: _("""
 on demande un nombre de composantes negatif pour  %(k1)s 
"""),

46: _("""
 on demande des composantes inconnues pour  %(k1)s 
"""),

47: _("""
 mot-clef :  %(k1)s  inconnu.
"""),

48: _("""
 composante inexistante dans le champ:  %(k1)s 
"""),

49: _("""
 type de champ non traite:  %(k1)s 
"""),

50: _("""
 type inconnu:  %(k1)s 
"""),

51: _("""
 il y a probablement une erreur dans la programmation
"""),

52: _("""
 mauvaise valeur pour fonree
"""),

53: _("""
 pas de composantes
"""),

54: _("""
 l"argument "indi" est non valide
"""),

55: _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur "debut" pour l"argument para
"""),

56: _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur "fin" pour l"argument para
"""),

57: _("""
 l"appel a uttcpu ne peut etre effectue avec la valeur  %(k1)s  pour l"argument para
"""),

58: _("""
 (uttrif) type de fonction non connu.
"""),

59: _("""
 il existe au moins un noeud qui n appartient pas au groupe de mailles.
"""),

60: _("""
 un sous-domaine  est non-connexe
"""),

61: _("""
 composante 1 de                         l objet mael_mass_desc non valide
"""),

62: _("""
 composante 2 de                         l objet mael_mass_desc non valide
"""),

63: _("""
 composante 3 de                         l objet mael_mass_desc non valide
"""),

64: _("""
 composante 1 de                         l objet mael_desc non valide
"""),

65: _("""
 composante 2 de                         l objet mael_desc non valide
"""),

66: _("""
 composante 3 de                         l objet mael_desc non valide
"""),

67: _("""
 composante 1 de                         l objet mael_raid_desc non valide
"""),

68: _("""
 composante 2 de                         l objet mael_raid_desc non valide
"""),

69: _("""
 composante 3 de                         l objet mael_raid_desc non valide
"""),

70: _("""
  erreur modg.desc comp 1 
"""),

71: _("""
  erreur modg.desc comp 2 
"""),

72: _("""
  erreur modg.desc comp 3 
"""),

73: _("""
 composante 5 de                         l objet modg.lidf non valide
"""),

74: _("""
 matrice de liaison 1 de                 l objet modg.lima non valide
"""),

75: _("""
 matrice de liaison 2 de                 l objet modg.lima non valide
"""),

76: _("""
 matrice de liaison 3 de                 l objet modg.lima non valide
"""),

77: _("""
 objet inexistant:  %(k1)s 
"""),

78: _("""
 objet existant:  %(k1)s 
"""),

79: _("""
 objet:  %(k1)s  de lonmax incorrect.
"""),

80: _("""
 collection:  %(k1)s  de lon_coll incorrect.
"""),

81: _("""
 collection:  %(k1)s  de lon_nom incorrect.
"""),

82: _("""
 objet:  %(k1)s  de type entier incorrect.
"""),

83: _("""
 objet:  %(k1)s  de type reel incorrect.
"""),

84: _("""
 objet:  %(k1)s  de type complexe incorrect.
"""),

85: _("""
 objet:  %(k1)s  de type logique incorrect.
"""),

86: _("""
 objet:  %(k1)s  de type chaine incorrect.
"""),

87: _("""
  le mot cle : %(k1)s  n est pas encore prevu.
"""),

88: _("""
 option " %(k1)s " a recalculer
"""),

89: _("""
 contacter l'assistance
"""),
}
