#@ MODIF soustruc Messages  DATE 30/06/2010   AUTEUR DELMAS J.DELMAS 
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
# RESPONSABLE DELMAS J.DELMAS

def _(x) : return x

cata_msg={

1: _("""
 les 2 maillages ne sont pas du meme type : 2D (ou 3D).
"""),

2: _("""
 la (super)maille :  %(k1)s  est en double.
"""),

3: _("""
 la maille :  %(k1)s  est en double.
"""),

4: _("""
 le GROUP_MA :  %(k1)s  est en double. on ignore le second.
"""),

5: _("""
 le GROUP_NO :  %(k1)s  est en double. on ignore le second.
"""),

6: _("""
 le noeud: %(k1)s  n'a pas les memes coordonnées dans les maillages:  %(k2)s  et  %(k3)s 
"""),

7: _("""
 matrice de rigidité non inversible (modes rigides ???) 
 - attention : critères de choc non calculés
"""),

8: _("""
 matrice de rigidité : pivot quasi-nul (modes rigides ???) 
 - attention : critères de choc non calculés
"""),

9: _("""
 mot-clef "DEFINITION" interdit :
 le MACR_ELEM :  %(k1)s  est déja défini.
"""),

10: _("""
 mot-clef "RIGI_MECA" interdit :
 il est deja calculé.
"""),

11: _("""
 mot-clef "RIGI_MECA" interdit :
 le resultat :  %(k1)s  existe deja.
"""),

12: _("""
 mot-clef "MASS_MECA" interdit :
 il faut avoir fait "DEFINITION" et "RIGI_MECA".
"""),

13: _("""
 mot-clef "MASS_MECA" interdit :
 il est deja calculé.
"""),

14: _("""
 mot-clef "CAS_CHARGE" interdit :
 il faut avoir fait "DEFINITION" et "RIGI_MECA".
"""),

15: _("""
 cet opérateur modifie un maillage existant
 le resultat doit etre identique au concept donné dans l'argument MAILLAGE.
"""),

16: _("""
 maillages avec super mailles :
 utiliser OPERATION = SOUS-STRU
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
 critère inconnu
"""),

22: _("""
 noeud %(k1)s trop éloigné de la normale au segment
 distance = %(r1)f
"""),

23: _("""
  On cherche à classer une liste de noeuds par abscisses croissantes
  de leur projection sur un segment de droite.
  2 noeuds ont la meme projection sur la droite.
  Le classement est donc arbitraire.

  -> Risque & Conseil :
     Vérifiez votre maillage.
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

79: _("""
 le calcul de reponse pour ce type de resultat n'est disponible que sur les macr_elem_stat obtenus a partir de la mesure
"""),

80: _("""
 la matrice de rigidite condensee n'a pas ete calculee
"""),

81: _("""
 la matrice de masse condensee n'a pas ete calculee
"""),

82: _("""
 nb de ddl capteur insuffisant ou nb vecteurs de base trop eleve, nb ddl capteur : %(i1)d ,nb vecteurs de base : %(i2)d
"""),

83: _("""
 nb ddl interface insuffisant ou nb modes identifies trop eleve, nb ddl interface : %(i1)d ,nb modes identifies : %(i2)d
"""),

84: _("""
  champ inexistant  base  %(k1)s  nom_cham  %(k2)s  nume_ordre  %(i1)d 
"""),

85: _("""
  au noeud de choc:  %(k1)s 
"""),

86: _("""
 noeud   %(k1)s   en dehors du segment %(k2)s   abscisse curviligne %(r1)f 
"""),

87: _("""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s 
"""),

88: _("""
  verifier votre maillage 
"""),

89: _("""
  coordonnee x < 0 pour le noeud  %(k1)s  maille  %(k2)s 
"""),

90: _("""
 
"""),

91: _("""
   
"""),

92: _("""
 
"""),

93: _("""
    pour le mode no : %(i1)d taux de flexibilite locale   :  %(r1)f 
 souplesse locale             :  %(r2)f 
 taux effort tranchant local  :  %(r3)f 
 
"""),

94: _("""
   -- bilan noeud de choc : %(k1)s  taux de restit flexibilite      :  %(r1)f 
  taux de restit effort tranchant :  %(r2)f 
"""),

95: _("""
  ( souplesse statique - souplesse locale )/ souplesse choc :  %(r1)f 
"""),

96: _("""
  souplesse locale / souplesse choc :  %(r1)f 
"""),

97: _("""
   
"""),

98: _("""
 !! attentionplus petite val sing def stat :  %(r1)f !! nous la forcons a :  %(r2)f 
"""),

99: _("""
 ---- conditionnement def stat :  %(r1)f 
"""),

}
