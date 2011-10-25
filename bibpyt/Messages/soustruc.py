#@ MODIF soustruc Messages  DATE 25/10/2011   AUTEUR COURTOIS M.COURTOIS 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg={

1: _(u"""
 les 2 maillages ne sont pas du meme type : 2D (ou 3D).
"""),

2: _(u"""
 la (super)maille :  %(k1)s  est en double.
"""),

3: _(u"""
 la maille :  %(k1)s  est en double.
"""),

4: _(u"""
 le GROUP_MA :  %(k1)s  est en double. on ignore le second.
"""),

5: _(u"""
 le GROUP_NO :  %(k1)s  est en double. on ignore le second.
"""),

6: _(u"""
 le noeud: %(k1)s  n'a pas les memes coordonnées dans les maillages:  %(k2)s  et  %(k3)s
"""),

7: _(u"""
 matrice de rigidité non inversible (modes rigides ???)
 - attention : critères de choc non calculés
"""),

8: _(u"""
 matrice de rigidité : pivot quasi-nul (modes rigides ???)
 - attention : critères de choc non calculés
"""),

9: _(u"""
 mot-clef "DEFINITION" interdit :
 le MACR_ELEM :  %(k1)s  est déja défini.
"""),

10: _(u"""
 mot-clef "RIGI_MECA" interdit :
 il est deja calculé.
"""),

11: _(u"""
 mot-clef "RIGI_MECA" interdit :
 le resultat :  %(k1)s  existe deja.
"""),

12: _(u"""
 mot-clef "MASS_MECA" interdit :
 il faut avoir fait "DEFINITION" et "RIGI_MECA".
"""),

13: _(u"""
 mot-clef "MASS_MECA" interdit :
 il est deja calculé.
"""),

14: _(u"""
 mot-clef "CAS_CHARGE" interdit :
 il faut avoir fait "DEFINITION" et "RIGI_MECA".
"""),

15: _(u"""
 cet opérateur modifie un maillage existant
 le resultat doit etre identique au concept donné dans l'argument MAILLAGE.
"""),

16: _(u"""
 maillages avec super mailles :
 utiliser OPERATION = SOUS-STRU
"""),

17: _(u"""
 groupe de noeuds inexistant
"""),

18: _(u"""
 groupes incompatibles
"""),

19: _(u"""
 il ne faut que des points
"""),

20: _(u"""
 vecteur nul pour l'orientation
"""),

21: _(u"""
 critère inconnu
"""),

22: _(u"""
 noeud %(k1)s trop éloigné de la normale au segment
 distance = %(r1)f
"""),

23: _(u"""
  On cherche à classer une liste de noeuds par abscisses croissantes
  de leur projection sur un segment de droite.
  2 noeuds ont la meme projection sur la droite.
  Le classement est donc arbitraire.

  -> Risque & Conseil :
     Vérifiez votre maillage.
"""),

24: _(u"""
 mot clef "sous_struc" interdit pour ce modele sans sous_structures.
"""),

25: _(u"""
 liste de mailles plus longue que la liste des sous_structures du modele.
"""),

26: _(u"""
 la maille :  %(k1)s  n existe pas dans le maillage :  %(k2)s
"""),

27: _(u"""
 la maille :  %(k1)s  n'est pas active dans le modele
"""),

28: _(u"""
 la maille :  %(k1)s  ne connait pas le chargement :  %(k2)s
"""),

29: _(u"""
 arret suite aux erreurs detectees.
"""),

30: _(u"""
 mot clef "affe_sous_struc" interdit pour ce maillage sans (super)mailles.
"""),

31: _(u"""
 la maille :  %(k1)s  n'appartient pas au maillage
"""),

32: _(u"""
 maille en double :  %(k1)s  dans le group_ma:  %(k2)s
"""),

33: _(u"""
 l'indice final est inferieur a l'indice initial
"""),

34: _(u"""
 l'indice final est superieur a la taille du groupe
"""),

35: _(u"""
 le group_ma :  %(k1)s  n'appartient pas au maillage
"""),

36: _(u"""
 le group_ma : %(k1)s est vide. on ne le cree pas.
"""),

37: _(u"""
 Le groupe de noeuds '%(k1)s' existe déjà.

 Conseil :
    Si vous souhaitez utiliser un nom de groupe existant, il suffit
    de le détruire avec DEFI_GROUP / DETR_GROUP_NO.
"""),


38: _(u"""
 le group_no : %(k1)s  est vide, on ne le cree pas.
"""),

39: _(u"""
 noeud en double :  %(k1)s  dans le group_no:  %(k2)s
"""),

40: _(u"""
 liste de charges trop longue
"""),

41: _(u"""
 l'exterieur du macr_elem_stat contient des noeuds qui ne portent aucun ddl, ces noeuds sont elimines.
"""),

42: _(u"""
 l'exterieur du macr_elem_stat contient des noeuds en double. ils sont elimines.
"""),

43: _(u"""
 grandeur:  %(k1)s  interdite. (seule autorisee: depl_r)
"""),

44: _(u"""
 la maille :  %(k1)s  n existe pas dans le maillage : %(k2)s
"""),

45: _(u"""
 num. equation incorrect
"""),

46: _(u"""
 cas de charge :  %(k1)s  inexistant sur le macr_elem_stat :  %(k2)s
"""),

47: _(u"""
 erreur programmeur 1
"""),

48: _(u"""
 noeud :  %(k1)s  inexistant dans le maillage :  %(k2)s
"""),

49: _(u"""
 group_no :  %(k1)s  inexistant dans le maillage :  %(k2)s
"""),

50: _(u"""
 liste de mailles trop longue.
"""),

51: _(u"""
 la liste des mailles est plus longue que la liste des macr_elem_stat.
"""),

52: _(u"""
 trop de reels pour le mot clef "tran"
"""),

53: _(u"""
 trop de reels pour le mot clef "angl_naut"
"""),

54: _(u"""
 trop de reels pour le mot clef "centre"
"""),

55: _(u"""
 melange de maillages 2d et 3d
"""),

56: _(u"""
  le macr_elem_stat : %(k1)s  n'existe pas.
"""),

57: _(u"""
 les arguments "prefixe" et "index" conduisent a des noms de noeuds trop longs (8 caracteres maxi).
"""),

58: _(u"""
 il faut : "tout" ou "maille" pour "defi_noeud".
"""),

59: _(u"""
  le noeud :  %(k1)s  n'appartient pas a la maille : %(k2)s
"""),

60: _(u"""
  le noeud :  %(k1)s  de la maille :  %(k2)s  a ete elimine (recollement). on ne peut donc le renommer.
"""),

61: _(u"""
 les arguments "prefixe" et "index" conduisent a des noms de group_no trop longs (8 caracteres maxi).
"""),

62: _(u"""
 le group_no :  %(k1)s  est vide. on ne le cree pas.
"""),

63: _(u"""
 liste trop longue
"""),

64: _(u"""
 la liste de mailles est plus longue que le nombre total de mailles.
"""),

65: _(u"""
 la liste de mailles  n'a pas la meme longueur que la liste de group_no.
"""),

66: _(u"""
 la liste de mailles  doit etre de dimension au moins 2 pour le recollement
"""),

67: _(u"""
 les group_no a recoller :  %(k1)s  et  %(k2)s  n'ont pas le meme nombre de noeuds.
"""),

68: _(u"""
 pour le recollement geometrique des group_no :  %(k1)s  et  %(k2)s  certains noeuds ne sont pas apparies
"""),

69: _(u"""
 amor_meca non implante.
"""),

70: _(u"""
 la sous-structuration n'est possible qu'en mecanique
"""),

71: _(u"""
 nombre de noeuds internes : 0
"""),

73: _(u"""
 la grandeur "depl_r" doit avoir lescomposantes (1 a 6) : dx,dy, ..., drz
"""),

74: _(u"""
 la grandeur "depl_r" doit avoir lacomposante "lagr".
"""),

75: _(u"""
 autorise : "lg" ou "gl" pas: %(k1)s
"""),

76: _(u"""
 ddl non prevu. on attend: dx
"""),

77: _(u"""
 ddls non prevus
"""),

78: _(u"""
 erreur dans le maillage
"""),

79: _(u"""
 le calcul de reponse pour ce type de resultat n'est disponible que sur les macr_elem_stat obtenus a partir de la mesure
"""),

80: _(u"""
 la matrice de rigidite condensee n'a pas ete calculee
"""),

81: _(u"""
 la matrice de masse condensee n'a pas ete calculee
"""),

82: _(u"""
 nb de ddl capteur insuffisant ou nb vecteurs de base trop eleve, nb ddl capteur : %(i1)d ,nb vecteurs de base : %(i2)d
"""),

83: _(u"""
 nb ddl interface insuffisant ou nb modes identifies trop eleve, nb ddl interface : %(i1)d ,nb modes identifies : %(i2)d
"""),

84: _(u"""
  champ inexistant  base  %(k1)s  nom_cham  %(k2)s  nume_ordre  %(i1)d
"""),

85: _(u"""
  au noeud de choc:  %(k1)s
"""),

86: _(u"""
 noeud   %(k1)s   en dehors du segment %(k2)s   abscisse curviligne %(r1)f
"""),

87: _(u"""
 trop de noeuds dans le group_no  noeud utilise:  %(k1)s
"""),

88: _(u"""
  Vérifiez votre maillage.
"""),

89: _(u"""
  coordonnee x < 0 pour le noeud  %(k1)s  maille  %(k2)s
"""),

93: _(u"""
    pour le mode no : %(i1)d taux de flexibilite locale   :  %(r1)f
 souplesse locale             :  %(r2)f
 taux effort tranchant local  :  %(r3)f

"""),

94: _(u"""
   -- bilan noeud de choc : %(k1)s  taux de restit flexibilite      :  %(r1)f
  taux de restit effort tranchant :  %(r2)f
"""),

95: _(u"""
  ( souplesse statique - souplesse locale )/ souplesse choc :  %(r1)f
"""),

96: _(u"""
  souplesse locale / souplesse choc :  %(r1)f
"""),

98: _(u"""
 !! attentionplus petite val sing def stat :  %(r1)f !! nous la forcons a :  %(r2)f
"""),

99: _(u"""
 ---- conditionnement def stat :  %(r1)f
"""),

}
