#@ MODIF utilitai Messages  DATE 17/07/2007   AUTEUR MACOCCO K.MACOCCO 
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
 le nombre de grels du ligrel du modele est nul.
"""),

2: _("""
 il ne faut pas demander 'tr' derriere cara si le type d'element discret ne prend pas en compte la rotation
"""),

3: _("""
 "*" est illicite dans une liste.
"""),

4: _("""
  %(k1)s  n'est pas une option reconnu.
"""),

5: _("""
 vecteur axe de norme nulle
"""),

6: _("""
 axe non colineaire a v1v2
"""),

7: _("""
 pb norme de axe
"""),

8: _("""
 erreur dans fointc
"""),

9: _("""
 dimension  %(k1)s  inconnue.
"""),

10: _("""
 maillage obligatoire.
"""),

11: _("""
 on ne peut pas creer un champ de vari_r avec le mot cle facteur affe (voir u2.01.09)
"""),

12: _("""
 mot cle affe/noeud interdit ici.
"""),

13: _("""
 mot cle affe/group_no interdit ici.
"""),

14: _("""
 type scalaire non traite :  %(k1)s
"""),

15: _("""
 incoherence entre nombre de composantes et nombre de valeurs
"""),

16: _("""
 il faut champ de fonctions svp
"""),

17: _("""
 les parametres doivent etre reels
"""),

18: _("""
 maillages diff.
"""),

19: _("""
 erreur pgmeur: augmenter nbpumx.
"""),

20: _("""
 le champ  %(k1)s n'est pas de type reel
"""),

21: _("""
 on ne traite que des "cham_no" ou des "cham_elem".
"""),

22: _("""
 unknown parameter (dvlp)
"""),

23: _("""
 on ne trouve aucun champ.
"""),

24: _("""
 le nom symbolique:  %(k1)s  est illicite pour ce resultat
"""),

25: _("""
 le champ cherche n'a pas encore ete calcule.
"""),

26: _("""
 pas la meme numerotation sur les cham_nos.
"""),

27: _("""
 il faut donner un maillage.
"""),

28: _("""
 champ non-assemblable en cham_no:  %(k1)s
"""),

29: _("""
 champ non-assemblable en cham_elem (elga):  %(k1)s
"""),

30: _("""
 Pour l'operation "ASSE", la commande ne permet pas de créer de champs CHAM_ELEM/ELEM
"""),

31: _("""
 nom_cmp2 et nom_cmp de longueur differentes.
"""),

32: _("""
 grandeur incorrecte pour: %(k1)s
"""),

33: _("""
 le mot-cle 'coef_c' n'est applicable que pour un champ de type complexe
"""),

34: _("""
 developpement non realise pour les champs aux elements. vraiment desole !
"""),

35: _("""
 le champ  %(k1)s n'est pas de type complexe
"""),

36: _("""
 on ne traite que des cham_no reels ou complexes. vraiment desole !
"""),

37: _("""
 acces "r8" interdit ici.
"""),

38: _("""
 acces interdit
"""),

39: _("""
 genre :  %(k1)s  non prevu.
"""),

40: _("""
 structure de donnees inexistante : %(k1)s
"""),

41: _("""
 duplcation "maillage" du .ltnt, objet inconnu:  %(k1)s
"""),

42: _("""
 type de sd. inconnu :  %(k1)s
"""),

43: _("""
 numerotation absente  probleme dans la matrice  %(k1)s
"""),

44: _("""
  erreur dans la  recuperation  du nombre de noeuds !
"""),

45: _("""
 type non connu.
"""),

46: _("""
 la fonction doit s appuyee sur un maillage pour lequel une abscisse curviligne a ete definie.
"""),

47: _("""
  le mot cle : %(k1)s n est pas autorise.
"""),





49: _("""
 la question : " %(k1)s " est inconnue
"""),

50: _("""
 cham_elem inexistant:  %(k1)s
"""),

51: _("""
 il n y a pas de nume_ddl pour ce cham_no
"""),

52: _("""
 type de charge inconnu
"""),

53: _("""
 suffixe inconu:  %(k1)s
"""),

54: _("""
 trop d objets
"""),

55: _("""
 champ inexistant: %(k1)s
"""),

56: _("""
 le champ : " %(k1)s " n est pas un champ
"""),

57: _("""
  la question n'a pas de reponse sur une grandeur de type matrice gd_1 x gd_2
"""),

58: _("""
 situation imprevue.
"""),

59: _("""
  la question n'a pas de sens sur une grandeur de type matrice gd_1 x gd_2
"""),

60: _("""
  la question n'a pas de sens sur une grandeur de type composee
"""),

61: _("""
 imprevu
"""),

62: _("""
 on ne sait pas associer de phenomene a ce ligrel :  %(k1)s
"""),

63: _("""
 phenomene inconnu :  %(k1)s
"""),

64: _("""
 probleme dismoi.
"""),

65: _("""
 le type de concept : " %(k1)s " est inconnu
"""),

66: _("""
 le phenomene :  %(k1)s  est inconnu.
"""),

67: _("""
 2
"""),

68: _("""
 type de resultat inconnu:  %(k1)s  pour l'objet :  %(k2)s
"""),

69: _("""
 le resulat compose ne contient aucun champ
"""),

70: _("""
 type_maille inconnu.
"""),

71: _("""
 mauvaise recuperation de nema
"""),

72: _("""
 on ne traite pas les noeuds tardifs
"""),

73: _("""
 grandeur inexistante
"""),

74: _("""
 composante de grandeur inexistante
"""),

75: _("""
 probleme avec la reponse  %(k1)s
"""),

76: _("""
 les conditions aux limites autres que des ddls bloques ne sont pas admises
"""),

77: _("""
 unite logique  %(k1)s , probleme lors du close
"""),

78: _("""
  erreur dans la  recuperation  du maillage!
"""),

79: _("""
  erreur dans la  recuperation  du nombre de mailles !
"""),

80: _("""
  gruopage :                                                            groupe_ma non present !
"""),

81: _("""
  erreur a l'appel de metis plus aucune unite logique libre !
"""),

82: _("""
 methode d'integration inexistante.
"""),

83: _("""
 interpolation par defaut "lineaire"
"""),

84: _("""
 interpolation  %(k1)s  non implantee
"""),

85: _("""
 recherche " %(k1)s " inconnue
"""),

86: _("""
 l'intitule " %(k1)s " n'est pas correct.
"""),

87: _("""
 le noeud " %(k1)s " n'est pas un noeud de choc.
"""),

88: _("""
 nom de sous-structure et d'intitule incompatible
"""),

89: _("""
 le noeud " %(k1)s " n'est pas un noeud de choc de l'intitule.
"""),

90: _("""
 le noeud " %(k1)s " n'est pas compatible avec le nom de la sous-structure.
"""),

91: _("""
 le parametre " %(k1)s " n'est pas un parametre de choc.
"""),

92: _("""
 le noeud " %(k1)s " n'existe pas.
"""),

93: _("""
 la composante " %(k1)s " du noeud " %(k2)s " n'existe pas.
"""),

94: _("""
 type de champ inconnu  %(k1)s
"""),

95: _("""
 "interp_nume" et ("inst" ou "list_inst") non compatibles
"""),

96: _("""
 "interp_nume" et ("freq" ou "list_freq") non compatibles
"""),

97: _("""
 erreur 0
"""),

98: _("""
 1bis
"""),

99: _("""
 erreur 1
"""),
}
