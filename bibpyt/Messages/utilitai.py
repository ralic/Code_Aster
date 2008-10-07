#@ MODIF utilitai Messages  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
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

cata_msg = {

# a traduire en francais par JP
1 : _("""
 le nombre de grels du ligrel du modele est nul.
"""),

2 : _("""
 il ne faut pas demander 'TR' derrière cara si le type d'élément discret ne prend pas en compte la rotation
"""),

3 : _("""
 "*" est illicite dans une liste.
"""),

4 : _("""
  %(k1)s  n'est pas une option reconnue
"""),

5 : _("""
 vecteur axe de norme nulle
"""),

6 : _("""
 axe non colineaire à v1v2
"""),

7 : _("""
 pb norme de axe
"""),

9 : _("""
 dimension  %(k1)s  inconnue.
"""),

10 : _("""
 maillage obligatoire.
"""),

11 : _("""
 on ne peut pas créer un champ de VARI_R avec le mot cle facteur AFFE
 (voir u2.01.09)
"""),

12 : _("""
 mot clé AFFE/NOEUD interdit ici.
"""),

13 : _("""
 mot clé AFFE/GROUP_NO interdit ici.
"""),

14 : _("""
 type scalaire non traité :  %(k1)s
"""),

15 : _("""
 incohérence entre nombre de composantes et nombre de valeurs
"""),

16 : _("""
 il faut donner un champ de fonctions
"""),

17 : _("""
 les parametres doivent être réels
"""),

18 : _("""
 maillages différents
"""),

20 : _("""
 le champ  %(k1)s n'est pas de type réel
"""),

21 : _("""
 on ne traite que des "CHAM_NO" ou des "CHAM_ELEM".
"""),

22: _("""
 la programmation prévoit que les entiers sont codés sur plus de 32 bits
 ce qui n'est pas le cas sur votre machine
"""),

23 : _("""
 on ne trouve aucun champ.
"""),

24 : _("""
 le nom symbolique:  %(k1)s  est illicite pour ce résultat
"""),

25 : _("""
 le champ cherché n'a pas encore été calculé.
"""),

26 : _("""
 pas la meme numerotation sur les CHAM_NO.
"""),

27 : _("""
 il faut donner un maillage.
"""),

28 : _("""
 champ non-assemblable en CHAM_NO:  %(k1)s
"""),

29 : _("""
 champ non-assemblable en CHAM_ELEM (ELGA):  %(k1)s
"""),

31 : _("""
 nom_cmp2 et nom_cmp de longueur differentes.
"""),

32: _("""
 Grandeur incorrecte pour le champ : %(k1)s
 grandeur proposée :  %(k2)s
 grandeur attendue :  %(k3)s
"""),

33 : _("""
 le mot-cle 'coef_c' n'est applicable que pour un champ de type complexe
"""),

34 : _("""
 developpement non realise pour les champs aux elements. vraiment desole !
"""),

35 : _("""
 le champ  %(k1)s n'est pas de type complexe
"""),

36 : _("""
 on ne traite que des cham_no reels ou complexes. vraiment desole !
"""),

37 : _("""
 acces "r8" interdit ici.
"""),

38 : _("""
 acces interdit
"""),

39 : _("""
 genre :  %(k1)s  non prevu.
"""),

40 : _("""
 structure de donnees inexistante : %(k1)s
"""),

41 : _("""
 duplcation "maillage" du .ltnt, objet inconnu:  %(k1)s
"""),

42 : _("""
 type de sd. inconnu :  %(k1)s
"""),

43 : _("""
 numerotation absente  probleme dans la matrice  %(k1)s
"""),

44 : _("""
  erreur dans la  recuperation  du nombre de noeuds !
"""),

45 : _("""
 type non connu.
"""),

46 : _("""
 la fonction doit s appuyee sur un maillage pour lequel une abscisse curviligne a ete definie.
"""),

47 : _("""
  le mot cle : %(k1)s n est pas autorise.
"""),

49 : _("""
  DISMOI :
  la question : " %(k1)s " est inconnue
"""),

50 : _("""
 CHAM_ELEM inexistant:  %(k1)s
"""),

51 : _("""
 il n y a pas de NUME_DDL pour ce CHAM_NO
"""),

52 : _("""
 type de charge inconnu
"""),

53 : _("""
 suffixe inconu:  %(k1)s
"""),

54 : _("""
 trop d objets
"""),

55 : _("""
 champ inexistant: %(k1)s
"""),

56 : _("""
 le champ : " %(k1)s " n est pas un champ
"""),

57 : _("""
  DISMOI :
  la question n'a pas de réponse sur une grandeur de type matrice gd_1 x gd_2
"""),

59 : _("""
  DISMOI :
  la question n'a pas de sens sur une grandeur de type matrice gd_1 x gd_2
"""),

60 : _("""
  DISMOI :
  la question n'a pas de sens sur une grandeur de type composée
"""),

63 : _("""
 phenomene inconnu :  %(k1)s
"""),

65 : _("""
 le type de concept : " %(k1)s " est inconnu
"""),

66 : _("""
 le phénomène :  %(k1)s  est inconnu.
"""),

68 : _("""
 type de resultat inconnu:  %(k1)s  pour l'objet :  %(k2)s
"""),

69 : _("""
 le resultat composé ne contient aucun champ
"""),

70 : _("""
 TYPE_MAILLE inconnu.
"""),

71 : _("""
 mauvaise recuperation de NEMA
"""),

72 : _("""
 on ne traite pas les noeuds tardifs
"""),

73 : _("""
 grandeur inexistante
"""),

74 : _("""
 composante de grandeur inexistante
"""),

75 : _("""
 probleme avec la reponse  %(k1)s
"""),

76 : _("""
 les conditions aux limites autres que des ddls bloques ne sont pas admises
"""),

77 : _("""
 unite logique  %(k1)s , probleme lors du close
"""),

78 : _("""
  erreur dans la  recuperation  du maillage
"""),

79 : _("""
  erreur dans la récuperation du nombre de mailles
"""),

80 : _("""
  groupe_ma non présent
"""),

81 : _("""
  erreur à l'appel de METIS
  plus aucune unité logique libre
"""),

82 : _("""
 methode d'integration inexistante.
"""),

83 : _("""
 interpolation par defaut "lineaire"
"""),

84 : _("""
 interpolation  %(k1)s  non implantee
"""),

85 : _("""
 recherche " %(k1)s " inconnue
"""),

86 : _("""
 l'intitule " %(k1)s " n'est pas correct.
"""),

87 : _("""
 le noeud " %(k1)s " n'est pas un noeud de choc.
"""),

88 : _("""
 nom de sous-structure et d'intitule incompatible
"""),

89 : _("""
 le noeud " %(k1)s " n'est pas un noeud de choc de l'intitule.
"""),

90 : _("""
 le noeud " %(k1)s " n'est pas compatible avec le nom de la sous-structure.
"""),

91 : _("""
 le parametre " %(k1)s " n'est pas un parametre de choc.
"""),

92 : _("""
 le noeud " %(k1)s " n'existe pas.
"""),

93 : _("""
 la composante " %(k1)s " du noeud " %(k2)s " n'existe pas.
"""),

94 : _("""
 type de champ inconnu  %(k1)s
"""),

95 : _("""
 "INTERP_NUME" et ("INST" ou "LIST_INST") non compatibles
"""),

96 : _("""
 "INTERP_NUME" et ("FREQ" ou "LIST_FREQ") non compatibles
"""),

99 : _("""
 objet %(k1)s inexistant
"""),

}
