#@ MODIF modelisa6 Messages  DATE 12/10/2011   AUTEUR COURTOIS M.COURTOIS 
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

cata_msg = {

1 : _(u"""
 Problème d'orientation: aucune maille ne touche le noeud indiqué
"""),

2 : _(u"""
 Certaines mailles n'ont pas pu être réorientées. L'ensemble des mailles n'est pas connexe.
"""),

3 : _(u"""
 on ne trouve pas de noeud assez près du noeud  %(k1)s
"""),

4 : _(u"""
  Erreurs dans les données
"""),

5 : _(u"""
 Extraction de plus de noeuds que n'en contient la maille
"""),

6 : _(u"""
  Nombre de noeuds négatif
"""),

7 : _(u"""
 nombre de noeuds sommets non prévu
"""),

8 : _(u"""
  on est sur 2 mailles orthogonales
"""),

9 : _(u"""
 le type de maille est inconnu
"""),

10 : _(u"""
 la maille  %(k1)s  ne fait pas partie du maillage  %(k2)s
"""),

11 : _(u"""
 PREF_MAILLE est trop long, PREF_NUME est trop grand
"""),

12 : _(u"""
 PREF_MAILLE est trop long
"""),

13 : _(u"""
 les %(i1)d noeuds imprimés ci-dessus n'appartiennent pas au modèle (c'est à dire qu'ils ne portent aucun degré de liberté) et pourtant ils ont ete affectés dans le mot-clé facteur : %(k1)s
"""),


17 : _(u"""
 la maille  %(k1)s  du group_ma  %(k2)s donne apres le mot cle  %(k3)s n'a pas un type géométrique autorisé
"""),

18 : _(u"""
 la maille  %(k1)s donné après le mot clé  %(k2)s n'a pas un type géométrique autorisé
"""),

19 : _(u"""
  mot cle non admis : %(k1)s  les mots-clés admissibles sont :  %(k2)s  ou  %(k3)s  ou  %(k4)s  ou  %(k5)s  ou  %(k6)s  ou  %(k7)s  ou  %(k8)s ou  %(k9)s
"""),

20 : _(u"""
 ce type de maille n'est pas encore traité :  %(k1)s
"""),

21 : _(u"""
 le nombre total de noeuds est /= de la somme des noeuds sommets, arêtes et intérieurs
"""),

22 : _(u"""
 les 2 listes %(k1)s  et  %(k2)s  ne sont pas de meme longueur
"""),

26 : _(u"""
 AFFE_FIBRE pour " %(k1)s ": il y a  %(k2)s  valeurs pour "VALE", ce devrait être un multiple de 3
"""),

27 : _(u"""
 dans le maillage " %(k1)s " la maille " %(k2)s " est de type " %(k3)s " (ni TRIA3 ni QUAD4)
"""),

28 : _(u"""
 Le noeud <%(k1)s> de la poutre, de coordonnées <%(r1)g  %(r2)g  %(r3)g>,
ne doit pas appartenir à des mailles constituant la trace de la poutre sur la coque.
Le problème vient de l'occurence %(i1)d de LIAISON_ELEM.

Solution : Il faut dédoubler le noeud.
"""),

30 : _(u"""
 l'indicateur :  %(k1)s de position des multiplicateurs de lagrange associés à une relation lineaire n'est pas correct.
"""),

31 : _(u"""
  ->  En thermique, les fonctions définissant le matériau (enthalpie, capacité calorifique, conductivité)
      doivent obligatoirement être décrites par des fonctions tabulées et non des formules.
      En effet, on a besoin d'évaluer la dérivée de ces fonctions. Elle peut être plus facilement et
      précisément obtenue pour une fonction linéaire par morceaux que pour une expression 'formule'.
  -> Conseil:
      Tabulez votre formule, à une finesse de discrétisation d'abcisse (TEMP) à votre convenance,
      par la commande CALC_FONC_INTERP
 """),

32 : _(u"""
 impossibilité, le noeud  %(k1)s ne porte le ddl de rotation %(k2)s
"""),

33 : _(u"""
 il faut COEF_GROUP ou FONC_GROUP
"""),

34 : _(u"""
 un element n'est ni TRIA3 ni TRIA6 ni QUAD4 ni QUAD8
"""),

35 : _(u"""
 un element n'est ni TRIA3 ni TRIA6 ni TRIA7 ni QUAD4 ni QUAD8 ni QUAD9
"""),

36 : _(u"""
  le noeud  %(k1)s  doit appartenir à une seule maille
"""),

37 : _(u"""
 la maille à laquelle appartient le noeud  %(k1)s  doit etre de type SEG3
"""),

38 : _(u"""
 on ne trouve pas les angles nautiques pour le tuyau
"""),

39 : _(u"""
 option  %(k1)s  invalide
"""),

40 : _(u"""
 il faut indiquer le mot-cle 'NOEUD_2' ou 'GROUP_NO_2' après le mot-facteur  %(k1)s  pour l'option '3D_POU'.
"""),

41 : _(u"""
 il ne faut donner qu'un seul noeud de poutre a raccorder au massif.
"""),

42 : _(u"""
 il ne faut donner qu'un un seul GROUP_NO à un noeud à raccorder au massif.
"""),

43 : _(u"""
 il ne faut donner q'un seul noeud dans le GROUP_NO :  %(k1)s
"""),

44 : _(u"""
 impossibilité, le noeud  %(k1)s porte le ddl de rotation  %(k2)s
"""),

45 : _(u"""
 impossibilité, le noeud poutre  %(k1)s  devrait porter le ddl  %(k2)s
"""),

46 : _(u"""
 impossibilité, la surface de raccord du massif est nulle
"""),

47 : _(u"""
 il faut donner un CARA_ELEM pour récupérer les caractéristiques de tuyau.
"""),

48 : _(u"""
 il faut indiquer le mot-cle 'NOEUD_2' ou 'GROUP_NO_2' après le mot-facteur  %(k1)s  pour l'option  %(k2)s
"""),

49 : _(u"""
 il ne faut donner qu'un seul noeud de poutre à raccorder à la coque.
"""),

50 : _(u"""
 il ne faut donner qu'un seul GROUP_NO à un noeud à raccorder à la coque.
"""),

51 : _(u"""
 il faut donner un vecteur orientant l'axe de la poutre sous le mot-cle "AXE_POUTRE".
"""),

52 : _(u"""
 il faut donner un vecteur non nul orientant l'axe de la poutre sous le mot-cle "AXE_POUTRE".
"""),

53 : _(u"""
 il faut donner un CARA_ELEM pour récupérer l'épaisseur des éléments de bord.
"""),

54 : _(u"""
 impossibilité, le noeud  %(k1)s ne porte pas le ddl de rotation  %(k2)s
"""),

55 : _(u"""
 impossibilité, la surface de raccord de la coque est nulle
"""),

56 : _(u"""
 plusieurs comportements de type  %(k1)s  ont ete trouvés

  -> Conseil:
     Vous avez sans doute enrichi votre matériau. Vous ne pouvez pas
     avoir en meme temps les mots clés 'ELAS', 'ELAS_FO', 'ELAS_xxx',...
"""),

57 : _(u"""
 comportement de type  %(k1)s  non trouvé
"""),

58 : _(u"""
 nappe interdite pour les caractéristiques matériau
"""),

59 : _(u"""
 déformation plastique cumulée p < 0
"""),

60 : _(u"""
  Le prolongement à droite étant exclu pour la fonction %(k1)s,
  il n'est pas possible d'extrapoler la fonction R(p) au delà de p = %(r1)f
"""),

62 : _(u"""
 la limite d elasticite est deja renseignee dans elas_meta
"""),

63 : _(u"""
 objet  %(k1)s .materiau.nomrc non trouve
"""),

64 : _(u"""
 type sd non traité:  %(k1)s
"""),

69 : _(u"""
 le mot cle: %(k1)s  est identique (sur ses 8 1ers caracteres) à un autre.
"""),

70 : _(u"""
 erreur lors de la définition de la courbe de traction, il manque le paramètre : %(k1)s
"""),

71 : _(u"""
 erreur lors de la définition de la courbe de traction : %(k1)s  nb de points < 2  !
"""),

72 : _(u"""
 erreur lors de la définition de la courbe de traction : %(k1)s  nb de points < 1  !
"""),

73 : _(u"""
 erreurs rencontrées.
"""),

74 : _(u"""
 erreur lors de la définition de la nappe des courbes de traction: nb de points < 2 !
"""),

75 : _(u"""
 erreur lors de la définition de la nappe des courbes de traction:  %(k1)s  nb de points < 1 !
"""),

76 : _(u"""
  erreur lors de la définition de la courbe de traction: fonction ou nappe !
"""),

80 : _(u"""
 comportement TRACTION non trouve
"""),

81 : _(u"""
 fonction SIGM non trouvée
"""),

82 : _(u"""
 comportement meta_traction non trouvé
"""),

83 : _(u"""
 fonction SIGM_F1 non trouvée
"""),

84 : _(u"""
 fonction SIGM_F2 non trouvée
"""),

85 : _(u"""
 fonction SIGM_F3 non trouvée
"""),

86 : _(u"""
 fonction SIGM_F4 non trouvée
"""),

87 : _(u"""
 fonction SIGM_C non trouvée
"""),

88 : _(u"""
 fonction constante interdite pour la courbe de traction %(k1)s
"""),

89 : _(u"""
 prolongement à gauche EXCLU pour la courbe  %(k1)s
"""),

90 : _(u"""
 prolongement à droite EXCLU pour la courbe  %(k1)s
"""),

91 : _(u"""
 concept de type :  %(k1)s  interdit pour la courbe de traction %(k2)s
"""),

92 : _(u"""
 materiau : %(k1)s  non trouvé
"""),

93 : _(u"""
  les fonctions complexes ne sont pas implementées
"""),

94 : _(u"""
 nb param. > 30 materiau  %(k1)s
"""),

95 : _(u"""
 mauvaise definition de la plage de frequence, aucun mode pris en compte
"""),

96 : _(u"""
 les %(i1)d mailles imprimées ci-dessus n'appartiennent pas au modèle et pourtant elles ont ete affectées dans le mot-clé facteur : %(k1)s
"""),

97 : _(u"""
 FREQ INIT plus grande que FREQ FIN
"""),

98 : _(u"""
 FREQ INIT nécessaire avec CHAMNO
"""),

99 : _(u"""
 FREQ FIN nécessaire avec CHAMNO
"""),

}
