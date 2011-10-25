#@ MODIF modelisa6 Messages  DATE 25/10/2011   AUTEUR FLEJOU J-L.FLEJOU 
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
 Probl�me d'orientation: aucune maille ne touche le noeud indiqu�
"""),

2 : _(u"""
 Certaines mailles n'ont pas pu �tre r�orient�es. L'ensemble des mailles n'est pas connexe.
"""),

3 : _(u"""
 on ne trouve pas de noeud assez pr�s du noeud  %(k1)s
"""),

4 : _(u"""
  Erreurs dans les donn�es
"""),

5 : _(u"""
 Extraction de plus de noeuds que n'en contient la maille
"""),

6 : _(u"""
  Nombre de noeuds n�gatif
"""),

7 : _(u"""
 nombre de noeuds sommets non pr�vu
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
 les %(i1)d noeuds imprim�s ci-dessus n'appartiennent pas au mod�le (c'est � dire qu'ils ne portent aucun degr� de libert�) et pourtant ils ont ete affect�s dans le mot-cl� facteur : %(k1)s
"""),


17 : _(u"""
 la maille  %(k1)s  du group_ma  %(k2)s donne apres le mot cle  %(k3)s n'a pas un type g�om�trique autoris�
"""),

18 : _(u"""
 la maille  %(k1)s donn� apr�s le mot cl�  %(k2)s n'a pas un type g�om�trique autoris�
"""),

19 : _(u"""
  mot cle non admis : %(k1)s  les mots-cl�s admissibles sont :  %(k2)s  ou  %(k3)s  ou  %(k4)s  ou  %(k5)s  ou  %(k6)s  ou  %(k7)s  ou  %(k8)s ou  %(k9)s
"""),

20 : _(u"""
 ce type de maille n'est pas encore trait� :  %(k1)s
"""),

21 : _(u"""
 le nombre total de noeuds est /= de la somme des noeuds sommets, ar�tes et int�rieurs
"""),

22 : _(u"""
 les 2 listes %(k1)s  et  %(k2)s  ne sont pas de meme longueur
"""),

26 : _(u"""
 AFFE_FIBRE pour " %(k1)s ": il y a  %(k2)s  valeurs pour "VALE", ce devrait �tre un multiple de 3
"""),

27 : _(u"""
 dans le maillage " %(k1)s " la maille " %(k2)s " est de type " %(k3)s " (ni TRIA3 ni QUAD4)
"""),

28 : _(u"""
 Le noeud <%(k1)s> de la poutre, de coordonn�es <%(r1)g  %(r2)g  %(r3)g>,
ne doit pas appartenir � des mailles constituant la trace de la poutre sur la coque.
Le probl�me vient de l'occurence %(i1)d de LIAISON_ELEM.

Solution : Il faut d�doubler le noeud.
"""),

30 : _(u"""
 l'indicateur :  %(k1)s de position des multiplicateurs de lagrange associ�s � une relation lineaire n'est pas correct.
"""),

31 : _(u"""
  ->  En thermique, les fonctions d�finissant le mat�riau (enthalpie, capacit� calorifique, conductivit�)
      doivent obligatoirement �tre d�crites par des fonctions tabul�es et non des formules.
      En effet, on a besoin d'�valuer la d�riv�e de ces fonctions. Elle peut �tre plus facilement et
      pr�cis�ment obtenue pour une fonction lin�aire par morceaux que pour une expression 'formule'.
  -> Conseil:
      Tabulez votre formule, � une finesse de discr�tisation d'abcisse (TEMP) � votre convenance,
      par la commande CALC_FONC_INTERP
 """),

32 : _(u"""
 impossibilit�, le noeud  %(k1)s ne porte le ddl de rotation %(k2)s
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
  le noeud  %(k1)s  doit appartenir � une seule maille
"""),

37 : _(u"""
 la maille � laquelle appartient le noeud  %(k1)s  doit etre de type SEG3
"""),

38 : _(u"""
 on ne trouve pas les angles nautiques pour le tuyau
"""),

39 : _(u"""
 option  %(k1)s  invalide
"""),

40 : _(u"""
 il faut indiquer le mot-cle 'NOEUD_2' ou 'GROUP_NO_2' apr�s le mot-facteur  %(k1)s  pour l'option '3D_POU'.
"""),

41 : _(u"""
 il ne faut donner qu'un seul noeud de poutre a raccorder au massif.
"""),

42 : _(u"""
 il ne faut donner qu'un un seul GROUP_NO � un noeud � raccorder au massif.
"""),

43 : _(u"""
 il ne faut donner q'un seul noeud dans le GROUP_NO :  %(k1)s
"""),

44 : _(u"""
 impossibilit�, le noeud  %(k1)s porte le ddl de rotation  %(k2)s
"""),

45 : _(u"""
 impossibilit�, le noeud poutre  %(k1)s  devrait porter le ddl  %(k2)s
"""),

46 : _(u"""
 impossibilit�, la surface de raccord du massif est nulle
"""),

47 : _(u"""
 il faut donner un CARA_ELEM pour r�cup�rer les caract�ristiques de tuyau.
"""),

48 : _(u"""
 il faut indiquer le mot-cle 'NOEUD_2' ou 'GROUP_NO_2' apr�s le mot-facteur  %(k1)s  pour l'option  %(k2)s
"""),

49 : _(u"""
 il ne faut donner qu'un seul noeud de poutre � raccorder � la coque.
"""),

50 : _(u"""
 il ne faut donner qu'un seul GROUP_NO � un noeud � raccorder � la coque.
"""),

51 : _(u"""
 il faut donner un vecteur orientant l'axe de la poutre sous le mot-cle "AXE_POUTRE".
"""),

52 : _(u"""
 il faut donner un vecteur non nul orientant l'axe de la poutre sous le mot-cle "AXE_POUTRE".
"""),

53 : _(u"""
 il faut donner un CARA_ELEM pour r�cup�rer l'�paisseur des �l�ments de bord.
"""),

54 : _(u"""
 impossibilit�, le noeud  %(k1)s ne porte pas le ddl de rotation  %(k2)s
"""),

55 : _(u"""
 impossibilit�, la surface de raccord de la coque est nulle
"""),

56 : _(u"""
 plusieurs comportements de type  %(k1)s  ont ete trouv�s

  -> Conseil:
     Vous avez sans doute enrichi votre mat�riau. Vous ne pouvez pas
     avoir en meme temps les mots cl�s 'ELAS', 'ELAS_FO', 'ELAS_xxx',...
"""),

57 : _(u"""
 comportement de type  %(k1)s  non trouv�
"""),

58 : _(u"""
 nappe interdite pour les caract�ristiques mat�riau
"""),

59 : _(u"""
 d�formation plastique cumul�e p < 0
"""),

60 : _(u"""
  Le prolongement � droite �tant exclu pour la fonction %(k1)s,
  il n'est pas possible d'extrapoler la fonction R(p) au del� de p = %(r1)f
"""),

62 : _(u"""
 la limite d elasticite est deja renseignee dans elas_meta
"""),

63 : _(u"""
 objet  %(k1)s .materiau.nomrc non trouve
"""),

64 : _(u"""
 type sd non trait�:  %(k1)s
"""),

69 : _(u"""
 le mot cle: %(k1)s  est identique (sur ses 8 1ers caracteres) � un autre.
"""),

70 : _(u"""
 erreur lors de la d�finition de la courbe de traction, il manque le param�tre : %(k1)s
"""),

71 : _(u"""
 erreur lors de la d�finition de la courbe de traction : %(k1)s  nb de points < 2  !
"""),

72 : _(u"""
 erreur lors de la d�finition de la courbe de traction : %(k1)s  nb de points < 1  !
"""),

73 : _(u"""
 erreurs rencontr�es.
"""),

74 : _(u"""
 erreur lors de la d�finition de la nappe des courbes de traction: nb de points < 2 !
"""),

75 : _(u"""
 erreur lors de la d�finition de la nappe des courbes de traction:  %(k1)s  nb de points < 1 !
"""),

76 : _(u"""
  erreur lors de la d�finition de la courbe de traction: fonction ou nappe !
"""),

80 : _(u"""
 comportement TRACTION non trouve
"""),

81 : _(u"""
 fonction SIGM non trouv�e
"""),

82 : _(u"""
 comportement meta_traction non trouv�
"""),

83 : _(u"""
 fonction SIGM_F1 non trouv�e
"""),

84 : _(u"""
 fonction SIGM_F2 non trouv�e
"""),

85 : _(u"""
 fonction SIGM_F3 non trouv�e
"""),

86 : _(u"""
 fonction SIGM_F4 non trouv�e
"""),

87 : _(u"""
 fonction SIGM_C non trouv�e
"""),

88 : _(u"""
 fonction constante interdite pour la courbe de traction %(k1)s
"""),

89 : _(u"""
 prolongement � gauche EXCLU pour la courbe  %(k1)s
"""),

90 : _(u"""
 prolongement � droite EXCLU pour la courbe  %(k1)s
"""),

91 : _(u"""
 concept de type :  %(k1)s  interdit pour la courbe de traction %(k2)s
"""),

92 : _(u"""
 mat�riau : %(k1)s  non affect� par la commande AFFE_MATERIAU.
"""),

93 : _(u"""
  les fonctions complexes ne sont pas implement�es
"""),

94 : _(u"""
 nb param. > 30 materiau  %(k1)s
"""),

95 : _(u"""
 mauvaise definition de la plage de frequence, aucun mode pris en compte
"""),

96 : _(u"""
 les %(i1)d mailles imprim�es ci-dessus n'appartiennent pas au mod�le et pourtant elles ont ete affect�es dans le mot-cl� facteur : %(k1)s
"""),

97 : _(u"""
 FREQ INIT plus grande que FREQ FIN
"""),

98 : _(u"""
 FREQ INIT n�cessaire avec CHAMNO
"""),

99 : _(u"""
 FREQ FIN n�cessaire avec CHAMNO
"""),

}
