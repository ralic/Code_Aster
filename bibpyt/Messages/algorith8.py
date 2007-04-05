#@ MODIF algorith8 Messages  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
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
 contraintes planes en grandes deformations non implantees
"""),

2: _("""
 caracteristique fluage                             incomplet
"""),

3: _("""
 aucun noeud n'est present dans la zone consideree 2
"""),

4: _("""
 aucun noeud n'est present dans la zone consideree 1
"""),

5: _("""
 aucun noeud n'est present dans la zone consideree 3
"""),

6: _("""
 message d'erreur inconnu (dvlp)
"""),

7: _("""
 incoherence de taille (dvlp)
"""),

8: _("""
 format de colonne inconnu (dvlp)
"""),

9: _("""
 erreur cham_elem_s
"""),

10: _("""
  bug 1
"""),

11: _("""
  bug 2
"""),

12: _("""
 f reste toujours negative
"""),

13: _("""
 f  reste toujours positive
"""),

14: _("""
 signe de sigma indetermine
"""),

15: _("""
 changement de signe de sigma
"""),

16: _("""
 f=0 : pas converge
"""),

17: _("""
 dvp : non coherent
"""),

18: _("""
 phase inconnue (dvlp)
"""),

19: _("""
 trop d'amortissements modaux
"""),

20: _("""
 La définition du repère d'orthotropie a été mal faite.
 Utilisez soit ANGL_REP  soit ANGL_AXE de la commande AFFE_CARA_ELEM mot clé facteur MASSIF
"""),

22: _("""
 type d element incompatible avec une loi elastique anisotrope
"""),

23: _("""
 denominateur nul dans le calcul de eta_pilotage
"""),

24: _("""
 cisaillement suiveur non implante
"""),

25: _("""
 pression imposee sur l'axe des coordonnees cylindriques
"""),

26: _("""
 mode non defini
"""),

27: _("""
 lecture du champ depl_calcule impossible
"""),

28: _("""
 prediction par extrapolation impossible : pas de temps nul
"""),

29: _("""
 iter_line_maxi doit etre inferieur a 1000
"""),

30: _("""
 mauvaise estimation de f
"""),

31: _("""
 borne superieure pmax incorrecte
"""),

32: _("""
 viscosite n egale a zero
"""),

33: _("""
 viscosite un_sur_k egale a zero
"""),

34: _("""
 g=0 : pas converge
"""),

35: _("""
 incompatibilite entre la loi de couplage  %(k1)s  et la modelisation choisi  %(k2)s
"""),

36: _("""
 il y a deja une loi de couplage
"""),

37: _("""
 il y a deja une loi hydraulique
"""),

38: _("""
 il y a deja une loi de mecanique
"""),

39: _("""
 il n y a pas de loi de couplage
"""),

40: _("""
 il n y a pas de loi hydraulique
"""),

41: _("""
 il n y a pas de loi de mecanique
"""),

42: _("""
 la loi de couplage est incorrecte pour une modelisation hm
"""),

43: _("""
 incompatibilite des comportements mecanique et hydraulique
"""),

44: _("""
 loi de mecanique incompatible avec une modelisation hm
"""),

45: _("""
 la loi de couplage est incorrecte pour une modelisation hhm
"""),

46: _("""
 loi de mecanique incompatible avec une loi hhm
"""),

47: _("""
 loi de mecanique incompatible avec une modelisation hhm
"""),

48: _("""
 il y a une loi de mecanique dans la relation thh
"""),

49: _("""
 la loi de couplage est incorrecte pour une modelisation thh
"""),

50: _("""
 loi de mecanique incompatible avec une loi thh
"""),

51: _("""
 il y a une loi de mecanique dans la relation thv
"""),

52: _("""
 la loi de couplage est incorrecte pour une modelisation thv
"""),

53: _("""
 loi de mecanique incompatible avec une loi thv
"""),

54: _("""
 la loi de couplage est incorrecte pour une modelisation thm
"""),

55: _("""
 loi de mecanique incompatible avec une modelisation thm
"""),

56: _("""
 la loi de couplage est incorrecte pour une modelisation thhm
"""),

57: _("""
 Loi de mecanique incompatible avec une modelisation THHM
"""),

58: _("""
 Methode non implantee
"""),

59: _("""
 Champ 'IN' inexistant
"""),






61: _("""
 Il manque le séchage de référence (AFFE_MATERIAU/AFFE_VARC/VALE_REF)
"""),




65: _("""
 echec loi de comp dans zerofo
"""),

66: _("""
  convergence atteinte surapproximation lineaire tangente de l'evolution plastique- risque d'imprecision
"""),

67: _("""
  endommagement maximal atteint au cours des resolutions internes
"""),

68: _("""
  erreur recuperation parametres materiau
"""),

69: _("""
  type de matrice demande non disponible
"""),

70: _("""
  erreur dans nmvecd
"""),

71: _("""
 valo >0
"""),

72: _("""
 dr negatif
"""),

73: _("""
 pb2 seq
"""),

74: _("""
 pb4 seq
"""),

75: _("""
 pb1 seq
"""),

76: _("""
 pb3 seq
"""),

77: _("""
 le nombre de composante dans le champ de vent est incorrect. on doit avoir : dx, dy, dz
"""),

78: _("""
 f(0)=0
"""),








80: _("""
 sy >= su. impossible.
"""),

81: _("""
 ep >= e. impossible.
"""),

82: _("""
 incoherence dans les donnees materiau : mey > mpy impossible.
"""),

83: _("""
 incoherence dans les donnees materiau : mez > mpz impossible.
"""),

84: _("""
 comportement de fluage sous irradiation inconnu
"""),

85: _("""
 definition multiple du comportement pour un element de poutre
"""),

86: _("""
 porosite strictement nulle( cas non traite)
"""),

87: _("""
 l'increment de temps vaut zero, verifier votre decoupage
"""),

88: _("""
 fluence decroissante (flux<0)
"""),

89: _("""
 le parametre a doit etre >=0
"""),

90: _("""
 la loi lmarc_irran'est compatible qu'avec une modelisation poutre
"""),

91: _("""
 stop, rigi_meca_tang non                         disponible
"""),

92: _("""
 la maille doit etre de type tetra10,penta15,hexa20,quad8 ou tria6. or la maille est de type :  %(k1)s .
"""),

93: _("""
 une maille maitre est de longueur nulle
"""),

96: _("""
 le sechage ne peut pas etre melange a un autre comportement
"""),

97: _("""
 evol_ther_sech est un mot-cle obligatoire pour le sechage de type sech_granger et sech_nappe
"""),

98: _("""
  le concept :  %(k1)s  n'est pas un champ de temperature
"""),

99: _("""
  le concept evol_ther :  %(k1)s  ne contient aucun champ de temperature
"""),
}
