#@ MODIF algorith8 Messages  DATE 02/10/2007   AUTEUR MACOCCO K.MACOCCO 
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
 contraintes planes en grandes déformations non implantées
"""),

2: _("""
 caractéristique fluage incomplet
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

12: _("""
 F reste toujours négative
"""),

13: _("""
 F  reste toujours positive
"""),

14: _("""
 signe de SIGMA indeterminé
"""),

15: _("""
 changement de signe de SIGMA
"""),

16: _("""
 F=0 : pas converge
"""),

17: _("""
 dvp : non cohérent
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
 type d'élément incompatible avec une loi élastique anisotrope
"""),

23: _("""
 dénominateur nul dans le calcul de ETA_PILOTAGE
"""),

24: _("""
 cisaillement suiveur non implanté
"""),

25: _("""
 pression imposée sur l'axe des coordonnées cylindriques
"""),

26: _("""
 mode non defini
"""),

27: _("""
 lecture du champ DEPL_CALCULE impossible
"""),

28: _("""
 prédiction par extrapolation impossible : pas de temps nul
"""),

29: _("""
 ITER_LINE_MAXI doit etre inférieur à 1000
"""),

30: _("""
 mauvaise estimation de f
"""),

31: _("""
 borne superieure PMAX incorrecte
"""),

32: _("""
 viscosité N égale à zéro
"""),

33: _("""
 viscosité UN_SUR_K égale à zéro
"""),

34: _("""
 g=0 : pas convergé
"""),

35: _("""
 incompatibilité entre la loi de couplage  %(k1)s  et la modélisation choisie  %(k2)s
"""),

36: _("""
 il y a deja une loi de couplage
"""),

37: _("""
 il y a deja une loi hydraulique
"""),

38: _("""
 il y a deja une loi de mécanique
"""),

39: _("""
 il n y a pas de loi de couplage
"""),

40: _("""
 il n y a pas de loi hydraulique
"""),

41: _("""
 il n y a pas de loi de mécanique
"""),

42: _("""
 la loi de couplage est incorrecte pour une modélisation HM
"""),

43: _("""
 incompatibilite des comportements mécanique et hydraulique
"""),

44: _("""
 loi de mécanique incompatible avec une modelisation HM
"""),

45: _("""
 la loi de couplage est incorrecte pour une modélisation HHM
"""),

46: _("""
 loi de mécanique incompatible avec une loi HHM
"""),

47: _("""
 loi de mécanique incompatible avec une modélisation HHM
"""),

48: _("""
 il y a une loi de mécanique dans la relation THH
"""),

49: _("""
 la loi de couplage est incorrecte pour une modélisation THH
"""),

50: _("""
 loi de mécanique incompatible avec une loi THH
"""),

51: _("""
 il y a une loi de mecanique dans la relation THV
"""),

52: _("""
 la loi de couplage est incorrecte pour une modélisation THV
"""),

53: _("""
 loi de mécanique incompatible avec une loi THV
"""),

54: _("""
 la loi de couplage est incorrecte pour une modélisation THM
"""),

55: _("""
 loi de mécanique incompatible avec une modélisation THM
"""),

56: _("""
 la loi de couplage est incorrecte pour une modélisation THHM
"""),

57: _("""
 Loi de mécanique incompatible avec une modélisation THHM
"""),

58: _("""
 Méthode non implantée
"""),

59: _("""
 Champ 'IN' inexistant
"""),

61: _("""
 Il manque le séchage de référence (AFFE_MATERIAU/AFFE_VARC/VALE_REF)
"""),

65: _("""
 echec loi de comportement dans ZEROFO
"""),

66: _("""
  convergence atteinte sur approximation linéaire tangente de l'évolution plastique
  risque d'imprecision
"""),

67: _("""
  endommagement maximal atteint au cours des resolutions internes
"""),

68: _("""
  erreur récupération paramètres matériau
"""),

69: _("""
  type de matrice demandé non disponible
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
 le nombre de composantes dans le champ de vent est incorrect. on doit avoir : DX, DY, DZ
"""),

78: _("""
 F(0)=0
"""),

80: _("""
 SY >= SU. impossible.
"""),

81: _("""
 EP >= E. impossible.
"""),

82: _("""
 incohérence dans les données matériau : MEY > MPY impossible.
"""),

83: _("""
 incohérence dans les données matériau : MEZ > MPZ impossible.
"""),

84: _("""
 comportement de fluage sous irradiation inconnu
"""),

85: _("""
 definition multiple du comportement pour un élément de poutre
"""),

86: _("""
 porosité strictement nulle( cas non traité)
"""),

87: _("""
 l'incrément de temps vaut zéro, vérifiez votre découpage
"""),

88: _("""
 fluence décroissante (flux<0)
"""),

89: _("""
 le parametre A doit etre >=0
"""),

90: _("""
 la loi LMARC_IRRA n'est compatible qu'avec une modélisation poutre
"""),

91: _("""
 stop, RIGI_MECA_TANG non disponible
"""),

92: _("""
 la maille doit etre de type TETRA10,PENTA15,HEXA20,QUAD8 ou TRIA6.
 or la maille est de type :  %(k1)s .
"""),

93: _("""
 une maille maitre est de longueur nulle
"""),

94: _("""
 le champ issu du concept %(k1)s n'est pas calculé à l'instant %(i3)i
"""),

96: _("""
 le séchage ne peut pas etre mélangé à un autre comportement
"""),

97: _("""
 EVOL_THER_SECH est un mot-clé obligatoire pour le séchage de type SECH_GRANGER et SECH_NAPPE
"""),

98: _("""
  le concept :  %(k1)s  n'est pas un champ de température
"""),

99: _("""
  le concept EVOL_THER :  %(k1)s  ne contient aucun champ de température
"""),
}
