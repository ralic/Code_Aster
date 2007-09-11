#@ MODIF algorith4 Messages  DATE 11/09/2007   AUTEUR DURAND C.DURAND 
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
 code-colonne inconnu (dvlp)
"""),

2: _("""
 trop de colonnes affichees (dvlp)
"""),

3: _("""
 trop de colonnes de suivi (limite a quatre)
"""),

4: _("""
 mode incorrect (dvlp)
"""),

5: _("""
 fichier doit etre défini dans la première occurrence
"""),

6: _("""
 fichier sans unité
"""),

7: _("""
 format d'affichage trop grand
"""),

8: _("""
 longueur format excessif (dvlp)
"""),

9: _("""
 dépassement de capacité d'affichage (dvlp)
"""),

10: _("""
 unité logique invalide (dvlp)
"""),

11: _("""
 sd affichage inexistante
"""),

12: _("""
 nombre incorrrect de colonnes (erreur interne)
"""),

13: _("""
 option inconnue pour INFO_RESIDU
"""),

14: _("""
 trop de colonnes d'affichage (limité à quinze)
"""),

15: _("""
 ligne trop large (dvlp)
"""),

16: _("""
 INFO_RESIDU ne correspond pas à un résidu
"""),

17: _("""
 nombre lignes titre incorrecte (dvlp)
"""),

18: _("""
 nombre colonnes incorrecte (dvlp)
"""),

19: _("""
 code colonne incorrect (dvlp)
"""),

20: _("""
 erreur operation sur sd impression colonnes (dvlp)
"""),

21: _("""
 format trop grand pour la largeur max. d'une colonne
"""),

22: _("""
 erreur ajou sur sd impression colonnes (dvlp)
"""),

23: _("""
 erreur sd impression colonnes pleines (dvlp)
"""),

24: _("""
 erreur supp sur sd impression colonnes (dvlp)
"""),

25: _("""
 erreur lire sur sd impression colonnes (dvlp)
"""),

26: _("""
 erreur ecri sur sd impression colonnes (dvlp)
"""),

27: _("""
 ne correspond a aucun type de colonne
"""),

28: _("""
 marquage interdit pour des colonnes de type texte
"""),

29: _("""
 ne correspond a aucun format de colonne
"""),

30: _("""
 ne correspond a aucune colonne
"""),

35: _("""
 rang superieur a dimension vecteur
"""),

36: _("""
 erreurresistance f_c < 0  ou = 0 !
"""),

37: _("""
 erreurf_t < 0 !
"""),

38: _("""
 erreur - valeur de crit_e_c superieure  a  1
"""),

39: _("""
 erreur - valeur de crit_e_c negative !!!!
"""),

40: _("""
 erreur - valeur de epsp_p_c negative !!!!
"""),

41: _("""
 erreur - valeur de epsp_r_c negative !!!!
"""),

42: _("""
 erreur - valeur de epsi_r_t negative !!!!
"""),

43: _("""
 erreur - valeur de fac_t_c negative ou > 1   !!!!
"""),





45: _("""
 la modelisation 1d n est pas autorisee
"""),

46: _("""
 element: %(k1)s non implante
"""),

47: _("""
 probleme sur le type d option
"""),

48: _("""
 matrice h non inversible
"""),

49: _("""
 modelisation  %(k1)s imcompatible avec la loi beton_double_dp .
"""),

50: _("""
  comportement inattendu :  %(k1)s
"""),

51: _("""
  syt et d_sigm_epsi doivent                        etre specifies sous l operande beton_ecro_line                    dans defi_materiau pour utiliser                                  la loi endo_isot_beton
"""),

52: _("""
  syc ne doit pas etre                valorise pour nu nul dans defi_materiau
"""),

53: _("""
  syc doit etre supérieur à SQRT((1+NU-2*NU*NU)/(2.D0*NU*NU))*SYT 
  dans DEFI_MATERIAU pour prendre en compte le confinement
"""),

54: _("""
 KSI non inversible
"""),

55: _("""
 CV approche 0 impossible
"""),

56: _("""
 dvp : option  %(k1)s  non prévue
"""),

57: _("""
 pb de convergence (dgp neg)
"""),

58: _("""
 pas de solution
"""),

59: _("""
 erreur: pb de convergence
"""),

60: _("""
 pb de convergence 2 (dgp neg)
"""),

61: _("""
 erreur: pb de conv 2
"""),

62: _("""
 loi BETON_REGLEMENT utilisable uniquement en modélisation C_PLAN ou D_PLAN
"""),

63: _("""
 la méthode de localisation  %(k1)s  est indisponible actuellement
"""),

64: _("""
 interaction non disponible
"""),

65: _("""
  %(k1)s  impossible actuellement
"""),

66: _("""
 augmenter NMAT
"""),

67: _("""
 ECOU_PLAS1 non disponible
"""),

68: _("""
 PYRAMIDAL1 pas encore disponible
"""),

69: _("""
 PYRAMIDAL2 pas encore disponible
"""),

70: _("""
 JOINT_GRAIN pas encore disponible
"""),

71: _("""
 RL pas encore disponible
"""),

72: _("""
  jacobien du systeme non lineaire à résoudre nul
  lors de la projection au sommet du cone de traction
  les parametres matériaux sont sans doute mal définis
"""),

73: _("""
  non convergence à itération maxi  %(k1)s  
  - erreur calculee  %(k2)s  >  %(k3)s
  mais tres faibles incréments de newton pour la loi BETON_DOUBLE_DP
  - on accepte la convergence.
"""),

74: _("""
  non convergence à itération maxi  %(k1)s 
  - erreur calculée  %(k2)s  >  %(k3)s 
  - pour la loi BETON_DOUBLE_DP 
  - redécoupage du pas de temps
"""),

75: _("""
 etat converge non conforme
 lors de la projection au sommet du cone de traction
"""),

76: _("""
 etat converge non conforme en compression
 lors de la projection au sommet du cone de traction
"""),

77: _("""
 jacobien du systeme non linéaire à résoudre nul
 lors de la projection au sommet des cones de compression et traction
 - les parametres matériaux sont sans doute mal définis.
"""),

78: _("""
 état convergé non conforme en traction
 lors de la projection au sommet des deux cones
"""),

79: _("""
 état convergé non conforme en compression
 lors de la projection au sommet des deux cones
"""),

80: _("""
  jacobien du systeme non linéaire à résoudre nul
  lors de la projection au sommet du cone de compression
  - les parametres matériaux sont sans doute mal définis.
"""),

81: _("""
 état convergé non conforme
 lors de la projection au sommet du cone de compression
"""),

82: _("""
 état convergé non conforme en traction
 lors de la projection au sommet du cone de compression
"""),

83: _("""
  jacobien du système non linéaire a resoudre nul
  - les parametres matériaux sont sans doute mal définis.
"""),

84: _("""
 intégration élastoplastique de loi multi-critere : erreur de programmation
"""),

85: _("""
  erreur de programmation : valeur de NSEUIL incorrecte.
"""),

86: _("""
  état convergé non conforme en traction et en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

87: _("""
  état converge non conforme en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

88: _("""
  état convergé non conforme en traction
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

89: _("""
 état convergé non conforme en traction
"""),

90: _("""
 état convergé non conforme en compression
"""),

91: _("""
 option  %(k1)s  non prévue (dvlp)
"""),

92: _("""
 valeurs initiales non conformes :
 il y a probablement une erreur dans la programmation
"""),

93: _("""
 cet algorithme ne traite pas encore les options RIGI_MECA_TANG et FULL_MECA
"""),

94: _("""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage propre                                avec sech comme parametre
"""),

95: _("""
 division par zéro dans LCUMFS
"""),

96: _("""
 erreur dans LCUMME : pb de dimension
"""),

97: _("""
 on ne traite pas actuellement plusieurs NOM_CHAM simultanément,
 on ne considère que le premier argument
"""),

98: _("""
 nombre de valeurs dans le fichier UNV DATASET 58 non identique
"""),

99: _("""
 nature du champ dans le fichier UNV DATASET 58 non identique
"""),
}
