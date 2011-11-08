#@ MODIF algorith4 Messages  DATE 07/11/2011   AUTEUR COURTOIS M.COURTOIS 
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

35 : _(u"""
 rang supérieur a dimension vecteur
"""),

36 : _(u"""
 <LCDPPA> il faut redécouper
"""),

45 : _(u"""
 la modélisation 1d n'est pas autorisée
"""),

48 : _(u"""
 élément à discontinuité avec une loi CZM_EXP : la matrice H est non inversible
"""),


50 : _(u"""
  comportement inattendu :  %(k1)s
"""),

51 : _(u"""
  SYT et D_SIGM_EPSI doivent être spécifiés sous l'opérande BETON_ECRO_LINE dans DEFI_MATERIAU pour utiliser la loi ENDO_ISOT_BETON
"""),

52 : _(u"""
  SYC ne doit pas être valorisé pour NU nul dans DEFI_MATERIAU
"""),

53 : _(u"""
  SYC doit être supérieur à SQRT((1+NU-2*NU*NU)/(2.D0*NU*NU))*SYT
  dans DEFI_MATERIAU pour prendre en compte le confinement
"""),

54 : _(u"""
 loi ENDO_ORTH_BETON : le paramètre KSI n'est pas inversible
"""),

57 : _(u"""
 Problème de convergence (DG négatif)
"""),

58 : _(u"""
 pas de solution
"""),

59 : _(u"""
 erreur: Problème de convergence
"""),

60 : _(u"""
 Problème de convergence 2 (DG négatif)
"""),

61 : _(u"""
 erreur: Problème de convergence 2
"""),

62 : _(u"""
 loi BETON_REGLE_PR utilisable uniquement en modélisation C_PLAN ou D_PLAN
"""),

63 : _(u"""
 la méthode de localisation  %(k1)s  est indisponible actuellement
"""),

65 : _(u"""
  %(k1)s  impossible actuellement
"""),


72 : _(u"""
  jacobien du système non linéaire à résoudre nul
  lors de la projection au sommet du cône de traction
  les paramètres matériaux sont sans doute mal définis
"""),

73 : _(u"""
  non convergence à itération max  %(k1)s
  - erreur calculée  %(k2)s  >  %(k3)s
  mais très faibles incréments de Newton pour la loi BETON_DOUBLE_DP
  - on accepte la convergence.
"""),

74 : _(u"""
  non convergence à itération max  %(k1)s
  - erreur calculée  %(k2)s  >  %(k3)s
  - pour la loi BETON_DOUBLE_DP
  - redécoupage du pas de temps
"""),

75 : _(u"""
 état converge non conforme
 lors de la projection au sommet du cône de traction
"""),

76 : _(u"""
 état converge non conforme en compression
 lors de la projection au sommet du cône de traction
"""),

77 : _(u"""
 jacobien du système non linéaire à résoudre nul
 lors de la projection au sommet des cônes de compression et traction
 - les paramètres matériaux sont sans doute mal définis.
"""),

78 : _(u"""
 état convergé non conforme en traction
 lors de la projection au sommet des deux cônes
"""),

79 : _(u"""
 état convergé non conforme en compression
 lors de la projection au sommet des deux cônes
"""),

80 : _(u"""
  jacobien du système non linéaire à résoudre nul
  lors de la projection au sommet du cône de compression
  - les paramètres matériaux sont sans doute mal définis.
"""),

81 : _(u"""
 état convergé non conforme
 lors de la projection au sommet du cône de compression
"""),

82 : _(u"""
 état convergé non conforme en traction
 lors de la projection au sommet du cône de compression
"""),

83 : _(u"""
  jacobien du système non linéaire a résoudre nul
  - les paramètres matériaux sont sans doute mal définis.
"""),

84 : _(u"""
 intégration élastoplastique de loi multi-critère : erreur de programmation
"""),

85 : _(u"""
  erreur de programmation : valeur de NSEUIL incorrecte.
"""),

86 : _(u"""
  état convergé non conforme en traction et en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en même temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

87 : _(u"""
  état converge non conforme en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en même temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

88 : _(u"""
  état convergé non conforme en traction
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en même temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

89 : _(u"""
 état convergé non conforme en traction
"""),

90 : _(u"""
 état convergé non conforme en compression
"""),

94 : _(u"""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage propre                                avec SECH comme paramètre
"""),

98 : _(u"""
 nombre de valeurs dans le fichier UNV DATASET 58 non identique
"""),

99 : _(u"""
 nature du champ dans le fichier UNV DATASET 58 non identique
"""),

}
