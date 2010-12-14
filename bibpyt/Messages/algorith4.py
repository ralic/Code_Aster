#@ MODIF algorith4 Messages  DATE 14/12/2010   AUTEUR PELLET J.PELLET 
# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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

cata_msg = {

35 : _("""
 rang superieur a dimension vecteur
"""),

36 : _("""
 <LCDPPA> il faut redecouper
"""),

45 : _("""
 la modelisation 1d n'est pas autorisée
"""),

48 : _("""
 élément à discontinuité avec une loi CZM_EXP : la matrice H est non inversible
"""),


50 : _("""
  comportement inattendu :  %(k1)s
"""),

51 : _("""
  SYT et D_SIGM_EPSI doivent être specifiés sous l'operande BETON_ECRO_LINE dans DEFI_MATERIAU pour utiliser la loi ENDO_ISOT_BETON
"""),

52 : _("""
  SYC ne doit pas être valorisé pour NU nul dans DEFI_MATERIAU
"""),

53 : _("""
  SYC doit etre supérieur à SQRT((1+NU-2*NU*NU)/(2.D0*NU*NU))*SYT 
  dans DEFI_MATERIAU pour prendre en compte le confinement
"""),

54 : _("""
 loi ENDO_ORTH_BETON : le paramètre KSI n'est pas inversible
"""),

57 : _("""
 pb de convergence (dgp neg)
"""),

58 : _("""
 pas de solution
"""),

59 : _("""
 erreur: pb de convergence
"""),

60 : _("""
 pb de convergence 2 (dgp neg)
"""),

61 : _("""
 erreur: pb de conv 2
"""),

62 : _("""
 loi BETON_REGLEMENT utilisable uniquement en modélisation C_PLAN ou D_PLAN
"""),

63 : _("""
 la méthode de localisation  %(k1)s  est indisponible actuellement
"""),

65 : _("""
  %(k1)s  impossible actuellement
"""),

68 : _("""
 PYRAMIDAL1 pas encore disponible
"""),

69 : _("""
 PYRAMIDAL2 pas encore disponible
"""),

70 : _("""
 JOINT_GRAIN pas encore disponible
"""),

71 : _("""
 RL pas encore disponible
"""),

72 : _("""
  jacobien du systeme non lineaire à résoudre nul
  lors de la projection au sommet du cone de traction
  les parametres matériaux sont sans doute mal définis
"""),

73 : _("""
  non convergence à itération maxi  %(k1)s  
  - erreur calculee  %(k2)s  >  %(k3)s
  mais tres faibles incréments de newton pour la loi BETON_DOUBLE_DP
  - on accepte la convergence.
"""),

74 : _("""
  non convergence à itération maxi  %(k1)s 
  - erreur calculée  %(k2)s  >  %(k3)s 
  - pour la loi BETON_DOUBLE_DP 
  - redécoupage du pas de temps
"""),

75 : _("""
 etat converge non conforme
 lors de la projection au sommet du cone de traction
"""),

76 : _("""
 etat converge non conforme en compression
 lors de la projection au sommet du cone de traction
"""),

77 : _("""
 jacobien du systeme non linéaire à résoudre nul
 lors de la projection au sommet des cones de compression et traction
 - les parametres matériaux sont sans doute mal définis.
"""),

78 : _("""
 état convergé non conforme en traction
 lors de la projection au sommet des deux cones
"""),

79 : _("""
 état convergé non conforme en compression
 lors de la projection au sommet des deux cones
"""),

80 : _("""
  jacobien du systeme non linéaire à résoudre nul
  lors de la projection au sommet du cone de compression
  - les parametres matériaux sont sans doute mal définis.
"""),

81 : _("""
 état convergé non conforme
 lors de la projection au sommet du cone de compression
"""),

82 : _("""
 état convergé non conforme en traction
 lors de la projection au sommet du cone de compression
"""),

83 : _("""
  jacobien du système non linéaire a resoudre nul
  - les parametres matériaux sont sans doute mal définis.
"""),

84 : _("""
 intégration élastoplastique de loi multi-critere : erreur de programmation
"""),

85 : _("""
  erreur de programmation : valeur de NSEUIL incorrecte.
"""),

86 : _("""
  état convergé non conforme en traction et en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

87 : _("""
  état converge non conforme en compression
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

88 : _("""
  état convergé non conforme en traction
  pour la loi de comportement BETON_DOUBLE_DP
  pour les deux critères en meme temps.
  il faut un saut élastique plus petit, ou redécouper le pas de temps
"""),

89 : _("""
 état convergé non conforme en traction
"""),

90 : _("""
 état convergé non conforme en compression
"""),

92 : _("""
 valeurs initiales non conformes :
 il y a probablement une erreur dans la programmation
"""),

94 : _("""
 il faut déclarer FONC_DESORP sous ELAS_FO pour le fluage propre                                avec sech comme parametre
"""),

98 : _("""
 nombre de valeurs dans le fichier UNV DATASET 58 non identique
"""),

99 : _("""
 nature du champ dans le fichier UNV DATASET 58 non identique
"""),

}
