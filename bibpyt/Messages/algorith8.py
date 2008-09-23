#@ MODIF algorith8 Messages  DATE 23/09/2008   AUTEUR ABBAS M.ABBAS 
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

1 : _("""
 contraintes planes en grandes déformations non implantées
"""),

2 : _("""
 caractéristique fluage incomplet
"""),

8 : _("""
 format de colonne inconnu (dvlp)
"""),

12 : _("""
 F reste toujours négative
"""),

13 : _("""
 F  reste toujours positive
"""),

14 : _("""
 signe de SIGMA indeterminé
"""),

15 : _("""
 changement de signe de SIGMA
"""),

16 : _("""
 F=0 : pas converge
"""),


20 : _("""
 La définition du repère d'orthotropie a été mal faite.
 Utilisez soit ANGL_REP  soit ANGL_AXE de la commande AFFE_CARA_ELEM mot clé facteur MASSIF
"""),

22 : _("""
 type d'élément incompatible avec une loi élastique anisotrope
"""),

24 : _("""
 cisaillement suiveur non implanté
"""),

25 : _("""
 pression imposée sur l'axe des coordonnées cylindriques
"""),

26 : _("""
 mode non defini
"""),

28 : _("""
 prédiction par extrapolation impossible : pas de temps nul
"""),

31 : _("""
 borne superieure PMAX incorrecte
"""),

32 : _("""
 la viscosité N doit être différente de zéro
"""),

33 : _("""
 la viscosité UN_SUR_K doit être différente de zéro
"""),

35 : _("""
 incompatibilité entre la loi de couplage  %(k1)s  et la modélisation choisie  %(k2)s
"""),

36 : _("""
 il y a deja une loi de couplage
"""),

37 : _("""
 il y a deja une loi hydraulique
"""),

38 : _("""
 il y a deja une loi de mécanique
"""),

39 : _("""
 il n y a pas de loi de couplage
"""),

40 : _("""
 il n y a pas de loi hydraulique
"""),

41 : _("""
 il n y a pas de loi de mécanique
"""),

42 : _("""
 la loi de couplage est incorrecte pour une modélisation HM
"""),

43 : _("""
 incompatibilite des comportements mécanique et hydraulique
"""),

44 : _("""
 loi de mécanique incompatible avec une modelisation HM
"""),

45 : _("""
 la loi de couplage est incorrecte pour une modélisation HHM
"""),

47 : _("""
 loi de mécanique incompatible avec une modélisation HHM
"""),

48 : _("""
 il y a une loi de mécanique dans la relation THH
"""),

49 : _("""
 la loi de couplage est incorrecte pour une modélisation THH
"""),

50 : _("""
 loi de mécanique incompatible avec une loi THH
"""),

51 : _("""
 il y a une loi de mecanique dans la relation THV
"""),

52 : _("""
 la loi de couplage est incorrecte pour une modélisation THV
"""),

53 : _("""
 loi de mécanique incompatible avec une loi THV
"""),

54 : _("""
 la loi de couplage est incorrecte pour une modélisation THM
"""),

55 : _("""
 loi de mécanique incompatible avec une modélisation THM
"""),

56 : _("""
 la loi de couplage est incorrecte pour une modélisation THHM
"""),

57 : _("""
 Loi de mécanique incompatible avec une modélisation THHM
"""),

61 : _("""
 Il manque le séchage de référence (AFFE_MATERIAU/AFFE_VARC/VALE_REF)
"""),

65 : _("""
 echec loi de comportement dans ZEROFO
"""),

66 : _("""
  convergence atteinte sur approximation linéaire tangente de l'évolution plastique
  risque d'imprecision
"""),

67 : _("""
  endommagement maximal atteint au cours des resolutions internes
"""),

77 : _("""
 le nombre de composantes dans le champ de vent est incorrect. on doit avoir : DX, DY, DZ
"""),

80 : _("""
Pour le comportement %(k3)s, matériau %(k4)s. Incohérence dans les données matériau.
   %(k1)s est >= %(k2)s cela n'est pas possible.
   valeur de %(k1)s : %(r1)E
   valeur de %(k2)s : %(r2)E
"""),

81 : _("""
L'association comportement vs matériau est incorrecte.
Les combinaisons possibles sont :
   comportement %(k1)s et matériau %(k2)s et %(k5)s
   comportement %(k3)s et matériau %(k4)s et %(k5)s
"""),


86 : _("""
 porosité strictement nulle( cas non traité)
"""),

87 : _("""
 l'incrément de temps vaut zéro, vérifiez votre découpage
"""),

88 : _("""
 fluence décroissante (flux<0)
"""),

89 : _("""
 le parametre A doit etre >=0
"""),

90 : _("""
 la loi LMARC_IRRA n'est compatible qu'avec une modélisation poutre
"""),

91 : _("""
 stop, RIGI_MECA_TANG non disponible
"""),

92 : _("""
 la maille doit etre de type TETRA10,PENTA15,HEXA20,QUAD8 ou TRIA6.
 or la maille est de type :  %(k1)s .
"""),

94 : _("""
 le champ issu du concept %(k1)s n'est pas calculé à l'instant %(i3)i
"""),

96 : _("""
 le séchage ne peut pas etre mélangé à un autre comportement
"""),

97 : _("""
 EVOL_THER_SECH est un mot-clé obligatoire pour le séchage de type SECH_GRANGER et SECH_NAPPE
"""),

98 : _("""
  le concept :  %(k1)s  n'est pas un champ de température
"""),

99 : _("""
  le concept EVOL_THER :  %(k1)s  ne contient aucun champ de température
"""),

}
