# -*- coding: iso-8859-1 -*-
#            CONFIGURATION MANAGEMENT OF EDF VERSION
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
# person_in_charge: josselin.delmas at edf.fr

cata_msg = {

1 : _(u"""
 contraintes planes en grandes déformations non implantées
"""),

2 : _(u"""
 caractéristique fluage incomplet
"""),

12 : _(u"""
 F reste toujours négative
"""),

13 : _(u"""
 F  reste toujours positive
"""),

14 : _(u"""
 signe de SIGMA indéterminé
"""),

15 : _(u"""
 changement de signe de SIGMA
"""),

16 : _(u"""
 F=0 : pas converge
"""),


20 : _(u"""
 La définition du repère d'orthotropie a été mal faite.
 Utilisez soit ANGL_REP  soit ANGL_AXE de la commande AFFE_CARA_ELEM mot clé facteur MASSIF
"""),

22 : _(u"""
 type d'élément incompatible avec une loi élastique anisotrope
"""),

24 : _(u"""
 Le chargement de type cisaillement (mot-clé CISA_2D) ne peut pas être suiveur (mot-clé TYPE_CHAR='SUIV').
"""),

25 : _(u"""
 On ne sait pas traiter un chargement de type pression (mot-clé PRES_REP) suiveuse (mot-clé TYPE_CHAR_='SUIV') imposé sur l'axe du modèle axisymétrique.

 Conseils :
  - Vérifiez que le chargement doit bien être suiveur.
  - Vérifiez que la zone d'application du chargement est la bonne.
"""),

28 : _(u"""
 prédiction par extrapolation impossible : pas de temps nul
"""),

31 : _(u"""
 borne supérieure PMAX incorrecte
"""),

32 : _(u"""
 la viscosité N doit être différente de zéro
"""),

33 : _(u"""
 la viscosité UN_SUR_K doit être différente de zéro
"""),

35 : _(u"""
 incompatibilité entre la loi de couplage  %(k1)s  et la modélisation choisie  %(k2)s
"""),

36 : _(u"""
 il y a déjà une loi de couplage
"""),

37 : _(u"""
 il y a déjà une loi hydraulique
"""),

38 : _(u"""
 il y a déjà une loi de mécanique
"""),

39 : _(u"""
 il n y a pas de loi de couplage
"""),

40 : _(u"""
 il n y a pas de loi hydraulique
"""),

41 : _(u"""
 il n y a pas de loi de mécanique
"""),

42 : _(u"""
 la loi de couplage est incorrecte pour une modélisation %(k1)s
"""),

43 : _(u"""
 incompatibilité des comportements mécanique et hydraulique
"""),

44 : _(u"""
 loi de mécanique incompatible avec une modélisation %(k1)s
"""),

46 : _(u"""
 il y a une loi de mécanique dans la relation %(k1)s
"""),

59 : _(u"""
 la loi de couplage doit être LIQU_SATU ou GAZ pour une modélisation H
"""),

61 : _(u"""
 Il manque le séchage de référence (AFFE_MATERIAU/AFFE_VARC/VALE_REF)
"""),

65 : _(u"""
Arrêt suite à l'échec de l'intégration de la loi de comportement.
   Vérifiez vos paramètres, la cohérence des unités.
   Essayez d'augmenter ITER_INTE_MAXI.
"""),

66 : _(u"""
  convergence atteinte sur approximation linéaire tangente de l'évolution plastique
  risque d'imprécision
"""),

67 : _(u"""
  endommagement maximal atteint au cours des résolutions internes
"""),

77 : _(u"""
 le nombre de composantes dans le champ de vent est incorrect. on doit avoir : DX, DY, DZ
"""),

80 : _(u"""
Pour le comportement %(k3)s, matériau %(k4)s. Incohérence dans les données matériau.
   %(k1)s est >= %(k2)s cela n'est pas possible.
   valeur de %(k1)s : %(r1)E
   valeur de %(k2)s : %(r2)E
"""),

81 : _(u"""
L'association comportement vs matériau est incorrecte.
Les combinaisons possibles sont :
   comportement %(k1)s et matériau %(k2)s et %(k5)s
   comportement %(k3)s et matériau %(k4)s et %(k5)s
"""),


86 : _(u"""
 porosité strictement nulle( cas non traité)
"""),

87 : _(u"""
 l'incrément de temps vaut zéro, vérifiez votre découpage
"""),

88 : _(u"""
 fluence décroissante (flux<0)
"""),

89 : _(u"""
 le paramètre A doit être >=0
"""),

90 : _(u"""
 la loi LMARC_IRRA n'est compatible qu'avec une modélisation poutre
"""),

91 : _(u"""
 stop, RIGI_MECA_TANG non disponible
"""),

92 : _(u"""
 la maille doit être de type TETRA10,PENTA15,HEXA20,QUAD8 ou TRIA6.
 or la maille est de type :  %(k1)s .
"""),

94 : _(u"""
 le champ issu du concept %(k1)s n'est pas calculé à l'instant %(i3)i
"""),

96 : _(u"""
 le séchage ne peut pas être mélangé à un autre comportement
"""),

97 : _(u"""
 EVOL_THER_SECH est un mot-clé obligatoire pour le séchage de type SECH_GRANGER et SECH_NAPPE
"""),

98 : _(u"""
  le concept :  %(k1)s  n'est pas un champ de température
"""),

99 : _(u"""
  le concept EVOL_THER :  %(k1)s  ne contient aucun champ de température
"""),

}
