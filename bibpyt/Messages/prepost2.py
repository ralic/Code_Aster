#@ MODIF prepost2 Messages  DATE 31/10/2011   AUTEUR COURTOIS M.COURTOIS 
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
  Il faut autant de nom pour NOM_CHAM_MED que pour NOM_CHAM.
"""),

2 : _(u"""
Modele inconnu, pas d'impression du champ  %(k1)s
"""),

3 : _(u"""
On ne sait pas écrire des champs par élément aux points de gauss au format CASTEM
"""),

4 : _(u"""
 erreur programmation
"""),

35 : _(u"""
   desole on ne sait pas écrire les champs aux noeuds de representation constante et a valeurs complexes au format  %(k1)s
"""),

36 : _(u"""
   desole on ne sait pas écrire le champ aux noeuds  %(k1)s  au format  %(k2)s
"""),

40 : _(u"""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du cham_gd  %(k1)s  n'est presente dans la grandeur  %(k2)s
"""),

41 : _(u"""
 aucune des composantes demandees sous le mot-cle nom_cmp pour l'impression du champ  %(k1)s  du concept  %(k2)s  n'est presente dans la grandeur  %(k3)s
"""),

46 : _(u"""
  numéro d'ordre  %(k1)s  non licite
"""),

51 : _(u"""
 type de structure non traite:  %(k1)s
"""),

52 : _(u"""
 on imprime que des champs elno
"""),

53 : _(u"""
 nbcmp différent
"""),

54 : _(u"""
 composante inconnue %(k1)s
"""),

55 : _(u"""
 L'ordre des composantes ?tabli lorsque que vous avez renseign? le mot-cl?
 NOM_CMP est diff?rent de celui du catalogue Aster:
    - ordre des composantes fournies     : %(k1)s %(k2)s %(k3)s %(k4)s %(k5)s %(k6)s
    - ordre des composantes du catalogue : %(k7)s %(k8)s %(k9)s %(k10)s %(k11)s %(k12)s
"""),

56 : _(u"""
 on imprime que des champs "elga" ou "elem"
"""),

57 : _(u"""
 nbsp différent de 1
"""),

58 : _(u"""
 pas de correspondance
"""),

59 : _(u"""
 aucun champ trouve, pas d'impression au format "gmsh"
"""),

60 : _(u"""
 On ne sait pas imprimer au format "gmsh" le champ %(k1)s
 car il est de type %(k2)s.
"""),

61 : _(u"""
 erreur de programmation : nbcmp différent de 1 ou 3.
"""),

62 : _(u"""
 on ne peut pas traiter des éléments a plus de 99 noeuds !
"""),

63 : _(u"""
 erreur de programation
"""),

64 : _(u"""
 seg4 élément inexistant dans castem, converti en seg2
"""),

65 : _(u"""
 tria7 élément inexistant dans castem, converti en tria6
"""),

66 : _(u"""
 quad9 élément inexistant dans castem, converti en quad8
"""),

67 : _(u"""
 les champs n'ont pas la même grandeur
"""),

68 : _(u"""
 les champs n'ont pas la même numérotation
"""),

69 : _(u"""
 les champs n'ont pas le même type de valeurs
"""),

70 : _(u"""
   desole on ne sait pas écrire les champs aux noeuds de representation constante au format ideas
"""),

71 : _(u"""
 élément noeud non disponible dans ensight
"""),

72 : _(u"""
 élément  %(k1)s  non disponible dans ensight
"""),

73 : _(u"""
 les éléments tria7 seront reduits a des tria6
"""),

74 : _(u"""
 les éléments quad9 seront reduits a des quad8
"""),

75 : _(u"""
 les éléments penta15 seront reduits a des penta6
"""),

76 : _(u"""
 il y a des groupes de noeuds dans le maillage  %(k1)s  qui n'apparaitront pas dans le fichier geometrie ensight: seuls des groupes de mailles peuvent y être integres
"""),

77 : _(u"""
 la dimension du probleme est invalide : il faut : 1d, 2d, 3d.
"""),

78 : _(u"""
 hexa27 élément inexistant dans ideas, converti en hexa20
"""),

79 : _(u"""
 tria7 élément inexistant dans ideas, converti en tria6
"""),

80 : _(u"""
 quad9 élément inexistant dans ideas, converti en quad8
"""),

81 : _(u"""
 seg4 élément inexistant dans ideas, converti en seg2
"""),

82 : _(u"""
 élément pyram5 non disponible dans ideas
"""),

83 : _(u"""
 élément pyram13 non disponible dans ideas
"""),

84 : _(u"""
 Le champ %(k1)s est un champ aux noeuds par éléments
 contenant des %(k2)s
 Or l'impression de ce type de champ n'est pas encore possible au format MED
 On n'imprimera donc pas ce champ dans le fichier MED
"""),

85 : _(u"""
 L'élément PENTA18 est inexistant dans ideas, il est converti en PENTA15.
"""),

86 : _(u"""
 L'élément PENTA18 est inexistant dans castem, il est converti en PENTA15.
"""),

93 : _(u"""
 on ne sait pas écrire les mailles de type  %(k1)s
"""),

}
